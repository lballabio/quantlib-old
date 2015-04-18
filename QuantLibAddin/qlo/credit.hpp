/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2010 Roland Lichters
 Copyright (C) 2014 Jose Aparicio

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_credit_hpp
#define qla_credit_hpp

#include <qlo/baseinstruments.hpp>
#include <qlo/yieldtermstructures.hpp>
#include <qlo/piecewiseyieldcurve.hpp>
#include <qlo/defaulttermstructures.hpp>
#include <qlo/schedule.hpp>
#include <qlo/pricingengines.hpp>
#include <qlo/quote.hpp>

#include <ql/handle.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/time/schedule.hpp>
#include <ql/time/businessdayconvention.hpp>
#include <ql/types.hpp>
#include <ql/instruments/creditdefaultswap.hpp>
#include <ql/termstructures/credit/defaultprobabilityhelpers.hpp>
#include <ql/currency.hpp>
#include <ql/experimental/credit/issuer.hpp>
#include <ql/experimental/credit/recoveryratequote.hpp>

namespace QuantLib {
    class Quote;
    class Date;
    class Basket;
}

namespace QuantLibAddin {

	/*!
		\todo Default term structures should be associated to their seniority 
		        and recovery and passed as a vector of TS.
	*/
    class Issuer
        : public ObjectHandler::LibraryObject<QuantLib::Issuer> {
    public:
        Issuer(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::DefaultProbabilityTermStructure>& dfts,
            const boost::shared_ptr<QuantLib::DefaultEventSet>& evtSet,
            bool permanent
            );
    };

    class DefaultEventSet
        : public ObjectHandler::LibraryObject<QuantLib::DefaultEventSet> {
    public:
        DefaultEventSet(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& eventType,
            const QuantLib::Date& eventDate,
            const QuantLib::Currency& cur,
            QuantLib::Seniority sen,
            const QuantLib::Date& settleDate,
            QuantLib::Real settledRecovery,
            bool permanent
        );
    };

    // To do, implement a Seniority inspector in credit.xml
    class RecoveryRateQuote : public Quote {
      public:
        RecoveryRateQuote(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    QuantLib::Seniority sen,
                    QuantLib::Real value,
                    bool permanent);
        QuantLib::Real setValue(QuantLib::Real value);
    private:
        boost::shared_ptr<QuantLib::RecoveryRateQuote> recoveryQuote_;
    };

    //class CreditDefaultSwap : public Instrument {
    //public:
    //    CreditDefaultSwap(
    //          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
    //          QuantLib::Protection::Side side,
    //          QuantLib::Real notional,
    //          QuantLib::Rate upfront,
    //          QuantLib::Rate spread,
    //          const boost::shared_ptr<QuantLib::Schedule>& schedule,
    //          QuantLib::BusinessDayConvention paymentConvention,
    //          const QuantLib::DayCounter& dayCounter,
    //          bool settlesAccrual,
    //          bool paysAtDefaultTime,
    //          const QuantLib::Date& protectionStart,
    //          const QuantLib::Date& upfrontDate,
    //          bool permanent);
    //};

    class MidPointCdsEngine : public PricingEngine {
      public:
        MidPointCdsEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure>&,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            bool permanent);
    };


    class DefaultProbabilityHelper 
        : public ObjectHandler::LibraryObject<QuantLib::DefaultProbabilityHelper> {
      protected:
        OH_LIB_CTOR(DefaultProbabilityHelper, QuantLib::DefaultProbabilityHelper);
    };

    class SpreadCdsHelper : public DefaultProbabilityHelper {
      public:
        SpreadCdsHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& period,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Frequency frequency,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::DateGeneration::Rule rule,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTS,
            bool settlesAccrual,
            bool paysAtDefaultTime,
            bool permanent);
    };

    class UpfrontCdsHelper : public DefaultProbabilityHelper {
      public:
        UpfrontCdsHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            QuantLib::Rate runningSpread,
            const QuantLib::Period& period,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Frequency frequency,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::DateGeneration::Rule rule,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTS,
            QuantLib::Natural upfrontSettlementDays,
            bool settlesAccrual,
            bool paysAtDefaultTime,
            bool permanent);
    };
    
    class HazardRateCurve 
        : public DefaultProbabilityTermStructure {
      public:
        HazardRateCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Rate>& hazardRates,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
        const std::vector<QuantLib::Time>& times() const;
        const std::vector<QuantLib::Date>& dates() const;
        const std::vector<QuantLib::Real>& data() const;
    };

    // Bootstrapped piecewise flat hazard rate curve 
    // traits = hazard rates
    // To do: add a Registry Manager factory once the number of options in 
    //   the combination of interpolation traits and algorithm grows.
    class PiecewiseHazardRateCurve : public DefaultProbabilityTermStructure {
      public:
        PiecewiseHazardRateCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<boost::shared_ptr<QuantLib::DefaultProbabilityHelper> >& helpers,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Calendar& calendar,
            const std::string& interpolator,
            QuantLib::Real accuracy,
            bool permanent);

        const std::vector<QuantLib::Date>& dates() const;
        const std::vector<QuantLib::Real>& data() const;
        /*
        const std::vector<QuantLib::Time>& times() const;
        const std::vector<QuantLib::Date>& dates() const;
        const std::vector<QuantLib::Real>& data() const;
        
        const std::vector<QuantLib::Real>& improvements(
            InterpolatedYieldCurve::Traits traits,
            InterpolatedYieldCurve::Interpolator interpolator) const;

        QuantLib::Size iterations(
            InterpolatedYieldCurve::Traits traits,
            InterpolatedYieldCurve::Interpolator interpolator) const;

        const std::vector<QuantLib::Time>& jumpTimes(
             InterpolatedYieldCurve::Traits traits,
             InterpolatedYieldCurve::Interpolator interpolator) const;

        const std::vector<QuantLib::Date>& jumpDates(
             InterpolatedYieldCurve::Traits traits,
             InterpolatedYieldCurve::Interpolator interpolator) const;
        */
    };

    // no jump dates and jumps, traits = Discount, interpolator = LogLinear
    class PiecewiseFlatForwardCurve : public YieldTermStructure {
      public:
        PiecewiseFlatForwardCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& referenceDate,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& qlrhs,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy,
            bool permanent);
        /*
        const std::vector<QuantLib::Time>& times(
             InterpolatedYieldCurve::Traits traits,
             InterpolatedYieldCurve::Interpolator interpolator) const;

        const std::vector<QuantLib::Date>& dates(
             InterpolatedYieldCurve::Traits traits,
             InterpolatedYieldCurve::Interpolator interpolator) const;

        const std::vector<QuantLib::Real>& data(
             InterpolatedYieldCurve::Traits traits,
             InterpolatedYieldCurve::Interpolator interpolator) const;

        const std::vector<QuantLib::Real>& improvements(
            InterpolatedYieldCurve::Traits traits,
            InterpolatedYieldCurve::Interpolator interpolator) const;

        QuantLib::Size iterations(
            InterpolatedYieldCurve::Traits traits,
            InterpolatedYieldCurve::Interpolator interpolator) const;
        */
    };

    class RiskyFixedBond : public Instrument {
    public:
        RiskyFixedBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            std::string name,
            QuantLib::Currency ccy,
            QuantLib::Real recoveryRate,
            QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure> 
                defaultTS,
            const boost::shared_ptr<QuantLib::Schedule>&  schedule,
            QuantLib::Real rate,
            QuantLib::DayCounter dayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real notionals,
            QuantLib::Handle<QuantLib::YieldTermStructure> yieldTS,
            QuantLib::Date npvDate,
            bool permanent);
    };



    class SyntheticCDO : public Instrument {
    public:
        SyntheticCDO(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const boost::shared_ptr<QuantLib::Basket>& bskt,
              QuantLib::Protection::Side side,
              const boost::shared_ptr<QuantLib::Schedule>& schedule,
              QuantLib::Rate upfront,
              QuantLib::Rate spread,
              const QuantLib::DayCounter& dayCounter,
              QuantLib::BusinessDayConvention paymentConvention,
              bool permanent);
    };


    class MidPointCDOEngine : public PricingEngine {
      public:
        MidPointCDOEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            bool permanent);
    };

    class NthToDefault : public Instrument {
    public:
        NthToDefault(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const boost::shared_ptr<QuantLib::Basket>& bskt,
              QuantLib::Size order,
              QuantLib::Protection::Side side,
              const boost::shared_ptr<QuantLib::Schedule>& schedule,
              QuantLib::Rate upfront,
              QuantLib::Rate spread,
              const QuantLib::DayCounter& dayCounter,
              QuantLib::Real notional,
              bool paysAccrual,
              bool permanent);
    };


    class IntegralNtdEngine : public PricingEngine {
      public:
        IntegralNtdEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Period& step,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            bool permanent);
    };



}

#endif
