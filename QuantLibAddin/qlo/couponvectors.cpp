
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/couponvectors.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/typefactory.hpp>

#include <ql/cashflow.hpp>
#include <ql/CashFlows/cashflowvectors.hpp>
#include <ql/CashFlows/simplecashflow.hpp>
#include <ql/CashFlows/cmscoupon.hpp>
#include <ql/CashFlows/analysis.hpp>

using QuantLib::earlier_than;
using QuantLib::CashFlow;

namespace QuantLibAddin {

    QuantLib::Date Leg::startDate() const {
        return QuantLib::CashFlows::startDate(leg_);
    }

    QuantLib::Date Leg::maturityDate() const {
        return QuantLib::CashFlows::maturityDate(leg_);
    }

    QuantLib::Real Leg::npv(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::bps(leg_, hYTS);
    }

    QuantLib::Real Leg::bps(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::bps(leg_, hYTS);
    }

    QuantLib::Rate Leg::atmRate(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::atmRate(leg_, hYTS);
    }

    std::vector<std::vector<boost::any> > Leg::analysis() const {
        return flowAnalysis(leg_);
    }

    const QuantLib::Leg& Leg::getQuantLibLeg() {
        return leg_;
    }

    MultiPhaseLeg::MultiPhaseLeg(
                    const std::vector<boost::shared_ptr<Leg> >& streams,
                    bool toBeSorted) {
        for (QuantLib::Size i=0; i<streams.size(); ++i) {
            const QuantLib::Leg& leg = streams[i]->getQuantLibLeg();
            leg_.insert(leg_.end(), leg.begin(), leg.end());
        }
        if (toBeSorted)
            std::sort(leg_.begin(), leg_.end(),
                      earlier_than<boost::shared_ptr<CashFlow> >());
    };

    SimpleCashFlowVector::SimpleCashFlowVector(const std::vector<double>& amounts,
                                               const std::vector<QuantLib::Date>& dates)
    {
        for (QuantLib::Size i=0; i < amounts.size(); ++i) {
            leg_.push_back(boost::shared_ptr<CashFlow>(
                new QuantLib::SimpleCashFlow(amounts[i],dates[i])));
        }
    }

    FixedRateLeg::FixedRateLeg(
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::BusinessDayConvention convention,
                    const std::vector<double>& nominals,
                    const std::vector<double>& couponRates,
                    const QuantLib::DayCounter& dayCounter) {
        leg_ = QuantLib::FixedRateLeg(*schedule,
                                      nominals,
                                      couponRates,
                                      dayCounter,
                                      convention,
                                      dayCounter);
    }

    FloatingRateLeg::FloatingRateLeg(
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<double>& nominals,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    QuantLib::Integer fixingDays,
                    const QuantLib::DayCounter& dayCounter,
                    QuantLib::BusinessDayConvention paymentAdjustment) {

        leg_ = QuantLib::FloatingRateLeg(*schedule,
                                         nominals,
                                         index,
                                         dayCounter,
                                         fixingDays,
                                         paymentAdjustment,
                                         gearings, spreads);
    }

    CappedFlooredFloatingRateLeg::CappedFlooredFloatingRateLeg(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        const std::vector<QuantLib::Real>& nominals,
        const std::vector<QuantLib::Real>& gearings,
        const boost::shared_ptr<QuantLib::IborIndex>& index,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Real>& caps,
        const std::vector<QuantLib::Real>& floors,  
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& dayCounter,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& volatility) {

            leg_ = QuantLib::CappedFlooredFloatingRateLeg(*schedule,
                                                          nominals,
                                                          index,
                                                          dayCounter,
                                                          fixingDays,
                                                          paymentAdjustment,
                                                          gearings, spreads,
                                                          caps, floors,
                                                          volatility);
    }

    VanillaCMSCouponPricer::VanillaCMSCouponPricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& v,
            const std::string& typeOfVanillaCMSCouponPricer,
            QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            QuantLib::Real meanReversion) {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::VanillaCMSCouponPricer> >()
            (typeOfVanillaCMSCouponPricer, v, modelOfYieldCurve, meanReversion);
    }

    CMSLeg::CMSLeg(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        leg_ = QuantLib::CMSLeg(*schedule,
                                nominals,
                                index,
                                pricer,
                                paymentDayCounter,
                                fixingDays,
                                paymentAdjustment,
                                gearings, spreads,
                                caps, floors);
    }

    CMSZeroLeg::CMSZeroLeg(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        leg_ = QuantLib::CMSZeroLeg(*schedule,
                                    nominals,
                                    index,
                                    pricer,
                                    paymentDayCounter,
                                    fixingDays,
                                    paymentAdjustment,
                                    gearings, spreads,
                                    caps, floors);
    }

    CMSInArrearsLeg::CMSInArrearsLeg(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        leg_ = QuantLib::CMSInArrearsLeg(*schedule,
                                         nominals,
                                         index,
                                         pricer,
                                         paymentDayCounter,
                                         fixingDays,
                                         paymentAdjustment,
                                         gearings, spreads,
                                         caps, floors);
    }

}
