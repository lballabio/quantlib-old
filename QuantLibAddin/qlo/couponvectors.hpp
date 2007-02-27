
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006, 2007 Cristina Duminuco
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

#ifndef qla_couponvectors_hpp
#define qla_couponvectors_hpp

#include <oh/objhandler.hpp>

#include <qlo/schedule.hpp>
#include <qlo/index.hpp>
#include <qlo/analysis.hpp>

#include <ql/CashFlows/conundrumpricer.hpp>
#include <ql/Volatilities/all.hpp>

namespace QuantLibAddin {
    
    class Leg : public ObjHandler::Object {
      public:
        QuantLib::Date startDate() const;
        QuantLib::Date maturityDate() const;
        QuantLib::Real npv(const QuantLib::Handle<QuantLib::YieldTermStructure>&) const;
        QuantLib::Real bps(const QuantLib::Handle<QuantLib::YieldTermStructure>&) const;
        QuantLib::Rate atmRate(const QuantLib::Handle<QuantLib::YieldTermStructure>&) const;
        void setPricer(const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer);
        void setPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers);

        std::vector<std::vector<boost::any> > analysis() const;
        const QuantLib::Leg& getQuantLibLeg();        
      protected:
        // copy or shared_ptr?
        QuantLib::Leg leg_;
    };

    class MultiPhaseLeg : public Leg {
      public:
        MultiPhaseLeg(const std::vector<boost::shared_ptr<Leg> >& legs,
                      bool toBeSorted);
    };

    class SimpleCashFlowVector : public Leg {
      public:
        SimpleCashFlowVector(const std::vector<double>& amounts, 
                             const std::vector<QuantLib::Date>& dates);
    };
    
    class FixedRateLeg : public Leg {
      public:
        FixedRateLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention              convention,
            const std::vector<double>&                   nominals,
            const std::vector<double>&                   couponRates,
            const QuantLib::DayCounter&                  dayCountID);
    };

    class FloatingRateCouponPricer : 
        public ObjHandler::LibraryObject<QuantLib::FloatingRateCouponPricer> {
      public:
          FloatingRateCouponPricer(){};
    };
    
    class IborLeg : public Leg {
      public:
        // no embedded options
        IborLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentAdjustment,
            const std::vector<double>& nominals,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Integer fixingDays,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Spread>& spreads);
        // with embedded options
        IborLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentAdjustment,
            const std::vector<double>& nominals,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Integer fixingDays,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps);
    };

    class IborCouponPricer : 
        public FloatingRateCouponPricer {
      public:
        IborCouponPricer(
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& vol,
            const std::string& typeOfIborCouponPricer);
    };

    class CmsLeg : public Leg {
      public:
        CmsLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentAdjustment,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            QuantLib::Integer fixingDays,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Real>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Real>& spreads,
            const std::vector<QuantLib::Real>& caps);
    };

    class CmsZeroLeg : public Leg {
      public:
        CmsZeroLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentAdjustment,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            QuantLib::Integer fixingDays,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Real>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Real>& spreads,
            const std::vector<QuantLib::Real>& caps);
    };

    class CmsInArrearsLeg : public Leg {
      public:
        CmsInArrearsLeg(
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentAdjustment,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            QuantLib::Integer fixingDays,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Real>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Real>& spreads,
            const std::vector<QuantLib::Real>& caps);
    };

    class CmsCouponPricer : 
        public FloatingRateCouponPricer {
      public:
        CmsCouponPricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& vol,
            const std::string& typeOfCmsCouponPricer,
            QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);
    };

}

#endif
