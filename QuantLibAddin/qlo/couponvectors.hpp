
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_couponvectors_hpp
#define qla_couponvectors_hpp

#include <qlo/leg.hpp>

#include <ql/time/businessdayconvention.hpp>
#include <ql/cashflows/replication.hpp>

namespace QuantLib {
    class Schedule;
    class IborIndex;
    class DayCounter;
    class SwapIndex;
    class CapletVolatilityStructure;
}

namespace QuantLibAddin {
    
    class FixedRateLeg : public Leg {
      public:
        FixedRateLeg(
            QuantLib::BusinessDayConvention              paymentConvention,
            const std::vector<QuantLib::Real>&           nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>&           couponRates,
            const QuantLib::DayCounter&                  paymentDayCounter);
    };

    class IborLeg : public Leg {
      public:
        IborLeg(
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps);
    };

    class DigitalIborLeg : public Leg {
      public:
        DigitalIborLeg(
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& callStrikes,
            std::string callPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& callDigitalPayoffs,
            const std::vector<QuantLib::Rate>& putStrikes,
            std::string putPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& putDigitalPayoffs,
            QuantLib::Replication::Type replicationType,
            QuantLib::Real eps);
    };

    class CmsLeg : public Leg {
      public:
        CmsLeg(
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps);
    };

    class DigitalCmsLeg : public Leg {
      public:
        DigitalCmsLeg(
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& callStrikes,
            std::string callPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& callDigitalPayoffs,
            const std::vector<QuantLib::Rate>& putStrikes,
            std::string putPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& putDigitalPayoffs,
            QuantLib::Replication::Type replicationType,
            QuantLib::Real eps);
    };

    class RangeAccrualLeg : public Leg {
      public:
        RangeAccrualLeg(
           QuantLib::BusinessDayConvention paymentConvention,
           const std::vector<QuantLib::Real>& nominals,
           const boost::shared_ptr<QuantLib::Schedule>& schedule,
           const std::vector<QuantLib::Natural>& fixingDays,
           const QuantLib::DayCounter& paymentDayCounter,
           const std::vector<QuantLib::Rate>& lowerTriggers,
           const std::vector<QuantLib::Real>& gearings,
           const boost::shared_ptr<QuantLib::IborIndex>& index,
           const std::vector<QuantLib::Spread>& spreads,
           const std::vector<QuantLib::Rate>& upperTriggers,
           const QuantLib::Period& observationTenor,
           QuantLib::BusinessDayConvention observationConvention);
    };

    class CmsZeroLeg : public Leg {
      public:
        CmsZeroLeg(
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps);
    };

    class FloatingRateCouponPricer : 
        public ObjectHandler::LibraryObject<QuantLib::FloatingRateCouponPricer> {
      public:
          FloatingRateCouponPricer(){};
    };
    
    class IborCouponPricer : public FloatingRateCouponPricer {
      public:
        IborCouponPricer(
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& vol,
            const std::string& typeOfIborCouponPricer);
    };
}

#endif
