
/*
 Copyright (C) 2006 Giorgio Facchinetti

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_rangeaccrual_hpp
#define qla_rangeaccrual_hpp

#include <qlo/couponvectors.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class RangeAccrualFloatersCoupon;
    class IborIndex;
    class SmileSection;
}

namespace QuantLibAddin {
    class RangeAccrualFloatersCoupon: public ObjectHandler::LibraryObject<QuantLib::RangeAccrualFloatersCoupon>{
	 public:
        RangeAccrualFloatersCoupon(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                QuantLib::Real nominal,
                const QuantLib::Date& paymentDate,
                const boost::shared_ptr<QuantLib::IborIndex>& index,
                const QuantLib::Date& startDate,                                  
                const QuantLib::Date& endDate,                                   
                QuantLib::Integer fixingDays,
                const QuantLib::DayCounter& dayCounter,
                QuantLib::Real gearing,
                QuantLib::Rate spread,
                const QuantLib::Date& refPeriodStart,
                const QuantLib::Date& refPeriodEnd,    
                const boost::shared_ptr<QuantLib::Schedule>&  observationsSchedule,
                QuantLib::Real lowerTrigger,                                    
                QuantLib::Real upperTrigger,
                bool permanent);
        RangeAccrualFloatersCoupon(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                const boost::shared_ptr<Leg>& rangeAccrualLeg,
                QuantLib::Size i,
                bool permanent);
    };  

    OH_OBJ_CLASS(RangeAccrualPricer, FloatingRateCouponPricer);

    class RangeAccrualPricerByBgm: public RangeAccrualPricer{
	 public:
        RangeAccrualPricerByBgm(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Real correlation,
            const boost::shared_ptr<QuantLib::SmileSection>& smilesOnExpiry,
            const boost::shared_ptr<QuantLib::SmileSection>& smilesOnPayment,
            bool isClosedFormula,
            bool byCallSpread,
            bool permanent);
    };    

}

#endif

