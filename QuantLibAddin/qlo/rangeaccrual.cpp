
/*
 Copyright (C) 2006 Giorgio Facchinetti

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

#include <qlo/rangeaccrual.hpp>
#include <ql/CashFlows/rangeaccrual.hpp>


namespace QuantLibAddin {
    
   RangeAccrualFloatersCoupon::RangeAccrualFloatersCoupon(
                const QuantLib::Real nominal,
                const QuantLib::Date& paymentDate,
                const boost::shared_ptr<QuantLib::InterestRateIndex>& index,
                const QuantLib::Date& startDate,                                  
                const QuantLib::Date& endDate,                                   
                QuantLib::Integer fixingDays,
                const QuantLib::DayCounter& dayCounter,
                QuantLib::Real gearing,
                QuantLib::Rate spread,
                const QuantLib::Date& refPeriodStart,
                const QuantLib::Date& refPeriodEnd,    
                const boost::shared_ptr<QuantLib::Schedule>&  observationsSchedule,
                const QuantLib::Real lowerTrigger,                                    
                const QuantLib::Real upperTrigger) {
    
      libraryObject_ = boost::shared_ptr<QuantLib::RangeAccrualFloatersCoupon>(
            new QuantLib::RangeAccrualFloatersCoupon(
                nominal,
                paymentDate,
                index,
                startDate,                                  
                endDate,                                   
                fixingDays, 
                dayCounter,
                gearing,
                spread,
                refPeriodStart,
                refPeriodEnd,    
                observationsSchedule,
                lowerTrigger,                                    
                upperTrigger));   
    }

    RangeAccrualPricerByBgm::RangeAccrualPricerByBgm(
            const QuantLib::Real correlation,
            const  boost::shared_ptr<QuantLib::SmileSection>& smilesOnExpiry,
            const  boost::shared_ptr<QuantLib::SmileSection>& smilesOnPayment,
            bool isClosedFormula,
            bool byCallSpread) {
    
      libraryObject_ = boost::shared_ptr<QuantLib::RangeAccrualPricerByBgm>(
            new QuantLib::RangeAccrualPricerByBgm(
                correlation,
                smilesOnExpiry, 
                smilesOnPayment,
                isClosedFormula,
                byCallSpread)); 
    }

}
