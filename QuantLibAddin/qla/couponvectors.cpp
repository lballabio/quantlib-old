
/*
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
    #include <qla/config.hpp>
#endif

#include <qla/couponvectors.hpp>
#include <qla/schedule.hpp>
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>
#include <qla/xibor.hpp>

#include <ql/CashFlows/basispointsensitivity.hpp>
#include <ql/CashFlows/fixedratecoupon.hpp>
#include <ql/CashFlows/parcoupon.hpp>

namespace QuantLibAddin {

    double CouponVector::getBPS(const std::string &termStructureID) const {

        OH_GET_REFERENCE(termStructure, termStructureID, 
            YieldTermStructure, QuantLib::YieldTermStructure)
        QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;
        discountingTermStructure.linkTo(termStructure);
        
        return QuantLib::BasisPointSensitivity(cashFlowVector_, discountingTermStructure);
    }

    FixedRateCouponVector::FixedRateCouponVector(
            const std::string         &scheduleID,
            const std::string         &conventionID,
            const std::vector<double> &nominals,
            const std::vector<double> &couponRates,
            const std::string         &dayCountID) {

        OH_GET_OBJECT(scheduleWrapper, scheduleID, Schedule)
        const QuantLib::Schedule& schedule = scheduleWrapper->getObject();

        QuantLib::BusinessDayConvention convention =
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        QuantLib::DayCounter dayCount =
            Create<QuantLib::DayCounter>()(dayCountID);
        
        cashFlowVector_ =
            QuantLib::FixedRateCouponVector(schedule,
                                            convention,
                                            nominals,
                                            couponRates,
                                            dayCount);
    }

    std::vector<std::vector<double> > FixedRateCouponVector::getLeg() {
        std::vector<std::vector<double> > leg;

        for (std::size_t i=0 ; i < cashFlowVector_.size() ; i++) {
            std::vector<double> cf;
            QuantLib::FixedRateCoupon& c =
                (QuantLib::FixedRateCoupon&) *(cashFlowVector_[i]);
            cf.push_back(c.accrualStartDate().serialNumber());
            cf.push_back(c.accrualEndDate().serialNumber());
            cf.push_back(c.date().serialNumber());
            cf.push_back(c.accrualPeriod());
            cf.push_back(c.accrualDays());
            cf.push_back(c.amount());
            leg.push_back(cf);
        }

        return leg;
    }

    FloatingRateCouponVector::FloatingRateCouponVector(
            const std::string         &scheduleID,
            const std::vector<double> &nominals,
            const std::string         &indexID,
            const std::vector<double> &spreads) {

        OH_GET_OBJECT(scheduleWrapper, scheduleID, Schedule)
        const QuantLib::Schedule& schedule = scheduleWrapper->getObject();

        OH_GET_REFERENCE(index, indexID, 
            Xibor, QuantLib::Xibor)

        cashFlowVector_ =
            QuantLib::FloatingRateCouponVector(schedule,
                                               index->businessDayConvention(),
                                               nominals,
                                               index,
                                               index->settlementDays(),
                                               spreads,
                                               index->dayCounter());
    }

    std::vector<std::vector<double> > FloatingRateCouponVector::getLeg() {
        std::vector<std::vector<double> > leg;

        for (std::size_t i=0 ; i < cashFlowVector_.size() ; i++) {
            std::vector<double> cf;
            QuantLib::ParCoupon& c =
                (QuantLib::ParCoupon&) *(cashFlowVector_[i]);
            cf.push_back(c.accrualStartDate().serialNumber());
            cf.push_back(c.accrualEndDate().serialNumber());
            cf.push_back(c.date().serialNumber());
            cf.push_back(c.fixingDate().serialNumber());
            cf.push_back(c.accrualPeriod());
            cf.push_back(c.accrualDays());
            cf.push_back(c.amount());
            cf.push_back(c.indexFixing());
            leg.push_back(cf);
        }

        return leg;
    }

}

