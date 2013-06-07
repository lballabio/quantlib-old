/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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


#include <ql/cashflows/fixedinflationcoupon.hpp>
#include <ql/cashflows/inflationcouponpricer.hpp>

namespace QuantLib {

    FixedInflationCoupon::
    FixedInflationCoupon(const Date& paymentDate,
                        Real nominal,
                        const Date& startDate,
                        const Date& endDate,
						const Date& baseDate,
                        Natural fixingDays,
						const Real fixedRate,
                        const boost::shared_ptr<ZeroInflationIndex>& index,
                        const Period& observationLag,
                        const DayCounter& dayCounter,
                        Real gearing,
                        Spread spread,
                        const Date& refPeriodStart,
                        const Date& refPeriodEnd)
    : InflationCoupon(paymentDate, nominal, startDate, endDate,
                  fixingDays, index, observationLag,
                  dayCounter, refPeriodStart, refPeriodEnd),
    zeroInflationIndex_(index), gearing_(gearing), spread_(spread), fixedRate_(fixedRate), baseDate_(baseDate) {}


    void FixedInflationCoupon::accept(AcyclicVisitor& v) {
        Visitor<FixedInflationCoupon>* v1 =
        dynamic_cast<Visitor<FixedInflationCoupon>*>(&v);
        if (v1 != 0)
            v1->visit(*this);
        else
            InflationCoupon::accept(v);
    }


    bool FixedInflationCoupon::checkPricerImpl(
        const boost::shared_ptr<InflationCouponPricer>&pricer) const {
        return boost::dynamic_pointer_cast<FixedInflationCouponPricer>(pricer) != NULL;
    }



   





}

