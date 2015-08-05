/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Bitquant Research Laboratories (Asia) Ltd.

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

#include <ql/time/daycounters/continuoustime.hpp>
#include <ql/time/date.hpp>

namespace QuantLib {
    ContinuousTime::Impl::Impl(TimeUnit units) :
        units_(units) {};


    Time ContinuousTime::Impl::yearFraction(const Date& d1,
                                            const Date& d2,
                                            const Date&,
                                            const Date&) const {
        Time divisor;
        switch (units_) {
        case Years:
            divisor = 365.25;
            break;
        case Months:
            divisor = 30.0;
            break;
        case Weeks:
            divisor = 7.0;
            break;
        case Days:
            divisor = 1.0;
            break;
        default:
            QL_FAIL("unknown time unit (" << units_ << ")");
        }
        return (d2 - d1) / divisor;
    }
}
