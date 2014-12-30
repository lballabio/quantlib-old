/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2009 Chris Kenyon

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

#include <ql/termstructures/inflationtermstructure.hpp>
#include <ql/indexes/inflationindex.hpp>

namespace QuantLib {

    std::pair<Date,Date> inflationPeriod(const Date& d,
                                         Frequency frequency) {

        Month month = d.month();
        Year year = d.year();

        Month startMonth, endMonth;
        switch (frequency) {
          case Annual:
            startMonth = January;
            endMonth = December;
            break;
          case Semiannual:
            startMonth = Month(6*((month-1)/6) + 1);
            endMonth = Month(startMonth + 5);
            break;
          case Quarterly:
            startMonth = Month(3*((month-1)/3) + 1);
            endMonth = Month(startMonth + 2);
            break;
          case Monthly:
            startMonth = endMonth = month;
            break;
          default:
            QL_FAIL("Frequency not handled: " << frequency);
            break;
        };

        Date startDate = Date(1, startMonth, year);
        Date endDate = Date::endOfMonth(Date(1, endMonth, year));

        return std::make_pair(startDate,endDate);
    }


    Time inflationYearFraction(Frequency f, bool indexIsInterpolated,
                               const DayCounter &dayCounter,
                               const Date &d1, const Date &d2) {

        Time t=0;
        if (indexIsInterpolated) {
            // N.B. we do not use linear interpolation between flat
            // fixing forecasts for forecasts.  This avoids awkwardnesses
            // when bootstrapping the inflation curve.
            t = dayCounter.yearFraction(d1, d2);
        } else {
            // I.e. fixing is constant for the whole inflation period.
            // Use the value for half way along the period.
            // But the inflation time is the time between period starts
            std::pair<Date,Date> limD1 = inflationPeriod(d1, f);
            std::pair<Date,Date> limD2 = inflationPeriod(d2, f);
            t = dayCounter.yearFraction(limD1.first, limD2.first);
        }

        return t;
    }


}
