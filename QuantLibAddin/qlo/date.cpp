
/*
 Copyright (C) 2006 Ferdinando Ametrano

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


#include <qlo/date.hpp>
#include <ql/settings.hpp>

using QuantLib::Date;
using QuantLib::Days;
using std::vector;
using std::string;

namespace QuantLibAddin {

    vector<Date> qlNextIMMdates(const Date& date,
                                const vector<bool>& mainCycle) {
        QL_REQUIRE(!mainCycle.empty(), "Main cycle flags vector is empty.");
        Date d = (date == Date() ?
                  Date(QuantLib::Settings::instance().evaluationDate()) :
                  date);
        vector<Date> out(1, Date::nextIMMdate(d, mainCycle[0]));

        QuantLib::Size n = mainCycle.size();
        if (n==0) return out;
        out.reserve(n);
        for (QuantLib::Size i=1; i<n; ++i)
            out.push_back(Date::nextIMMdate(out[i-1]+1*Days, mainCycle[i]));
        return out;
    }

    vector<string> qlNextIMMcodes(const Date& date,
                                  const vector<bool>& mainCycle) {
        vector<Date> immDates = qlNextIMMdates(date, mainCycle);
        vector<string> out;
        QuantLib::Size n = mainCycle.size();
        if (n==0) return out;
        out.reserve(n);
        for (QuantLib::Size i=0; i<n; ++i)
            out.push_back(Date::IMMcode(immDates[i]));
        return out;
    }

}
