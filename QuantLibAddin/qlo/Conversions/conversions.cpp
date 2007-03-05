
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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
#include <qlo/Conversions/conversions.hpp>
#include <ql/date.hpp>
#include <ql/period.hpp>
#include <ql/interestrate.hpp>

using std::vector;
using QuantLib::Date;
using QuantLib::Period;
using QuantLib::Size;
using QuantLib::InterestRate;

namespace ObjHandler {

    double libraryToScalar(const InterestRate &i) {
        return i.rate();
    }

    double libraryToScalar(const QuantLib::Rate &r) {
        return r;
    }

    long libraryToScalar(const Date &d) {
        return d.serialNumber();
    }

    std::string libraryToScalar(const Period &period) {
        std::ostringstream s;
        s << period;
        return s.str();
    }

    vector<std::string> libraryToVector(const vector<Period> &v) {
        vector<std::string> ret;
        ret.reserve(v.size());
        for (vector<Period>::const_iterator i = v.begin(); i != v.end(); ++i)
            ret.push_back(libraryToScalar(*i));
        return ret;
    }

    vector<long> libraryToVector(const vector<Date> &v) {
        vector<long> ret;
        ret.reserve(v.size());
        for (vector<Date>::const_iterator i = v.begin(); i != v.end(); ++i)
            ret.push_back(i->serialNumber());
        return ret;
    }

    vector<long> libraryToVector(const vector<Size>& v) {
        return vector<long>(v.begin(), v.end());
    }
}
