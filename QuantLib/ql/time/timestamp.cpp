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

#include <ql/time/timestamp.hpp>
#include <ql/errors.hpp>
#include <ctime>

#if defined(BOOST_NO_STDC_NAMESPACE)
    namespace std { using ::time; using ::time_t; using ::tm;
                    using ::gmtime; using ::localtime; }
#endif

namespace QuantLib {

    // constructors
    TimeStamp::TimeStamp()
        : Date(),
          serialTimeStamp_(BigInteger(0)) {}
    TimeStamp::TimeStamp(Day d,
                         Month m,
                         Year y,
                         Hour h,
                         Minute mt,
                         Second s,
                         Millisecond ms)
        : Date(d, m, y) {
        setSerial(h, mt, s, ms);
    }

    TimeStamp::TimeStamp(Date &date,
                         Hour h,
                         Minute mt,
                         Second s,
                         Millisecond ms)
        : Date(date) {
        setSerial(h, mt, s, ms);
    }

    void TimeStamp::setSerial(Hour h, Minute mt, Second s, 
                               Millisecond ms) {
        QL_REQUIRE(h >= 0 && h <= 23,
                   "hour " << h << " out of bound. It must be in [0, 59]");
        QL_REQUIRE(mt >= 0 && mt <= 59,
                   "minute " << mt << " out of bound. It must be in [0, 59]");
        QL_REQUIRE(s >= 0 && s <= 60,
                   "second " << s << " out of bound. It must be in [0, 60]");
        QL_REQUIRE(ms >= 0 && ms <= 59,
                   "millisecond " << ms << " out of bound. It must be in [0, 59]");
        serialTimeStamp_ = ((((h * 60 + mt) *60) + s) * 1000) + ms;
    }

    TimeStamp TimeStamp::now() {
        std::time_t t;

        if (std::time(&t) == std::time_t(-1)) // -1 means time() didn't work
            return TimeStamp();
        std::tm *lt = std::gmtime(&t);
        return TimeStamp(Day(lt->tm_mday),
                         Month(lt->tm_mon+1),
                         Year(lt->tm_year+1900),
                         Hour(lt->tm_hour),
                         Minute(lt->tm_min),
                         Second(lt->tm_sec));
    }

    BigInteger TimeStamp::serialTimeStamp() const {
        return serialTimeStamp_;
    }

    Time TimeStamp::dayFraction() const {
        return (float) serialTimeStamp_ / 86400.0 / 1000.0;
    }

    Hour TimeStamp::hour() const {
        Integer h = serialTimeStamp_ / 1000 / 60 / 60;
        return Hour(h);
    }

    Minute TimeStamp::minute() const {
        Integer m = (serialTimeStamp_ % (60 * 60 * 1000)) / 1000 / 60;  
        return Minute(m);
    }

    Second TimeStamp::second() const {
        Integer s = (serialTimeStamp_ % 60 * 1000) / 1000;
        return Second(s);
    }

    Millisecond TimeStamp::millisecond() const {
        return serialTimeStamp_ % 1000;
    }
}
