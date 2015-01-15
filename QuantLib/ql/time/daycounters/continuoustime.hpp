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

/*! \file continoustime.hpp
     \brief Day counter for continuous time measurements
*/

#ifndef quantlib_continuous_time_hpp
#define quantlib_continuous_time_hpp

#include <ql/time/daycounter.hpp>
#include <ql/time/timeunit.hpp>

namespace QuantLib {

    //! Continous day counter 

    class ContinuousTime : public DayCounter {
      private:
        class Impl : public DayCounter::Impl {
          public:
            Impl(TimeUnit units);
            std::string name() const { return "ContinuousTime"; }
            Time yearFraction(const Date& d1,
                              const Date& d2,
                              const Date&,
                              const Date&) const;
            TimeUnit units_;
        };
      public:
        ContinuousTime(TimeUnit units)
        : DayCounter(boost::shared_ptr<ContinuousTime::Impl>(
                     new ContinuousTime::Impl(units))) {}
        static ContinuousTime perYear() {
            return ContinuousTime(Years);
        }
        static ContinuousTime perMonth() {
            return ContinuousTime(Months);
        }
        static ContinuousTime perWeek() {
            return ContinuousTime(Weeks);
        }
        static ContinuousTime perDay() {
            return ContinuousTime(Days);
        }
    };

}

#endif
