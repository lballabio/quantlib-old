
/*
 * Copyright (C) 2000-2001 QuantLib Group
 *
 * This file is part of QuantLib.
 * QuantLib is a C++ open source library for financial quantitative
 * analysts and developers --- http://quantlib.org/
 *
 * QuantLib is free software and you are allowed to use, copy, modify, merge,
 * publish, distribute, and/or sell copies of it under the conditions stated
 * in the QuantLib License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details.
 *
 * You should have received a copy of the license along with this file;
 * if not, please email quantlib-users@lists.sourceforge.net
 * The license is also available at http://quantlib.org/LICENSE.TXT
 *
 * The members of the QuantLib Group are listed in the Authors.txt file, also
 * available at http://quantlib.org/group.html
*/

/*! \file thirty360.hpp
    \brief 30/360 day counters

    \fullpath
    ql/DayCounters/%thirty360.hpp
*/

// $Id$

#ifndef quantlib_thirty360_day_counter_h
#define quantlib_thirty360_day_counter_h

#include "ql/daycounter.hpp"

namespace QuantLib {

    namespace DayCounters {

        //! 30/360 day count convention
        /*! The day count can be calculated according to US, European, or 
            Italian conventions.
        */
        class Thirty360 : public DayCounter {
          public:
            enum Convention { USA, European, Italian };
          private:
            class Thirty360Factory : public DayCounter::factory {
              public:
                Thirty360Factory(Convention c) : convention_(c) {}
                DayCounter create() const {
                    return Thirty360(convention_);
                }
              private:
                Convention convention_;
            };
            class Thirty360USImpl : public DayCounter::DayCounterImpl {
              public:
                std::string name() const { return std::string("30/360");}
                int dayCount(const Date& d1, const Date& d2) const;
                Time yearFraction(const Date& d1, const Date& d2,
                    const Date&, const Date&) const {
                        return dayCount(d1,d2)/360.0; }
            };
            class Thirty360EuImpl : public DayCounter::DayCounterImpl {
              public:
                std::string name() const { return std::string("30/360eu");}
                int dayCount(const Date& d1, const Date& d2) const;
                Time yearFraction(const Date& d1, const Date& d2,
                    const Date&, const Date&) const {
                        return dayCount(d1,d2)/360.0; }
            };
            class Thirty360ItImpl : public DayCounter::DayCounterImpl {
              public:
                std::string name() const { return std::string("30/360it");}
                int dayCount(const Date& d1, const Date& d2) const;
                Time yearFraction(const Date& d1, const Date& d2,
                    const Date&, const Date&) const {
                        return dayCount(d1,d2)/360.0; }
            };
            static Handle<DayCounterImpl> implementation(Convention c);
          public:
            Thirty360(Convention c = Thirty360::USA)
            : DayCounter(implementation(c)) {}
            //! returns a factory of 30/360 day counters
            Handle<factory> getFactory(Convention c) const {
                return Handle<factory>(new Thirty360Factory(c));
            }
        };

    }

}


#endif
