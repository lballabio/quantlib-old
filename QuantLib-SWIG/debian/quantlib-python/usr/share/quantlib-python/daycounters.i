
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005 StatPro Italia srl
 Copyright (C) 2005 Johan Witters

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

#ifndef quantlib_day_counters_i
#define quantlib_day_counters_i

%include common.i
%include date.i
%include types.i
%include stl.i
%include null.i

%{
using QuantLib::DayCounter;
%}

class DayCounter {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-count")     dayCount;
    %rename("year-fraction") yearFraction;
    #endif
  protected:
    DayCounter();
  public:
    BigInteger dayCount(const Date& d1, const Date& d2) const;
    Time yearFraction(const Date& d1, const Date& d2,
                      const Date& startRef = Date(),
                      const Date& endRef = Date()) const;
    std::string name() const;
    %extend {
        #if !defined(SWIGPERL)
        std::string __str__() {
            return self->name()+" day counter";
        }
        #endif
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGJAVA)
        bool __eq__(const DayCounter& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON) || defined(SWIGJAVA)
        bool __ne__(const DayCounter& other) {
            return (*self) != other;
        }
        #endif
        #endif
    }
};

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("DayCounter=?") DayCounter_equal;
%inline %{
    bool DayCounter_equal(const DayCounter& d1, const DayCounter& d2) {
        return d1 == d2;
    }
%}
#endif

namespace QuantLib {

    class Actual360 : public DayCounter {};
    class Actual365Fixed : public DayCounter {};
    class Actual365NoLeap : public DayCounter {};
    class Thirty360 : public DayCounter {
      public:
        enum Convention { USA, BondBasis, European, EurobondBasis, Italian };
        Thirty360(Convention c = USA);
    };
    class ActualActual : public DayCounter {
      public:
        enum Convention { ISMA, Bond, ISDA, Historical, Actual365, AFB, Euro };
        ActualActual(Convention c = ISDA);
    };
    class OneDayCounter : public DayCounter {};
    class SimpleDayCounter : public DayCounter {};
    class Business252 : public DayCounter {};
}


#endif
