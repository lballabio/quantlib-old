
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_day_counters_i
#define quantlib_day_counters_i

%include common.i
%include date.i
%include types.i
%include stl.i
%include null.i

%{
using QuantLib::DayCounter;
using QuantLib::Actual360;
using QuantLib::Actual365;
using QuantLib::Thirty360;
using QuantLib::ActualActual;
using QuantLib::SimpleDayCounter;
%}

class DayCounter {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-count")     dayCount;
    %rename("year-fraction") yearFraction;
    %rename(">string")       __str__;
    #endif
  private:
    DayCounter();
  public:
    int dayCount(const Date& d1, const Date& d2);
    Time yearFraction(const Date& d1, const Date& d2,
                      const Date& startRef = Date(), 
                      const Date& endRef = Date());
    %extend {
        DayCounter(std::string s) {
            s = StringFormatter::toLowercase(s);
            if (s == "act365" || s == "act/365")
                return new DayCounter(Actual365());
            else if (s == "act360" || s == "act/360")
                return new DayCounter(Actual360());
            else if (s == "30/360" || s == "30/360us")
                return new DayCounter(Thirty360(Thirty360::USA));
            else if (s == "30e/360" || s == "30/360e" || s == "30/360eu")
                return new DayCounter(Thirty360(Thirty360::European));
            else if (s == "30/360i" || s == "30/360it")
                return new DayCounter(Thirty360(Thirty360::Italian));
            else if (s == "actact" || s == "act/act" || 
                     s == "act/act(b)" || s == "act/act (bond)")
                return new DayCounter(ActualActual(ActualActual::Bond));
            else if (s == "actacte" || s == "act/act(e)" 
                     || s == "act/act(Euro)")
                return new DayCounter(ActualActual(ActualActual::Euro));
            else if (s == "actacth" || s == "act/act(h)" 
                     || s == "act/act (ISDA)")
                return new DayCounter(ActualActual(ActualActual::Historical));
            else if (s == "simple")
                return new DayCounter(SimpleDayCounter());
            else
                QL_FAIL("Unknown day counter: " + s);
            QL_DUMMY_RETURN((DayCounter*)(0));
        }
        std::string __str__() {
            return self->name()+" day counter";
        }
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        bool __eq__(const DayCounter& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON)
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


#endif
