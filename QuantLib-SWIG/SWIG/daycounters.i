
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_day_counters_i
#define quantlib_day_counters_i

%include date.i
%include types.i
%include stl.i
%include null.i

%{
using QuantLib::DayCounter;
using QuantLib::DayCounters::Actual360;
using QuantLib::DayCounters::Actual365;
using QuantLib::DayCounters::Thirty360;
using QuantLib::DayCounters::ActualActual;
%}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("day-count")     dayCount;
%rename("year-fraction") yearFraction;
%rename(">string")       __str__;
%rename("equal")         __eq__;
#endif
#if defined(SWIGGUILE)
%scheme%{ 
    (define DayCounter=? DayCounter-equal) 
    (export DayCounter=?)
%}
#endif

class DayCounter {
  public:
    // constructor redefined below as string-based factory
    int dayCount(const Date& d1, const Date& d2);
    Time yearFraction(const Date& d1, const Date& d2,
                      const Date& startRef = NullDate, 
                      const Date& endRef = NullDate);
};

// replicate the DayCounter interface
%addmethods DayCounter {
    DayCounter(std::string s) {
        s = StringFormatter::toLowercase(s);
        if (s == "act365" || s == "act/365")
            return new Actual365;
        else if (s == "act360" || s == "act/360")
            return new Actual360;
        else if (s == "30/360" || s == "30/360us")
            return new Thirty360(Thirty360::USA);
        else if (s == "30e/360" || s == "30/360e" || s == "30/360eu")
            return new Thirty360(Thirty360::European);
        else if (s == "30/360i" || s == "30/360it")
            return new Thirty360(Thirty360::Italian);
        else if (s == "actact" || s == "act/act" || 
                 s == "act/act(b)" || s == "act/act (Bond)")
            return new ActualActual(ActualActual::Bond);
        else if (s == "actacte" || s == "act/act(e)" 
                 || s == "act/act(Euro)")
            return new ActualActual(ActualActual::Euro);
        else if (s == "actacth" || s == "act/act(h)" 
                 || s == "act/act (ISDA)")
            return new ActualActual(ActualActual::Historical);
        else
            throw Error("Unknown day counter: " + s);
        QL_DUMMY_RETURN((DayCounter*)(0));
    }
    std::string __str__() {
        return self->name()+" day counter";
    }
    bool __eq__(const DayCounter& other) {
        return (*self) == other;
    }
    
    #if defined(SWIGPYTHON)
    bool __ne__(const DayCounter& other) {
        return (*self) != other;
    }
    #endif
   
}



#endif
