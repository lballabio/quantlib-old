
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
%include string.i
%include null.i

%{
using QuantLib::DayCounter;
using QuantLib::DayCounters::Actual360;
using QuantLib::DayCounters::Actual365;
using QuantLib::DayCounters::ActualActual;
using QuantLib::DayCounters::Thirty360;
%}

#if defined(SWIGMZSCHEME)
%rename(day_count)     dayCount;
%rename(year_fraction) yearFraction;
%rename(str)           __str__;
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
    DayCounter(const string& name) {
        string s = StringFormatter::toLowercase(name);
        if (s == "act365" || s == "act/365")
            return new Actual365;
        else if (s == "act360" || s == "act/360")
            return new Actual360;
        else if (s == "actacte" || s == "act/act(e)" || s == "act/act(Euro)")
            return new ActualActual(ActualActual::Euro);
        else if (s == "30/360" || s == "30/360us")
            return new Thirty360(Thirty360::USA);
        else if (s == "30e/360" || s == "30/360e" || s == "30/360eu")
            return new Thirty360(Thirty360::European);
        else if (s == "30/360i" || s == "30/360it")
            return new Thirty360(Thirty360::Italian);
        else if (s == "actact" || s == "act/act" || 
                 s == "act/act(b)" || s == "act/act (Bond)")
            return new ActualActual(ActualActual::Bond);
        else if (s == "actacth" || s == "act/act(h)" || s == "act/act (ISDA)")
            return new ActualActual(ActualActual::Historical);
        else
            throw Error("Unknown day counter: " + name);
        QL_DUMMY_RETURN((DayCounter*)(0));
    }
    string __str__() {
        return self->name()+" day counter";
    }

    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    string __repr__() {
        return "DayCounter('"+self->name()+"')";
    }
    int __cmp__(const DayCounter& other) {
        return ((*self) == other ? 0 : 1);
    }
    #endif

    #if defined(SWIGMZSCHEME)
    bool equal(const DayCounter& other) {
        return (*self) == other;
    }
    #endif

}


#endif
