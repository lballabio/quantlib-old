
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

#ifndef quantlib_calendar_i
#define quantlib_calendar_i

%include common.i
%include date.i
%include string.i

%{
using QuantLib::RollingConvention;
using QuantLib::Preceding;
using QuantLib::ModifiedPreceding;
using QuantLib::Following;
using QuantLib::ModifiedFollowing;
%}

// typemap rolling conventions to corresponding strings

%{
RollingConvention rollconvFromString(string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "f" || s == "fol" || s == "following")
        return Following;
    else if (s == "mf" ||s == "modfol" || s == "modifiedfollowing")
        return ModifiedFollowing;
    else if (s == "p" || s == "pre" || s == "preceding")
        return Preceding;
    else if (s == "mp" ||s == "modpre" || s == "modifiedpreceding")
        return ModifiedPreceding;
    else 
        throw Error("unknown rolling convention");
}

string rollconvToString(RollingConvention rc) {
    switch (rc) {
      case Following:
        return "Following";
      case ModifiedFollowing:
        return "ModifiedFollowing";
      case Preceding:
        return "Preceding";
      case ModifiedPreceding:
        return "ModifiedPreceding";
      default:
        throw Error("unknown rolling convention");
    }
}
%}

MapToString(RollingConvention,rollconvFromString,rollconvToString);


%{
using QuantLib::Calendar;
using QuantLib::Calendars::Frankfurt;
using QuantLib::Calendars::Helsinki;
using QuantLib::Calendars::Johannesburg;
using QuantLib::Calendars::London;
using QuantLib::Calendars::Milan;
using QuantLib::Calendars::NewYork;
using QuantLib::Calendars::TARGET;
using QuantLib::Calendars::Tokyo;
using QuantLib::Calendars::Toronto;
using QuantLib::Calendars::Sydney;
using QuantLib::Calendars::Wellington;
using QuantLib::Calendars::Zurich;
%}

#if defined(SWIGMZSCHEME)
%rename(is_business_day) isBusinessDay;
%rename(is_holiday)      isHoliday;
%rename(str)             __str__;
#endif

// export Calendar
class Calendar {
  public:
    // constructor redefined below as string-based factory
    bool isBusinessDay(const Date& d);
    bool isHoliday(const Date& d);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    Date roll(const Date& d, 
              RollingConvention convention = Following);
    Date advance(const Date& d, int n, TimeUnit unit,
                 RollingConvention convention = Following);
    #endif
    #if defined(SWIGRUBY)
    %pragma(ruby) pred = "isBusinessDay";
    %pragma(ruby) pred = "isHoliday";
    #endif
};


%addmethods Calendar {
    Calendar(const string& name) {
        string s = StringFormatter::toLowercase(name);
        if (s == "target" || s == "euro" || s == "eur")
            return new TARGET;
        else if (s == "newyork" || s == "ny" || s == "nyc")
            return new NewYork;
        else if (s == "london" || s == "lon")
            return new London;
        else if (s == "milan" || s == "mil")
            return new Milan;
        else if (s == "frankfurt" || s == "fft")
            return new Frankfurt;
        else if (s == "zurich" || s == "zur")
            return new Zurich;
        else if (s == "helsinki")
            return new Helsinki;
        else if (s == "johannesburg")
            return new Johannesburg;
        else if (s == "wellington")
            return new Wellington;
        else if (s == "tokyo")
            return new Tokyo;
        else if (s == "toronto")
            return new Toronto;
        else if (s == "sydney")
            return new Sydney;
        else
            throw Error("Unknown calendar: " + name);
        QL_DUMMY_RETURN((Calendar*)(0));
    }
    string __str__() {
        return self->name()+" calendar";
    }

    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    string __repr__() {
        return "Calendar('" + self->name() + "')";
    }
    int __cmp__(const Calendar& other) {
        return ((*self) == other ? 0 : 1);
    }
    #endif

    #if defined(SWIGMZSCHEME)
    Date* roll(const Date& d, 
               RollingConvention convention = Following) {
        return new Date(self->roll(d,convention));
    }
    Date* advance(const Date& d, int n, TimeUnit unit,
                  RollingConvention convention = Following) {
        return new Date(self->advance(d,n,unit,convention));
    }
    bool equal(const Calendar& other) {
        return (*self) == other;
    }
    #endif
}


#endif

