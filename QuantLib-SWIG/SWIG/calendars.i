
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
%include stl.i

// typemap rolling conventions to corresponding strings

%{
using QuantLib::RollingConvention;

RollingConvention rollconvFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "f" || s == "fol" || s == "following")
        return QuantLib::Following;
    else if (s == "mf" ||s == "modfol" || s == "modifiedfollowing")
        return QuantLib::ModifiedFollowing;
    else if (s == "p" || s == "pre" || s == "preceding")
        return QuantLib::Preceding;
    else if (s == "mp" ||s == "modpre" || s == "modifiedpreceding")
        return QuantLib::ModifiedPreceding;
    else 
        throw Error("unknown rolling convention");
}

std::string rollconvToString(RollingConvention rc) {
    switch (rc) {
      case QuantLib::Following:
        return "Following";
      case QuantLib::ModifiedFollowing:
        return "ModifiedFollowing";
      case QuantLib::Preceding:
        return "Preceding";
      case QuantLib::ModifiedPreceding:
        return "ModifiedPreceding";
      default:
        throw Error("unknown rolling convention");
    }
}
%}

MapToString(RollingConvention,rollconvFromString,rollconvToString);


%{
using QuantLib::Calendar;
using QuantLib::Calendars::TARGET;
using QuantLib::Calendars::NewYork;
using QuantLib::Calendars::London;
using QuantLib::Calendars::Milan;
using QuantLib::Calendars::Frankfurt;
using QuantLib::Calendars::Zurich;
using QuantLib::Calendars::Helsinki;
using QuantLib::Calendars::Johannesburg;
using QuantLib::Calendars::Wellington;
using QuantLib::Calendars::Tokyo;
using QuantLib::Calendars::Toronto;
using QuantLib::Calendars::Sydney;
%}

#if defined(SWIGGUILE)
%scheme%{ 
%}
#endif


class Calendar {
    #if defined(SWIGRUBY)
    %rename("isBusinessDay?")   isBusinessDay;
    %rename("isHoliday?")       isHoliday;
    #endif
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-business-day?") isBusinessDay;
    %rename("is-holiday?")      isHoliday;
    %rename(">string")          __str__;
    #if defined(SWIGGUILE)
    // resolve overloading
    %rename(advance_period)     advance(const Date&,const Period&,
                                        RollingConvention);
    %rename(advance_units)      advance(const Date&,int,TimeUnit,
                                        RollingConvention);
    %scheme %{
    (define (Calendar-advance . args)
      (if (integer? (caddr args))
          (apply Calendar-advance-units args)
          (apply Calendar-advance-period args)))
    (export Calendar-advance)
    %}
    #endif
    #endif
  private:
    Calendar();
  public:
    // constructor redefined below as string-based factory
    bool isBusinessDay(const Date& d);
    bool isHoliday(const Date& d);
    Date roll(const Date& d, 
              RollingConvention convention = QuantLib::Following);
    Date advance(const Date& d, int n, TimeUnit unit,
                 RollingConvention convention = QuantLib::Following);
    Date advance(const Date& d, const Period& period,
                 RollingConvention convention = QuantLib::Following);
    %extend {
        Calendar(const std::string& name) {
            std::string s = StringFormatter::toLowercase(name);
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
        std::string __str__() {
            return self->name()+" calendar";
        }
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        bool __eq__(const Calendar& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON)
        bool __ne__(const Calendar& other) {
            return (*self) != other;
        }
        #endif
        #endif
    }
};
ReturnByValue(Calendar);

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Calendar=?") Calendar_equal;
%inline %{
    bool Calendar_equal(const Calendar& c1, const Calendar& c2) {
        return c1 == c2;
    }
%}
#endif


#endif

