
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

#ifndef quantlib_date_i
#define quantlib_date_i

%include types.i
%include string.i

%{
#include <cstdlib>
#include <string>
using QuantLib::Date;
using QuantLib::Day;
using QuantLib::Year;
%}

typedef int Day;
typedef int Year;

// typemap weekdays to corresponding strings

%{
using QuantLib::Weekday;
using QuantLib::Sunday;
using QuantLib::Monday;
using QuantLib::Tuesday;
using QuantLib::Wednesday;
using QuantLib::Thursday;
using QuantLib::Friday;
using QuantLib::Saturday;

using QuantLib::StringFormatter;
using QuantLib::DateFormatter;
using QuantLib::IntegerFormatter;
%}

%{
Weekday weekdayFromString(string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "sun" || s == "sunday")
        return Sunday;
    else if (s == "mon" || s == "monday")
        return Monday;
    else if (s == "tue" || s == "tuesday")
        return Tuesday;
    else if (s == "wed" || s == "wednesday")
        return Wednesday;
    else if (s == "thu" || s == "thursday")
        return Thursday;
    else if (s == "fri" || s == "friday")
        return Friday;
    else if (s == "sat" || s == "saturday")
        return Saturday;
    else
        throw Error("unknown weekday");
}

string stringFromWeekday(Weekday w) {
    switch (w) {
      case Sunday:    return "Sunday";
      case Monday:    return "Monday";
      case Tuesday:   return "Tuesday";
      case Wednesday: return "Wednesday";
      case Thursday:  return "Thursday";
      case Friday:    return "Friday";
      case Saturday:  return "Saturday";
      default:        throw Error("unknown weekday");
    }
}
%}

MapToString(Weekday,weekdayFromString,stringFromWeekday);


// typemap months to corresponding numbers

%{
using QuantLib::Month;
%}

MapToInteger(Month);


// typemap time units to corresponding strings

%{
using QuantLib::TimeUnit;
using QuantLib::Days;
using QuantLib::Weeks;
using QuantLib::Months;
using QuantLib::Years;
%}

%{
TimeUnit timeunitFromString(string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "d" || s == "day" || s == "days")
        return Days;
    else if (s == "w" || s == "week" || s == "weeks")
        return Weeks;
    else if (s == "m" || s == "month" || s == "months")
        return Months;
    else if (s == "y" || s == "year" || s == "years")
        return Years;
    else 
        throw Error("unknown time unit");
}

string stringFromTimeunit(TimeUnit u) {
    switch (u) {
      case Days:   return "days";
      case Weeks:  return "weeks";
      case Months: return "months";
      case Years:  return "years";
      default:        throw Error("unknown time unit");
    }
}
%}

MapToString(TimeUnit,timeunitFromString,stringFromTimeunit);


// time period

%{
using QuantLib::Period;
%}

class Period {
  public:
    Period(int n, TimeUnit units);
    int length() const;
    TimeUnit units() const;
};

%addmethods Period {
    string __str__() {
        string s = IntegerFormatter::toString(self->length());
        switch (self->units()) {
          case Days:
            return s + " day(s)";
          case Weeks:
            return s + " week(s)";
          case Months:
            return s + " month(s)";
          case Years:
            return s + " year(s)";
          default:
            return "Unknown period";
        }
        QL_DUMMY_RETURN(string());
    }
}


// and finally, the Date class

#if defined(SWIGRUBY)
%rename(__add__) operator+;
%rename(__sub__) operator-;
#endif

#if defined(SWIGMZSCHEME)
%rename(day_of_month)  dayOfMonth;
%rename(day_of_year)   dayOfYear;
%rename(weekday_number) weekdayNumber;
%rename(serial_number) serialNumber;
%rename(str) __str__;
#endif

class Date {
  public:
    Date(Day d, Month m, Year y);
    // access functions
    Weekday weekday() const;
    Day dayOfMonth() const;
    Day dayOfYear() const;        // one-based
    Month month() const;
    Year year() const;
    int serialNumber() const;
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    // increment/decrement dates
    Date operator+(int days) const;
    Date operator-(int days) const;
    Date plusDays(int days) const;
    Date plusWeeks(int weeks) const;
    Date plusMonths(int months) const;
    Date plusYears(int years) const;
    Date plus(int units, TimeUnit) const;
    // leap years
    static bool isLeap(Year y);
    // earliest and latest allowed date
    static Date minDate();
    static Date maxDate();
    #endif
};

%addmethods Date {
    int weekdayNumber() {
        return int(self->weekday());
    }
    string __str__() {
        return DateFormatter::toString(*self);
    }
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    int __cmp__(const Date& other) {
        if (*self < other)
            return -1;
        else if (*self == other)
            return 0;
        else 
            return 1;
    }
    #endif
    
    #if defined(SWIGPYTHON)
    string __repr__() {
        return DateFormatter::toString(*self);
    }
    bool __nonzero__() {
        return (*self != Date());
    }
    #endif
    
    #if defined(SWIGRUBY)
    Date succ() {
        return self->plusDays(1);
    }
    #endif

    #if defined(SWIGMZSCHEME)
    // increment/decrement dates
    Date* plus_days(int days) {
		return new Date(self->plusDays(days));
	}
    Date* plus_weeks(int weeks) {
		return new Date(self->plusWeeks(weeks));
	}
    Date* plus_months(int months) {
		return new Date(self->plusMonths(months));
	}
    Date* plus_years(int years) {
		return new Date(self->plusYears(years));
	}
    Date* plus(int n, TimeUnit unit) {
		return new Date(self->plus(n,unit));
	}
	// difference - comparison
    int days_from(const Date& other) {
        return (*self)-other;
    }
	bool equal(const Date& other) {
		return (*self == other);
	}
    bool less(const Date& other) {
        return (*self < other);
    }
    bool less_equal(const Date& other) {
        return (*self <= other);
    }
    bool greater(const Date& other) {
        return (*self > other);
    }
    bool greater_equal(const Date& other) {
        return (*self >= other);
    }
    #endif
}

#if defined(SWIGPYTHON) || defined(SWIGRUBY)
%inline %{
    Date DateFromSerialNumber(int serialNumber) {
        return Date(serialNumber);
    }
%}
#endif

#if defined(SWIGMZSCHEME)
%inline %{
	Date* Date_from_serial_number(int serialNumber) {
		return new Date(serialNumber);
	}
    bool Date_is_leap(int y) {
		return Date::isLeap(Year(y)) ? 1 : 0;
	}
    Date* Date_min_date() {
		return new Date(Date::minDate());
	}
    Date* Date_max_date() {
		return new Date(Date::maxDate());
	}
%}
#endif


#endif
