
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
%include stl.i

%{
using QuantLib::Day;
using QuantLib::Year;
%}

typedef int Day;
typedef int Year;


// typemap weekdays to corresponding strings

%{
using QuantLib::Weekday;

Weekday weekdayFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "sun" || s == "sunday")
        return QuantLib::Sunday;
    else if (s == "mon" || s == "monday")
        return QuantLib::Monday;
    else if (s == "tue" || s == "tuesday")
        return QuantLib::Tuesday;
    else if (s == "wed" || s == "wednesday")
        return QuantLib::Wednesday;
    else if (s == "thu" || s == "thursday")
        return QuantLib::Thursday;
    else if (s == "fri" || s == "friday")
        return QuantLib::Friday;
    else if (s == "sat" || s == "saturday")
        return QuantLib::Saturday;
    else
        throw Error("unknown weekday");
}
 
std::string stringFromWeekday(Weekday w) {
    switch (w) {
      case QuantLib::Sunday:    return "Sunday";
      case QuantLib::Monday:    return "Monday";
      case QuantLib::Tuesday:   return "Tuesday";
      case QuantLib::Wednesday: return "Wednesday";
      case QuantLib::Thursday:  return "Thursday";
      case QuantLib::Friday:    return "Friday";
      case QuantLib::Saturday:  return "Saturday";
      default:                  throw Error("unknown weekday");
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

TimeUnit timeunitFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "d" || s == "day" || s == "days")
        return QuantLib::Days;
    else if (s == "w" || s == "week" || s == "weeks")
        return QuantLib::Weeks;
    else if (s == "m" || s == "month" || s == "months")
        return QuantLib::Months;
    else if (s == "y" || s == "year" || s == "years")
        return QuantLib::Years;
    else 
        throw Error("unknown time unit");
}

std::string stringFromTimeunit(TimeUnit u) {
    switch (u) {
      case QuantLib::Days:   return "days";
      case QuantLib::Weeks:  return "weeks";
      case QuantLib::Months: return "months";
      case QuantLib::Years:  return "years";
      default:               throw Error("unknown time unit");
    }
}
%}

MapToString(TimeUnit,timeunitFromString,stringFromTimeunit);


// time period

%{
using QuantLib::Period;
%}

#if defined(SWIGMZSCHEME)
%rename(str) __str__;
#endif

class Period {
  public:
    Period(int n, TimeUnit units);
    int length() const;
    TimeUnit units() const;
};

%addmethods Period {
    std::string __str__() {
        std::string s = IntegerFormatter::toString(self->length());
        switch (self->units()) {
          case QuantLib::Days:
            return s + " day(s)";
          case QuantLib::Weeks:
            return s + " week(s)";
          case QuantLib::Months:
            return s + " month(s)";
          case QuantLib::Years:
            return s + " year(s)";
          default:
            return "Unknown period";
        }
        QL_DUMMY_RETURN(std::string());
    }
}


// and finally, the Date class

%{
using QuantLib::Date;
using QuantLib::DateFormatter;
Date NullDate = Date();
%}

#if defined(SWIGRUBY)
%rename(__add__) operator+;
%rename(__sub__) operator-;
#endif

#if defined(SWIGMZSCHEME)
%rename(day_of_month)   dayOfMonth;
%rename(day_of_year)    dayOfYear;
%rename(weekday_number) weekdayNumber;
%rename(serial_number)  serialNumber;
%rename(plus_days)   plusDays;
%rename(plus_weeks)  plusWeeks;
%rename(plus_months) plusMonths;
%rename(plus_years)  plusYears;
%rename(is_leap) isLeap;
%rename(min_date) minDate;
%rename(max_date) maxDate;
%rename(str) __str__;
// also, allow return by value
%typemap(out) Date {
    $result = SWIG_MakePtr (new Date($1), SWIGTYPE_p_Date);
}
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
    // increment/decrement dates
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
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    Date operator+(int days) const;
    Date operator-(int days) const;
    #endif
};

%addmethods Date {
    int weekdayNumber() {
        return int(self->weekday());
    }
    std::string __str__() {
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
    std::string __repr__() {
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

#if defined(SWIGMZSCHEME)
%rename(date_from_serial_number) DateFromSerialNumber;
#endif
%inline %{
    Date DateFromSerialNumber(int serialNumber) {
        return Date(serialNumber);
    }
%}



#endif
