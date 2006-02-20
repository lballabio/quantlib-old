
/*
 Copyright (C) 2000-2005 StatPro Italia srl
 Copyright (C) 2005 Johan Witters

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

#ifndef quantlib_date_i
#define quantlib_date_i

%include common.i
%include types.i
%include stl.i

%{
using QuantLib::Day;
using QuantLib::Year;
%}

typedef Integer Day;
typedef Integer Year;

#if defined(SWIGJAVA)
%javaconst(1);
#endif

%{
using QuantLib::Weekday;
using QuantLib::Sunday;
using QuantLib::Monday;
using QuantLib::Tuesday;
using QuantLib::Wednesday;
using QuantLib::Thursday;
using QuantLib::Friday;
using QuantLib::Saturday;
%}

enum Weekday {
    Sunday    = 1,
    Monday    = 2,
    Tuesday   = 3,
    Wednesday = 4,
    Thursday  = 5,
    Friday    = 6,
    Saturday  = 7
};


%{
using QuantLib::Month;
using QuantLib::January;
using QuantLib::February;
using QuantLib::March;
using QuantLib::April;
using QuantLib::May;
using QuantLib::June;
using QuantLib::July;
using QuantLib::August;
using QuantLib::September;
using QuantLib::October;
using QuantLib::November;
using QuantLib::December;
%}

enum Month {
    January   = 1,
    February  = 2,
    March     = 3,
    April     = 4,
    May       = 5,
    June      = 6,
    July      = 7,
    August    = 8,
    September = 9,
    October   = 10,
    November  = 11,
    December  = 12
};


%{
using QuantLib::TimeUnit;
using QuantLib::Days;
using QuantLib::Weeks;
using QuantLib::Months;
using QuantLib::Years;
%}

enum TimeUnit { Days, Weeks, Months, Years };


%{
using QuantLib::Frequency;
using QuantLib::NoFrequency;
using QuantLib::Once;
using QuantLib::Annual;
using QuantLib::Semiannual;
using QuantLib::EveryFourthMonth;
using QuantLib::Quarterly;
using QuantLib::Bimonthly;
using QuantLib::Monthly;
%}

enum Frequency {
    NoFrequency = -1,
    Once = 0,
    Annual = 1,
    Semiannual = 2,
    EveryFourthMonth = 3,
    Quarterly = 4,
    Bimonthly = 6,
    Monthly = 12
};

#if defined(SWIGJAVA)
%javaconst(0);
#endif

// time period

%{
using QuantLib::Period;
using QuantLib::PeriodParser;
%}

class Period {
    #if defined(SWIGJAVA)
    %rename("repr")           __repr__;
    %rename("compare")        __cmp__;
    #endif
  public:
    Period(Integer n, TimeUnit units);
    Integer length() const;
    TimeUnit units() const;
    %extend {
        Period(const std::string& str) {
            return new Period(PeriodParser::parse(str));
        }
        std::string __str__() {
            std::ostringstream out;
            out << *self;
            return out.str();
        }
        std::string __repr__() {
            std::ostringstream out;
            out << "Period(\"" << QuantLib::io::short_period(*self) << "\")";
            return out.str();
        }
        int __cmp__(const Period& other) {
            if (*self < other)
                return -1;
            if (*self == other)
                return 0;
            return 1;
        }
    }
};

namespace std {
    %template(PeriodVector) vector<Period>;
}



%{
using QuantLib::Date;
using QuantLib::DateParser;
%}

#if defined(SWIGR)
%Rruntime %{
setMethod("as.numeric", "_p_Date",
    function(x) x$serialNumber())			  
%}
#endif

#if defined(SWIGRUBY)
%mixin Date "Comparable";
#endif
class Date {
    #if defined(SWIGRUBY)
    %rename("isLeap?")        isLeap;
    %rename("isEOM?")         isEOM;
    %rename("isIMMdate?")     isIMMdate;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-of-month")   dayOfMonth;
    %rename("day-of-year")    dayOfYear;
    %rename("weekday-number") weekdayNumber;
    %rename("serial-number")  serialNumber;
    %rename("is-leap?")       isLeap;
    %rename("min-date")       minDate;
    %rename("max-date")       maxDate;
    %rename("todays-date")    todaysDate;
    %rename("end-of-month")   endOfMonth;
    %rename("is-eom?")        isEOM;
    %rename("next-weekday")   nextWeekday;
    %rename("nth-weekday")    nthWeekday;
    %rename("is-imm-date?")   isIMMdate;
    %rename("next-imm-date")  nextIMMdate;
    #endif
  public:
    Date();
    Date(Day d, Month m, Year y);
    Date(BigInteger serialNumber);
    // access functions
    Weekday weekday() const;
    Day dayOfMonth() const;
    Day dayOfYear() const;        // one-based
    Month month() const;
    Year year() const;
    BigInteger serialNumber() const;
    // static methods
    static bool isLeap(Year y);
    static Date minDate();
    static Date maxDate();
    static Date todaysDate();
    static Date endOfMonth(const Date&);
    static bool isEOM(const Date&);
    static Date nextWeekday(const Date&, Weekday);
    static Date nthWeekday(Size n, Weekday, Month m, Year y);
    static bool isIMMdate(const Date&);
    static Date nextIMMdate(const Date&);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGJAVA) || defined(SWIGR) 
    Date operator+(BigInteger days) const;
    Date operator-(BigInteger days) const;
    Date operator+(const Period&) const;
    Date operator-(const Period&) const;
    #endif
    %extend {
        Date(const std::string& str, const std::string& fmt) {
            return new Date(DateParser::parse(str,fmt));
        }
        Integer weekdayNumber() {
            return int(self->weekday());
        }
        std::string __str__() {
            std::ostringstream out;
            out << *self;
            return out.str();
        }
        std::string __repr__() {
            std::ostringstream out;
            out << "Date(" << self->dayOfMonth() << ","
                << int(self->month()) << "," << self->year() << ")";
            return out.str();
        }
        std::string ISO() {
            std::ostringstream out;
            out << QuantLib::io::iso_date(*self);
            return out.str();
        }
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        BigInteger operator-(const Date& other) {
            return *self - other;
        }
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
        bool __nonzero__() {
            return (*self != Date());
        }
        int __hash__() {
            return self->serialNumber();
        }
        #endif
        #if defined(SWIGRUBY)
        Date succ() {
            return *self + 1;
        }
        #endif
        #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        Date advance(Integer n, TimeUnit units) {
            return *self + n*units;
        }
        #endif
    }
};

#if defined(SWIGPYTHON)
%pythoncode %{
Date._old___add__ = Date.__add__
Date._old___sub__ = Date.__sub__
def Date_new___add__(self,x):
    if type(x) is tuple and len(x) == 2:
        return self._old___add__(Period(x[0],x[1]))
    else:
        return self._old___add__(x)
def Date_new___sub__(self,x):
    if type(x) is tuple and len(x) == 2:
        return self._old___sub__(Period(x[0],x[1]))
    else:
        return self._old___sub__(x)
Date.__add__ = Date_new___add__
Date.__sub__ = Date_new___sub__
%}
#endif

namespace std {
    %template(DateVector) vector<Date>;
}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Date=?")  Date_equal;
%rename("Date<?")  Date_less;
%rename("Date<=?") Date_less_equal;
%rename("Date>?")  Date_greater;
%rename("Date>=?") Date_greater_equal;
%inline %{
    // difference - comparison
    BigInteger Date_days_between(const Date& d1, const Date& d2) {
        return d2-d1;
    }
    bool Date_equal(const Date& d1, const Date& d2) {
        return d1 == d2;
    }
    bool Date_less(const Date& d1, const Date& d2) {
        return d1 < d2;
    }
    bool Date_less_equal(const Date& d1, const Date& d2) {
        return d1 <= d2;
    }
    bool Date_greater(const Date& d1, const Date& d2) {
        return d1 > d2;
    }
    bool Date_greater_equal(const Date& d1, const Date& d2) {
        return d1 >= d2;
    }
%}
#endif


#endif
