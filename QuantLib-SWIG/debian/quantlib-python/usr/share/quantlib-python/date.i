
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2005 Johan Witters
 Copyright (C) 2013 Simon Shakeshaft

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
using QuantLib::EveryFourthWeek;
using QuantLib::Biweekly;
using QuantLib::Weekly;
using QuantLib::Daily;
using QuantLib::OtherFrequency;
%}

enum Frequency {
    NoFrequency = -1,
    Once = 0,
    Annual = 1,
    Semiannual = 2,
    EveryFourthMonth = 3,
    Quarterly = 4,
    Bimonthly = 6,
    Monthly = 12,
    EveryFourthWeek = 13,
    Biweekly = 26,
    Weekly = 52,
    Daily = 365,
    OtherFrequency = 999
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
    Period();
    Period(Integer n, TimeUnit units);
    explicit Period(Frequency);
    Integer length() const;
    TimeUnit units() const;
    Frequency frequency() const;
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
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGR)
        Period __neg__() {
            return -(*self);
        }
        Period __mul__(Integer n) {
            return *self*n;
        }
        #if defined(SWIGPYTHON)
        Period __rmul__(Integer n) {
            return *self*n;
        }
        bool __lt__(const Period& other) {
            return *self < other;
        }
        #endif
        bool __eq__(const Period& other) {
            return *self == other;
        }
        int __cmp__(const Period& other) {
            return *self < other  ? -1 :
                   *self == other ?  0 :
                                     1;
        }
        #endif
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
setAs("_p_Date", "character",
function(from) {from$ISO()})

setAs("character", "_p_Date",
function(from) { DateParser_parseISO(from) })


setMethod("as.numeric", "_p_Date",
    function(x) x$serialNumber())
setMethod("+", c("_p_Date", "numeric"),
    function(e1,e2) Date___add__(e1,e2))
setMethod("-", c("_p_Date", "numeric"),
    function(e1,e2) Date___sub__(e1,e2))
setMethod("+", c("_p_Date", "_p_Period"),
    function(e1,e2) Date___add__(e1,e2))
setMethod("-", c("_p_Date", "_p_Period"),
    function(e1,e2) Date___sub__(e1,e2))

setAs("character", "_p_Period",
function(from) {Period(from)})
%}
#endif

#if defined(SWIGRUBY)
%mixin Date "Comparable";
#endif

#if defined(SWIGCSHARP)
%typemap(cscode) Date %{
    public static Date operator+(Date d, int i) {
        return new Date(d.serialNumber() + i);
    }
    public static Date operator-(Date d, int i) {
        return new Date(d.serialNumber() - i);
    }
    public static bool operator==(Date d1, Date d2) {
        object o1 = (object)d1;
        object o2 = (object)d2;
        if (o1 == null && o2 == null)
            return true;
        if (o1 == null || o2 == null)
            return false;
        return d1.serialNumber() == d2.serialNumber();
    }
    public static bool operator!=(Date d1, Date d2) {
        object o1 = (object)d1;
        object o2 = (object)d2;
        if (o1 == null && o2 == null)
            return false;
        if (o1 == null || o2 == null)
            return true;
        return d1.serialNumber() != d2.serialNumber();
    }
    public static bool operator>(Date d1, Date d2) {
        object o1 = (object)d1;
        object o2 = (object)d2;
        if (o1 == null || o2 == null)
            return false;
        return d1.serialNumber() > d2.serialNumber();
    }
    public static bool operator<(Date d1, Date d2) {
        object o1 = (object)d1;
        object o2 = (object)d2;
        if (o1 == null || o2 == null)
            return false;
        return d1.serialNumber() < d2.serialNumber();
    }
    public override bool Equals(object o)
    {
        try
        {
            Date d = (Date)o;
            return this.serialNumber() == d.serialNumber();
        }
        catch
        {
           return false;
        }
   }
   public override int GetHashCode()
   {
       return this.serialNumber();
   }
%}
#endif

%{
    // used in Date(string, string) defined below
    void _replace_format(std::string& s, const std::string& old_format,
                         const std::string& new_format) {
        std::string::size_type i = s.find(old_format);
        if (i != std::string::npos)
            s.replace(i, old_format.length(), new_format);
    }
%}

class Date {
    #if defined(SWIGRUBY)
    %rename("isLeap?")        isLeap;
    %rename("isEndOfMonth?")         isEndOfMonth;
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
    %rename("is-eom?")        isEndOfMonth;
    %rename("next-weekday")   nextWeekday;
    %rename("nth-weekday")    nthWeekday;
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
    static bool isEndOfMonth(const Date&);
    static Date nextWeekday(const Date&, Weekday);
    static Date nthWeekday(Size n, Weekday, Month m, Year y);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGJAVA) \
     || defined(SWIGR) || defined(SWIGCSHARP)
    Date operator+(BigInteger days) const;
    Date operator-(BigInteger days) const;
    Date operator+(const Period&) const;
    Date operator-(const Period&) const;
    #endif
    %extend {
        Date(const std::string& str, std::string fmt) {
            // convert our old format into the corresponding Boost one
            _replace_format(fmt, "YYYY", "%Y");
            _replace_format(fmt, "yyyy", "%Y");
            _replace_format(fmt, "YY", "%y");
            _replace_format(fmt, "yy", "%y");
            _replace_format(fmt, "MM", "%m");
            _replace_format(fmt, "mm", "%m");
            _replace_format(fmt, "DD", "%d");
            _replace_format(fmt, "dd", "%d");
            return new Date(DateParser::parseFormatted(str,fmt));
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
            if (*self == Date())
                out << "Date()";
            else
                out << "Date(" << self->dayOfMonth() << ","
                    << int(self->month()) << "," << self->year() << ")";
            return out.str();
        }
        std::string ISO() {
            std::ostringstream out;
            out << QuantLib::io::iso_date(*self);
            return out.str();
        }
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGR)
        BigInteger operator-(const Date& other) {
            return *self - other;
        }
        bool __eq__(const Date& other) {
            return *self == other;
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
        bool __lt__(const Date& other) {
            return *self < other;
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

class DateParser {
  public:
    static Date parseFormatted(const std::string& str, const std::string& fmt);
    static Date parseISO(const std::string& str);
    %extend {
        static Date parse(const std::string& str, std::string fmt) {
            // convert our old format into the corresponding Boost one
            _replace_format(fmt, "YYYY", "%Y");
            _replace_format(fmt, "yyyy", "%Y");
            _replace_format(fmt, "YY", "%y");
            _replace_format(fmt, "yy", "%y");
            _replace_format(fmt, "MM", "%m");
            _replace_format(fmt, "mm", "%m");
            _replace_format(fmt, "DD", "%d");
            _replace_format(fmt, "dd", "%d");
            return DateParser::parseFormatted(str,fmt);
        }
    }
};

class PeriodParser {
  public:
    static Period parse(const std::string& str);
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

#if defined(SWIGR)


%Rruntime %{
setAs("_p_std__vectorTDate_std__allocatorTDate_t_t",
    "character",
function(from) {if (from$size())
sapply(1:from$size(), function(y) from$"__getitem__"(i=y-1)$ISO())} )

setAs("character", "_p_std__vectorTDate_std__allocatorTDate_t_t",
function(from) { a <- DateVector(length(from));
sapply(1:length(from), function(n) {
a[n] <- from[n] } )
a
})

%}

bool operator==(const Date&, const Date&);
bool operator!=(const Date&, const Date&);
bool operator<(const Date&, const Date&);
bool operator<=(const Date&, const Date&);
bool operator>(const Date&, const Date&);
bool operator>=(const Date&, const Date&);

#endif

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

%{
using QuantLib::IMM;
%}

struct IMM {
    #if defined(SWIGRUBY)
    %rename("isIMMdate?")        isIMMdate;
    %rename("isIMMcode?")        isIMMcode;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-imm-date?")      isIMMdate;
    %rename("is-imm-code?")      isIMMcode;
    %rename("next-date")         nextDate;
    %rename("next-code")         nextCode;
    #endif
    enum Month { F =  1, G =  2, H =  3,
                 J =  4, K =  5, M =  6,
                 N =  7, Q =  8, U =  9,
                 V = 10, X = 11, Z = 12 };

    static bool isIMMdate(const Date& d,
                          bool mainCycle = true);
    static bool isIMMcode(const std::string& code,
                          bool mainCycle = true);
    static std::string code(const Date& immDate);
    static Date date(const std::string& immCode,
                     const Date& referenceDate = Date());
    static Date nextDate(const Date& d = Date(),
                         bool mainCycle = true);
    static Date nextDate(const std::string& immCode,
                         bool mainCycle = true,
                         const Date& referenceDate = Date());
    static std::string nextCode(const Date& d = Date(),
                                bool mainCycle = true);
    static std::string nextCode(const std::string& immCode,
                                bool mainCycle = true,
                                const Date& referenceDate = Date());
};


#endif
