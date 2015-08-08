
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
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

#ifndef quantlib_calendar_i
#define quantlib_calendar_i

%include common.i
%include date.i
%include stl.i

%{
using QuantLib::Calendar;
%}

%{
using QuantLib::BusinessDayConvention;
using QuantLib::Following;
using QuantLib::ModifiedFollowing;
using QuantLib::Preceding;
using QuantLib::ModifiedPreceding;
using QuantLib::Unadjusted;
using QuantLib::HalfMonthModifiedFollowing;
%}

enum BusinessDayConvention {
    Following,
    ModifiedFollowing,
    Preceding,
    ModifiedPreceding,
    Unadjusted,
    HalfMonthModifiedFollowing
};

%{
using QuantLib::JointCalendarRule;
using QuantLib::JoinHolidays;
using QuantLib::JoinBusinessDays;
%}

enum JointCalendarRule { JoinHolidays, JoinBusinessDays };


#if defined(SWIGRUBY)
%mixin Calendar "Comparable";
#endif
class Calendar {
    #if defined(SWIGRUBY)
    %rename("isBusinessDay?")   isBusinessDay;
    %rename("isHoliday?")       isHoliday;
    %rename("isEndOfMonth?")    isEndOfMonth;
    %rename("addHoliday!")      addHoliday;
    %rename("removeHoliday!")   removeHoliday;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-business-day?") isBusinessDay;
    %rename("is-holiday?")      isHoliday;
    %rename("is-end-of-month?") isEndOfMonth;
    %rename("add-holiday")      addHoliday;
    %rename("remove-holiday")   removeHoliday;
    #endif
  protected:
    Calendar();
  public:
    bool isBusinessDay(const Date&);
    bool isHoliday(const Date&);
    bool isEndOfMonth(const Date&);
    void addHoliday(const Date&);
    void removeHoliday(const Date&);
    Date adjust(const Date& d,
                BusinessDayConvention convention = QuantLib::Following);
    Date advance(const Date& d, Integer n, TimeUnit unit,
                 BusinessDayConvention convention = QuantLib::Following,
                 bool endOfMonth = false);
    Date advance(const Date& d, const Period& period,
                 BusinessDayConvention convention = QuantLib::Following,
                 bool endOfMonth = false);
    BigInteger businessDaysBetween(const Date& from,
                                   const Date& to,
                                   bool includeFirst = true,
                                   bool includeLast = false);
    std::string name();
    %extend {
        #if !defined(SWIGPERL)
        std::string __str__() {
            return self->name()+" calendar";
        }
        #endif
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGJAVA)
        bool __eq__(const Calendar& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON) || defined(SWIGJAVA)
        bool __ne__(const Calendar& other) {
            return (*self) != other;
        }
        #endif
        #endif
    }
};

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Calendar=?") Calendar_equal;
%inline %{
    bool Calendar_equal(const Calendar& c1, const Calendar& c2) {
        return c1 == c2;
    }
%}
#endif

namespace QuantLib {

    class Argentina : public Calendar {
      public:
        enum Market { Merval };
        Argentina(Market m = Merval);
    };

    class Australia : public Calendar {};

    class Brazil : public Calendar {
      public:
        enum Market { Settlement, Exchange };
        Brazil(Market m = Settlement);
    };

    class Canada : public Calendar {
      public:
        enum Market { Settlement, TSX };
        Canada(Market m = Settlement);
    };

    class China : public Calendar {
      public:
        enum Market { SSE, IB };
        China(Market m = SSE);
    };

    class CzechRepublic : public Calendar {
      public:
        enum Market { PSE };
        CzechRepublic(Market m = PSE);
    };

    class Denmark : public Calendar {};
    class Finland : public Calendar {};

    class Germany : public Calendar {
      public:
        enum Market { Settlement, FrankfurtStockExchange, Xetra, Eurex };
        Germany(Market m = FrankfurtStockExchange);
    };

    class HongKong : public Calendar {
      public:
        enum Market { HKEx };
        HongKong(Market m = HKEx);
    };

    class Hungary : public Calendar {};

    class Iceland : public Calendar {
      public:
        enum Market { ICEX };
        Iceland(Market m = ICEX);
    };

    class India : public Calendar {
      public:
        enum Market { NSE };
        India(Market m = NSE);
    };

    class Indonesia : public Calendar {
      public:
        enum Market { BEJ, JSX };
        Indonesia(Market m = BEJ);
    };

    class Italy : public Calendar {
      public:
        enum Market { Settlement, Exchange };
        Italy(Market m = Settlement);
    };

    class Japan : public Calendar {};

    class Mexico : public Calendar {
      public:
        enum Market { BMV };
        Mexico(Market m = BMV);
    };

    class NewZealand : public Calendar {};
    class Norway : public Calendar {};
    class Poland : public Calendar {};

    class Russia : public Calendar {
      public:
        enum Market { Settlement, MOEX };
        Russia(Market m = Settlement);
    };

    class SaudiArabia : public Calendar {
      public:
        enum Market { Tadawul };
        SaudiArabia(Market m = Tadawul);
    };

    class Singapore : public Calendar {
      public:
        enum Market { SGX };
        Singapore(Market m = SGX);
    };

    class Slovakia : public Calendar {
      public:
        enum Market { BSSE };
        Slovakia(Market m = BSSE);
    };

    class SouthAfrica : public Calendar {};

    class SouthKorea : public Calendar {
      public:
        enum Market { Settlement, KRX };
        SouthKorea(Market m = KRX);
    };

    class Sweden : public Calendar {};
    class Switzerland : public Calendar {};

    class Taiwan : public Calendar {
      public:
        enum Market { TSEC };
        Taiwan(Market m = TSEC);
    };

    class TARGET : public Calendar {};
    class Turkey : public Calendar {};

    class Ukraine : public Calendar {
      public:
        enum Market { USE };
        Ukraine(Market m = USE);
    };

    class UnitedKingdom : public Calendar {
      public:
        enum Market { Settlement, Exchange, Metals };
        UnitedKingdom(Market m = Settlement);
    };

    class UnitedStates : public Calendar {
      public:
        enum Market { Settlement, NYSE, GovernmentBond, NERC };
        UnitedStates(Market m = Settlement);
    };

    // others

    class NullCalendar : public Calendar {};

    class WeekendsOnly : public Calendar {};

    class JointCalendar : public Calendar {
      public:
        JointCalendar(const Calendar&, const Calendar&,
                      JointCalendarRule rule = QuantLib::JoinHolidays);
        JointCalendar(const Calendar&, const Calendar&, const Calendar&,
                      JointCalendarRule rule = QuantLib::JoinHolidays);
        JointCalendar(const Calendar&, const Calendar&,
                      const Calendar&, const Calendar&,
                      JointCalendarRule rule = QuantLib::JoinHolidays);
    };

    class BespokeCalendar : public Calendar {
      public:
        BespokeCalendar(const std::string& name);
        void addWeekend(Weekday);
    };

}


#endif

