
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
using QuantLib::Unadjusted;
using QuantLib::Preceding;
using QuantLib::ModifiedPreceding;
using QuantLib::Following;
using QuantLib::ModifiedFollowing;
using QuantLib::MonthEndReference;
%}

enum BusinessDayConvention {
    Unadjusted,
    Preceding,
    ModifiedPreceding,
    Following,
    ModifiedFollowing,
    MonthEndReference
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
    // constructor redefined below as string-based factory
    bool isBusinessDay(const Date&);
    bool isHoliday(const Date&);
    bool isEndOfMonth(const Date&);
    void addHoliday(const Date&);
    void removeHoliday(const Date&);
    Date adjust(const Date& d,
                BusinessDayConvention convention = QuantLib::Following,
                const Date& origin = Date());
    Date advance(const Date& d, Integer n, TimeUnit unit,
                 BusinessDayConvention convention = QuantLib::Following);
    Date advance(const Date& d, const Period& period,
                 BusinessDayConvention convention = QuantLib::Following);
    %extend {
        std::string __str__() {
            return self->name()+" calendar";
        }
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

    class Beijing : public Calendar {};
    class Bombay : public Calendar {};
    class Bratislava : public Calendar {};
    class Budapest : public Calendar {};
    class Copenhagen : public Calendar {};

    class Germany : public Calendar {
      public:
        enum Market { Settlement, FrankfurtStockExchange, Xetra, Eurex };
        Germany(Market m = FrankfurtStockExchange);
    };

    class Helsinki : public Calendar {};
    class HongKong : public Calendar {};

    class Italy : public Calendar {
      public:
        enum Market { Settlement, Exchange };
        Italy(Market m = Settlement);
    };

    class Istanbul : public Calendar {};
    class Johannesburg : public Calendar {};
    class NullCalendar : public Calendar {};
    class Oslo : public Calendar {};
    class Prague : public Calendar {};
    class Riyadh : public Calendar {};
    class Seoul : public Calendar {};
    class Singapore : public Calendar {};
    class Stockholm : public Calendar {};
    class Sydney : public Calendar {};
    class TARGET : public Calendar {};
    class Taipei : public Calendar {};
    class Taiwan : public Calendar {};
    class Tokyo : public Calendar {};
    class Toronto : public Calendar {};

    class UnitedKingdom : public Calendar {
      public:
        enum Market { Settlement, Exchange, Metals };
        UnitedKingdom(Market m = Settlement);
    };

    class UnitedStates : public Calendar {
      public:
        enum Market { Settlement, Exchange, GovernmentBond };
        UnitedStates(Market m = Settlement);
    };

    class Warsaw : public Calendar {};
    class Wellington : public Calendar {};
    class Zurich : public Calendar {};

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

}


#endif

