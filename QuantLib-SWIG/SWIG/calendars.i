
/*
 Copyright (C) 2000-2004 StatPro Italia srl

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

// typemap rolling conventions to corresponding strings
%{
using QuantLib::BusinessDayConvention;

BusinessDayConvention bdconvFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "" || s == "none" || s == "unadjusted")
        return QuantLib::Unadjusted;
    else if (s == "f" || s == "fol" || s == "following")
        return QuantLib::Following;
    else if (s == "mf" ||s == "modfol" || s == "modifiedfollowing")
        return QuantLib::ModifiedFollowing;
    else if (s == "p" || s == "pre" || s == "preceding")
        return QuantLib::Preceding;
    else if (s == "mp" ||s == "modpre" || s == "modifiedpreceding")
        return QuantLib::ModifiedPreceding;
    else if (s == "mer" ||s == "mendref" || s == "monthendreference")
        return QuantLib::MonthEndReference;
    else
        QL_FAIL("unknown business day convention");
}

std::string bdconvToString(BusinessDayConvention rc) {
    switch (rc) {
      case QuantLib::Unadjusted:
        return "Unadjusted";
      case QuantLib::Following:
        return "Following";
      case QuantLib::ModifiedFollowing:
        return "ModifiedFollowing";
      case QuantLib::Preceding:
        return "Preceding";
      case QuantLib::ModifiedPreceding:
        return "ModifiedPreceding";
      case QuantLib::MonthEndReference:
        return "MonthEndReference";
      default:
        QL_FAIL("unknown business day convention");
    }
}
%}

MapToString(BusinessDayConvention,bdconvFromString,bdconvToString);

// typemap joint calendar rules to corresponding strings
%{
using QuantLib::JointCalendarRule;
using QuantLib::JoinBusinessDays;

JointCalendarRule joinRuleFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "h" || s == "holidays" || s == "joinholidays")
        return QuantLib::JoinHolidays;
    else if (s == "b" ||s == "businessdays" || s == "joinbusinessdays")
        return QuantLib::JoinBusinessDays;
    else
        QL_FAIL("unknown joint calendar rule");
}

std::string joinRuleToString(JointCalendarRule jr) {
    switch (jr) {
      case QuantLib::JoinHolidays:
        return "JoinHolidays";
      case QuantLib::JoinBusinessDays:
        return "JoinBusinessDays";
      default:
        QL_FAIL("unknown joint calendar rule");
    }
}
%}

MapToString(JointCalendarRule,joinRuleFromString,joinRuleToString);


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
    %rename(">string")          __str__;
    #endif
  private:
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

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Calendar=?") Calendar_equal;
%inline %{
    bool Calendar_equal(const Calendar& c1, const Calendar& c2) {
        return c1 == c2;
    }
%}
#endif

namespace QuantLib {

    class Beijing : public Calendar {
      public:
        Beijing();
    };

    class Budapest : public Calendar {
      public:
        Budapest();
    };

    class Copenhagen : public Calendar {
      public:
        Copenhagen();
    };

    class Germany : public Calendar {
      public:
        enum Market { Settlement, FrankfurtStockExchange, Xetra, Eurex };
        Germany(Market m = FrankfurtStockExchange);
    };

    class Helsinki : public Calendar {
      public:
        Helsinki();
    };

    class HongKong : public Calendar {
      public:
        HongKong();
    };

    class Italy : public Calendar {
      public:
        enum Market { Settlement, Exchange };
        Italy(Market m = Settlement);
    };

    class Johannesburg : public Calendar {
      public:
        Johannesburg();
    };

    class NullCalendar : public Calendar {
      public:
        NullCalendar();
    };

    class Oslo : public Calendar {
      public:
        Oslo();
    };

    class Riyadh : public Calendar {
      public:
        Riyadh();
    };

    class Seoul : public Calendar {
      public:
        Seoul();
    };

    class Singapore : public Calendar {
      public:
        Singapore();
    };

    class Stockholm : public Calendar {
      public:
        Stockholm();
    };

    class Sydney : public Calendar {
      public:
        Sydney();
    };

    class TARGET : public Calendar {
      public:
        TARGET();
    };

    class Taiwan : public Calendar {
      public:
        Taiwan();
    };

    class Tokyo : public Calendar {
      public:
        Tokyo();
    };

    class Toronto : public Calendar {
      public:
        Toronto();
    };

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

    class Warsaw : public Calendar {
      public:
        Warsaw();
    };

    class Wellington : public Calendar {
      public:
        Wellington();
    };

    class Zurich : public Calendar {
      public:
        Zurich();
    };

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

