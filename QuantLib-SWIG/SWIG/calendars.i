
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
using QuantLib::Beijing;
using QuantLib::Budapest;
using QuantLib::Copenhagen;
using QuantLib::Germany;
using QuantLib::Helsinki;
using QuantLib::HongKong;
using QuantLib::Johannesburg;
using QuantLib::JointCalendar;
using QuantLib::Italy;
using QuantLib::NullCalendar;
using QuantLib::Oslo;
using QuantLib::Riyadh;
using QuantLib::Seoul;
using QuantLib::Singapore;
using QuantLib::Stockholm;
using QuantLib::Sydney;
using QuantLib::TARGET;
using QuantLib::Taiwan;
using QuantLib::Tokyo;
using QuantLib::Toronto;
using QuantLib::UnitedKingdom;
using QuantLib::UnitedStates;
using QuantLib::Warsaw;
using QuantLib::Wellington;
using QuantLib::Zurich;
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
        Calendar(const std::string& name) {
            std::string s = StringFormatter::toLowercase(name);
            if (s == "target" || s == "euro" || s == "eur")
                return new Calendar(TARGET());
            else if (s == "unitedstates" || s == "us")
                return new Calendar(UnitedStates(UnitedStates::Settlement));
            else if (s == "usexchange" || s == "nyexchange" || s == "nyse")
                return new Calendar(UnitedStates(UnitedStates::Exchange));
            else if (s == "usbonds" || s == "usbond")
                return new Calendar(
                                UnitedStates(UnitedStates::GovernmentBond));
            else if (s == "unitedkingdom" || s == "uk")
                return new Calendar(UnitedKingdom(UnitedKingdom::Settlement));
            else if (s == "ukexchange" || s == "londonexchange")
                return new Calendar(UnitedKingdom(UnitedKingdom::Exchange));
            else if (s == "ukmetals" || s == "londonmetals")
                return new Calendar(UnitedKingdom(UnitedKingdom::Metals));
            else if (s == "beijing")
                return new Calendar(Beijing());
            else if (s == "budapest")
                return new Calendar(Budapest());
            else if (s == "copenhagen")
                return new Calendar(Copenhagen());
            else if (s == "germany")
                return new Calendar(Germany(Germany::Settlement));
            else if (s == "germanyexchange")
                return new Calendar(Germany(Germany::FrankfurtStockExchange));
            else if (s == "germanyxetra" || s == "xetra")
                return new Calendar(Germany(Germany::Xetra));
            else if (s == "germanyeurex" || s == "eurex")
                return new Calendar(Germany(Germany::Eurex));
            else if (s == "helsinki")
                return new Calendar(Helsinki());
            else if (s == "hongkong")
                return new Calendar(HongKong());
            else if (s == "italy")
                return new Calendar(Italy(Italy::Settlement));
            else if (s == "italyexchange" || s == "milanexchange")
                return new Calendar(Italy(Italy::Exchange));
            else if (s == "johannesburg" || s == "jhb")
                return new Calendar(Johannesburg());
            else if (s == "oslo")
                return new Calendar(Oslo());
            else if (s == "riyadh")
                return new Calendar(Riyadh());
            else if (s == "seoul")
                return new Calendar(Seoul());
            else if (s == "singapore")
                return new Calendar(Singapore());
            else if (s == "stockholm")
                return new Calendar(Stockholm());
            else if (s == "sydney")
                return new Calendar(Sydney());
            else if (s == "taiwan")
                return new Calendar(Taiwan());
            else if (s == "tokyo")
                return new Calendar(Tokyo());
            else if (s == "toronto")
                return new Calendar(Toronto());
            else if (s == "warsaw")
                return new Calendar(Warsaw());
            else if (s == "wellington")
                return new Calendar(Wellington());
            else if (s == "zurich" || s == "zur")
                return new Calendar(Zurich());
            else if (s == "null")
                return new Calendar(NullCalendar());
            else
                QL_FAIL("Unknown calendar: " + name);
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

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Calendar=?") Calendar_equal;
%inline %{
    bool Calendar_equal(const Calendar& c1, const Calendar& c2) {
        return c1 == c2;
    }
%}
#endif


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


#endif

