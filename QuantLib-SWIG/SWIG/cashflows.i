
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

#ifndef quantlib_cash_flows_i
#define quantlib_cash_flows_i

%include date.i
%include types.i
%include calendars.i
%include daycounters.i
%include indexes.i
%include termstructures.i
%include vectors.i

%{
using QuantLib::CashFlow;
%}

%template(CashFlow) Handle<CashFlow>;
%extend Handle<CashFlow> {
    double amount() {
        return (*self)->amount();
    }
    Date date() {
        return (*self)->date();
    }
}
IsObservable(Handle<CashFlow>);
ReturnByValue(Handle<CashFlow>);


// implementations

%{
using QuantLib::CashFlows::SimpleCashFlow;
using QuantLib::CashFlows::FixedRateCoupon;
using QuantLib::CashFlows::FloatingRateCoupon;
typedef Handle<CashFlow> SimpleCashFlowHandle;
typedef Handle<CashFlow> FixedRateCouponHandle;
typedef Handle<CashFlow> FloatingRateCouponHandle;
%}

%rename(SimpleCashFlow) SimpleCashFlowHandle;
class SimpleCashFlowHandle : public Handle<CashFlow> {};
%extend SimpleCashFlowHandle {
    SimpleCashFlowHandle(double amount, const Date& date) {
        return new SimpleCashFlowHandle(
            new SimpleCashFlow(amount,date));
    }
}

%rename(FixedRateCoupon) FixedRateCouponHandle;
class FixedRateCouponHandle : public Handle<CashFlow> {};
%extend FixedRateCouponHandle {
    FixedRateCouponHandle(double nominal, const Date& paymentDate, 
                          Rate rate, const DayCounter& dayCounter, 
                          const Date& startDate, const Date& endDate,
                          const Date& refPeriodStart = Date(), 
                          const Date& refPeriodEnd = Date()) {
        return new FixedRateCouponHandle(
            new FixedRateCoupon(nominal, paymentDate, rate, 
                dayCounter, startDate, endDate, refPeriodStart,
                refPeriodEnd));
    }
}

%rename(FloatingRateCoupon) FloatingRateCouponHandle;
class FloatingRateCouponHandle : public Handle<CashFlow> {};
%extend FloatingRateCouponHandle {
    FloatingRateCouponHandle(double nominal, const Date& paymentDate, 
                             const XiborHandle& index, const Date& startDate, 
                             const Date& endDate, int fixingDays, 
                             Spread spread = 0.0, 
                             const Date& refPeriodStart = Date(), 
                             const Date& refPeriodEnd = Date()) {
        return new FloatingRateCouponHandle(
            new FloatingRateCoupon(nominal, paymentDate, index,
                startDate, endDate, fixingDays, spread,
                refPeriodStart, refPeriodEnd));
    }
}

namespace std {
    %template(CashFlowVector) vector<Handle<CashFlow> >;
}

// cash flow vector builders

%{
using QuantLib::CashFlows::FixedRateCouponVector;
using QuantLib::CashFlows::FloatingRateCouponVector;
%}

std::vector<Handle<CashFlow> > FixedRateCouponVector(
    const std::vector<double>& nominals, 
    const std::vector<double>& couponRates,
    const Date& startDate, const Date& endDate, int frequency,
    const Calendar& calendar, RollingConvention convention,
    bool isAdjusted, const DayCounter& dayCount,
    const DayCounter& firstPeriodDayCount, 
    const Date& stubDate = Date());

std::vector<Handle<CashFlow> > FloatingRateCouponVector(
    const std::vector<double>& nominals,
    const Date& startDate, const Date& endDate,
    int frequency, const Calendar& calendar,
    RollingConvention convention,
    const XiborHandle& index, int indexFixingDays, 
    const std::vector<double>& spreads, 
    const Date& stubDate = Date());


#endif
