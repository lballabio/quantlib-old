
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_cash_flows_i
#define quantlib_cash_flows_i

%include date.i
%include types.i
%include calendars.i
%include daycounters.i
%include indexes.i
%include termstructures.i
%include scheduler.i
%include vectors.i

%{
using QuantLib::CashFlow;
%}

%ignore CashFlow;
class CashFlow {
  public:
    double amount() const;
    Date date() const;
};

%template(CashFlow) Handle<CashFlow>;
IsObservable(Handle<CashFlow>);


// implementations

%{
using QuantLib::CashFlows::SimpleCashFlow;
using QuantLib::CashFlows::FixedRateCoupon;
using QuantLib::CashFlows::ParCoupon;
typedef Handle<CashFlow> SimpleCashFlowHandle;
typedef Handle<CashFlow> FixedRateCouponHandle;
typedef Handle<CashFlow> ParCouponHandle;
%}

%rename(SimpleCashFlow) SimpleCashFlowHandle;
class SimpleCashFlowHandle : public Handle<CashFlow> {
  public:
    %extend {
        SimpleCashFlowHandle(double amount, const Date& date) {
            return new SimpleCashFlowHandle(
                new SimpleCashFlow(amount,date));
        }
    }
};

%rename(FixedRateCoupon) FixedRateCouponHandle;
class FixedRateCouponHandle : public Handle<CashFlow> {
  public:
    %extend {
        FixedRateCouponHandle(double nominal, const Date& paymentDate, 
                              Rate rate, const DayCounter& dayCounter, 
                              const Date& startDate, const Date& endDate,
                              const Date& refPeriodStart = Date(), 
                              const Date& refPeriodEnd = Date()) {
            return new FixedRateCouponHandle(
                new FixedRateCoupon(nominal, paymentDate, rate, 
                                    dayCounter, startDate, endDate, 
                                    refPeriodStart, refPeriodEnd));
        }
    }
};

%rename(ParCoupon) ParCouponHandle;
class ParCouponHandle : public Handle<CashFlow> {
  public:
    %extend {
        ParCouponHandle(double nominal, const Date& paymentDate, 
                        const XiborHandle& index, 
                        const Date& startDate, const Date& endDate, 
                        int fixingDays, Spread spread = 0.0, 
                        const Date& refPeriodStart = Date(), 
                        const Date& refPeriodEnd = Date()) {
            return new ParCouponHandle(
                new ParCoupon(nominal, paymentDate, index,
                              startDate, endDate, fixingDays, spread,
                              refPeriodStart, refPeriodEnd));
        }
	Date accrualStartDate() {
	   return Handle<ParCoupon>(*self)->accrualStartDate();
	}
	Date accrualEndDate() {
	   return Handle<ParCoupon>(*self)->accrualEndDate();
	}
	double fixing() {
	   return Handle<ParCoupon>(*self)->fixing();
	}
	double nominal() {
	   return Handle<ParCoupon>(*self)->nominal();
	}
	double amount() {
	   return Handle<ParCoupon>(*self)->amount();
	}
    }
};

namespace std {
    %template(CashFlowVector) vector<Handle<CashFlow> >;
}

// cash flow vector builders

%{
using QuantLib::CashFlows::FixedRateCouponVector;
using QuantLib::CashFlows::FloatingRateCouponVector;
%}

std::vector<Handle<CashFlow> > 
FixedRateCouponVector(const Schedule& schedule, 
                      const std::vector<double>& nominals,
                      const std::vector<Rate>& couponRates,
                      const DayCounter& dayCount, 
                      const DayCounter& firstPeriodDayCount
                        = DayCounter());

// deprecated
std::vector<Handle<CashFlow> > FixedRateCouponVector(
    const std::vector<double>& nominals, 
    const std::vector<double>& couponRates,
    const Date& startDate, const Date& endDate, int frequency,
    const Calendar& calendar, RollingConvention convention,
    bool isAdjusted, const DayCounter& dayCount,
    const DayCounter& firstPeriodDayCount, 
    const Date& stubDate = Date());

// deprecated
std::vector<Handle<CashFlow> > FixedRateCouponVector(
    const std::vector<double>& nominals,
    const std::vector<Rate>& couponRates,
    const DayCounter& dayCount, const DayCounter& firstPeriodDayCount,
    const Schedule& schedule);



std::vector<Handle<CashFlow> > 
FloatingRateCouponVector(const Schedule& schedule,
                         const std::vector<double>& nominals,
                         const XiborHandle& index, int fixingDays,
                         const std::vector<Spread>& spreads = 
                             std::vector<Spread>());

// deprecated
std::vector<Handle<CashFlow> > FloatingRateCouponVector(
    const std::vector<double>& nominals,
    const Date& startDate, const Date& endDate,
    int frequency, const Calendar& calendar,
    RollingConvention convention,
    const XiborHandle& index, int indexFixingDays, 
    const std::vector<double>& spreads = std::vector<double>(), 
    const Date& stubDate = Date());

// deprecated
std::vector<Handle<CashFlow> > FloatingRateCouponVector(
    const std::vector<double>& nominals,
    const XiborHandle& index, int fixingDays,
    const std::vector<Spread>& spreads,
    const Schedule& schedule);


#endif
