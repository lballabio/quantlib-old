
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
using QuantLib::SimpleCashFlow;
using QuantLib::FixedRateCoupon;
using QuantLib::ParCoupon;
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
            Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
            return new ParCouponHandle(
                new ParCoupon(nominal, paymentDate, libor,
                              startDate, endDate, fixingDays, spread,
                              refPeriodStart, refPeriodEnd));
        }
        Date accrualStartDate() {
            return boost::dynamic_pointer_cast<ParCoupon>(*self)
                 ->accrualStartDate();
        }
        Date accrualEndDate() {
            return boost::dynamic_pointer_cast<ParCoupon>(*self)
                 ->accrualEndDate();
        }
        double fixing() {
            return boost::dynamic_pointer_cast<ParCoupon>(*self)->fixing();
        }
        double nominal() {
            return boost::dynamic_pointer_cast<ParCoupon>(*self)->nominal();
        }
        double amount() {
            return boost::dynamic_pointer_cast<ParCoupon>(*self)->amount();
        }
    }
};

namespace std {
    %template(CashFlowVector) vector<Handle<CashFlow> >;
}

// cash flow vector builders

%{
using QuantLib::FixedRateCouponVector;
%}

std::vector<Handle<CashFlow> > 
FixedRateCouponVector(const Schedule& schedule, 
                      const std::vector<double>& nominals,
                      const std::vector<Rate>& couponRates,
                      const DayCounter& dayCount, 
                      const DayCounter& firstPeriodDayCount
                        = DayCounter());

%inline %{
std::vector<Handle<CashFlow> > 
FloatingRateCouponVector(const Schedule& schedule,
                         const std::vector<double>& nominals,
                         const XiborHandle& index, int fixingDays,
                         const std::vector<Spread>& spreads = 
                             std::vector<Spread>()) {
    Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
    return QuantLib::FloatingRateCouponVector(schedule,nominals,libor,
                                              fixingDays,spreads);
}
%}


#endif
