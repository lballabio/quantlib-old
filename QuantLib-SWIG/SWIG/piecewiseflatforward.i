
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

#ifndef quantlib_piecewise_flat_forward_i
#define quantlib_piecewise_flat_forward_i

%include date.i
%include calendars.i
%include daycounters.i
%include cashflows.i
%include marketelements.i
%include termstructures.i
%include types.i
%include vectors.i

%{
using QuantLib::RateHelper;
using QuantLib::DepositRateHelper;
using QuantLib::FraRateHelper;
using QuantLib::FuturesRateHelper;
using QuantLib::SwapRateHelper;
typedef Handle<RateHelper> DepositRateHelperHandle;
typedef Handle<RateHelper> FraRateHelperHandle;
typedef Handle<RateHelper> FuturesRateHelperHandle;
typedef Handle<RateHelper> SwapRateHelperHandle;
%}

// rate helpers for curve bootstrapping
%template(RateHelper) Handle<RateHelper>;

%rename(DepositRateHelper) DepositRateHelperHandle;
class DepositRateHelperHandle : public Handle<RateHelper> {
  public:
    %extend {
        DepositRateHelperHandle(
                const RelinkableHandle<Quote>& rate,
                int n, TimeUnit units, int settlementDays,
                const Calendar& calendar, RollingConvention convention, 
                const DayCounter& dayCounter) {
            return new DepositRateHelperHandle(
                new DepositRateHelper(rate,n,units,settlementDays,
                                      calendar, convention,dayCounter));
        }
        DepositRateHelperHandle(
                double rate, int n, TimeUnit units, int settlementDays,
                const Calendar& calendar, RollingConvention convention, 
                const DayCounter& dayCounter) {
            return new DepositRateHelperHandle(
                new DepositRateHelper(rate,n,units,settlementDays,
                                      calendar, convention,dayCounter));
        }
    }
};

%rename(FraRateHelper) FraRateHelperHandle;
class FraRateHelperHandle : public Handle<RateHelper> {
  public:
    %extend {
        FraRateHelperHandle(
                const RelinkableHandle<Quote>& rate,
                int monthsToStart, int monthsToEnd, int settlementDays,
                const Calendar& calendar, RollingConvention convention,
                const DayCounter& dayCounter) {
            return new FraRateHelperHandle(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  settlementDays,calendar,convention,
                                  dayCounter));
        }
        FraRateHelperHandle(
                double rate,
                int monthsToStart, int monthsToEnd, int settlementDays,
                const Calendar& calendar, RollingConvention convention,
                const DayCounter& dayCounter) {
            return new FraRateHelperHandle(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  settlementDays,calendar,convention,
                                  dayCounter));
        }
    }
};

%rename(FuturesRateHelper) FuturesRateHelperHandle;
class FuturesRateHelperHandle : public Handle<RateHelper> {
  public:
    %extend {
        FuturesRateHelperHandle(
                const RelinkableHandle<Quote>& price,
                const Date& immDate, int nMonths,
                const Calendar& calendar, RollingConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperHandle(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,dayCounter));
        }
        FuturesRateHelperHandle(
                double price, const Date& immDate, int nMonths,
                const Calendar& calendar, RollingConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperHandle(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,dayCounter));
        }
        FuturesRateHelperHandle(
	            const RelinkableHandle<Quote>& price,
                const Date& immDate, const Date& matDate,
                const Calendar& calendar, RollingConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperHandle(
                new FuturesRateHelper(price,immDate,matDate,
                                      calendar,convention,dayCounter));
        }
    }
};

%rename(SwapRateHelper) SwapRateHelperHandle;
class SwapRateHelperHandle : public Handle<RateHelper> {
  public:
    %extend {
        SwapRateHelperHandle(
                const RelinkableHandle<Quote>& rate,
                int n, TimeUnit units, int settlementDays,
                const Calendar& calendar, RollingConvention rollingConvention,
                int fixedFrequency, bool fixedIsAdjusted,
                const DayCounter& fixedDayCount, int floatingFrequency) {
            return new SwapRateHelperHandle(
                new SwapRateHelper(rate, n, units, settlementDays,
                                   calendar, rollingConvention, 
                                   fixedFrequency, fixedIsAdjusted, 
                                   fixedDayCount, floatingFrequency));
        }
        SwapRateHelperHandle(
                double rate, int n, TimeUnit units, int settlementDays,
                const Calendar& calendar, RollingConvention rollingConvention,
                int fixedFrequency, bool fixedIsAdjusted,
                const DayCounter& fixedDayCount, int floatingFrequency) {
            return new SwapRateHelperHandle(
                new SwapRateHelper(rate, n, units, settlementDays,
                                   calendar, rollingConvention, 
                                   fixedFrequency, fixedIsAdjusted, 
                                   fixedDayCount, floatingFrequency));
        }
    }
};


// allow use of RateHelper vectors
namespace std {
    %template(RateHelperVector) vector<Handle<RateHelper> >;
}


// the curve itself

%{
using QuantLib::PiecewiseFlatForward;
typedef Handle<TermStructure> PiecewiseFlatForwardHandle;
%}

%rename(PiecewiseFlatForward) PiecewiseFlatForwardHandle;
class PiecewiseFlatForwardHandle : public Handle<TermStructure> {
  public:
    %extend {
        PiecewiseFlatForwardHandle(
                const Date& todaysDate, 
                const Date& referenceDate, 
                const std::vector<Handle<RateHelper> >& instruments,
                const DayCounter& dayCounter, 
                double accuracy = 1.0e-12) {
	        return new PiecewiseFlatForwardHandle(
	            new PiecewiseFlatForward(todaysDate, referenceDate, 
                                         instruments, dayCounter, accuracy));
        }
        PiecewiseFlatForwardHandle(
                const Date& todaysDate,
                const std::vector<Date>& dates,
                const std::vector<double>& forwards,
                const DayCounter& dayCounter) {
	        return new PiecewiseFlatForwardHandle(
                new PiecewiseFlatForward(todaysDate,
                                         dates, forwards, dayCounter));
        }
        const std::vector<Date>& dates() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->dates();
        }
        const std::vector<double>& times() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->times();
        }
    }
};


#endif
