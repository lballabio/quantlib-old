
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
using QuantLib::TermStructures::RateHelper;
using QuantLib::TermStructures::DepositRateHelper;
using QuantLib::TermStructures::FraRateHelper;
using QuantLib::TermStructures::FuturesRateHelper;
using QuantLib::TermStructures::SwapRateHelper;
typedef Handle<RateHelper> DepositRateHelperHandle;
typedef Handle<RateHelper> FraRateHelperHandle;
typedef Handle<RateHelper> FuturesRateHelperHandle;
typedef Handle<RateHelper> SwapRateHelperHandle;
%}

// rate helpers for curve bootstrapping
%template(RateHelper) Handle<RateHelper>;
ReturnByValue(Handle<RateHelper>);

%rename(DepositRateHelper) DepositRateHelperHandle;
class DepositRateHelperHandle : public Handle<RateHelper> {};
%extend DepositRateHelperHandle {
    DepositRateHelperHandle(
        const RelinkableHandle<MarketElement>& rate,
        int settlementDays, int n, TimeUnit units, 
        const Calendar& calendar, RollingConvention convention, 
        const DayCounter& dayCounter) {
            return new DepositRateHelperHandle(
                new DepositRateHelper(rate,settlementDays,n,units,calendar,
                    convention,dayCounter));
    }
}

%rename(FraRateHelper) FraRateHelperHandle;
class FraRateHelperHandle : public Handle<RateHelper> {};
%extend FraRateHelperHandle {
    FraRateHelperHandle(
        const RelinkableHandle<MarketElement>& rate,
        int settlementDays, int monthsToStart, int monthsToEnd,
        const Calendar& calendar, RollingConvention convention,
        const DayCounter& dayCounter) {
            return new FraRateHelperHandle(
                new FraRateHelper(rate,settlementDays,monthsToStart,
                    monthsToEnd,calendar,convention,dayCounter));
    }
}

%rename(FuturesRateHelper) FuturesRateHelperHandle;
class FuturesRateHelperHandle : public Handle<RateHelper> {};
%extend FuturesRateHelperHandle {
    FuturesRateHelperHandle(
        const RelinkableHandle<MarketElement>& price,
        const Date& immDate, int settlementDays, int nMonths,
        const Calendar& calendar, RollingConvention convention,
        const DayCounter& dayCounter) {
            return new FuturesRateHelperHandle(
                new FuturesRateHelper(price,immDate,settlementDays,nMonths,
                    calendar,convention,dayCounter));
    }
}

%rename(SwapRateHelper) SwapRateHelperHandle;
class SwapRateHelperHandle : public Handle<RateHelper> {};
%extend SwapRateHelperHandle {
    SwapRateHelperHandle(
        const RelinkableHandle<MarketElement>& rate,
        int settlementDays, int lengthInYears, 
        const Calendar& calendar, RollingConvention rollingConvention,
        int fixedFrequency, bool fixedIsAdjusted,
        const DayCounter& fixedDayCount, int floatingFrequency) {
            return new SwapRateHelperHandle(
                new SwapRateHelper(rate, settlementDays, lengthInYears,
                    calendar, rollingConvention, fixedFrequency,
                    fixedIsAdjusted, fixedDayCount, floatingFrequency));
    }
}


// allow use of RateHelper vectors
namespace std {
    %template(RateHelperVector) vector<Handle<RateHelper> >;
}


// the curve itself

%{
using QuantLib::TermStructures::PiecewiseFlatForward;
typedef Handle<TermStructure> PiecewiseFlatForwardHandle;
%}

%rename(PiecewiseFlatForward) PiecewiseFlatForwardHandle;
class PiecewiseFlatForwardHandle : public Handle<TermStructure> {};
%extend PiecewiseFlatForwardHandle {
    PiecewiseFlatForwardHandle(
        const Date& settlementDate, 
        const std::vector<Handle<RateHelper> >& instruments,
        const DayCounter& dayCounter, 
        double accuracy = 1.0e-12) {
	        return new PiecewiseFlatForwardHandle(
	            new PiecewiseFlatForward(settlementDate, instruments, 
                                         dayCounter, accuracy));
    }
    const std::vector<Date>& dates() {
        return Handle<PiecewiseFlatForward>(*self)->dates();
    }
    const std::vector<double>& times() {
        return Handle<PiecewiseFlatForward>(*self)->times();
    }
}


#endif
