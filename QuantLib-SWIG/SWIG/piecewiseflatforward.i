
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
typedef boost::shared_ptr<RateHelper> DepositRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FraRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FuturesRateHelperPtr;
typedef boost::shared_ptr<RateHelper> SwapRateHelperPtr;
%}

// rate helpers for curve bootstrapping
%template(RateHelper) boost::shared_ptr<RateHelper>;

%rename(DepositRateHelper) DepositRateHelperPtr;
class DepositRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        DepositRateHelperPtr(
                const RelinkableHandle<Quote>& rate,
                Integer n, TimeUnit units, Integer settlementDays,
                const Calendar& calendar, BusinessDayConvention convention, 
                const DayCounter& dayCounter) {
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate,n,units,settlementDays,
                                      calendar, convention,dayCounter));
        }
        DepositRateHelperPtr(
                Rate rate, Integer n, TimeUnit units, Integer settlementDays,
                const Calendar& calendar, BusinessDayConvention convention, 
                const DayCounter& dayCounter) {
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate,n,units,settlementDays,
                                      calendar, convention,dayCounter));
        }
    }
};

%rename(FraRateHelper) FraRateHelperPtr;
class FraRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        FraRateHelperPtr(
                const RelinkableHandle<Quote>& rate,
                Integer monthsToStart, Integer monthsToEnd, 
                Integer settlementDays,
                const Calendar& calendar, BusinessDayConvention convention,
                const DayCounter& dayCounter) {
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  settlementDays,calendar,convention,
                                  dayCounter));
        }
        FraRateHelperPtr(
                Rate rate,
                Integer monthsToStart, Integer monthsToEnd, 
                Integer settlementDays,
                const Calendar& calendar, BusinessDayConvention convention,
                const DayCounter& dayCounter) {
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  settlementDays,calendar,convention,
                                  dayCounter));
        }
    }
};

%rename(FuturesRateHelper) FuturesRateHelperPtr;
class FuturesRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        FuturesRateHelperPtr(
                const RelinkableHandle<Quote>& price,
                const Date& immDate, Integer nMonths,
                const Calendar& calendar, BusinessDayConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,dayCounter));
        }
        FuturesRateHelperPtr(
                Real price, const Date& immDate, Integer nMonths,
                const Calendar& calendar, BusinessDayConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,dayCounter));
        }
        FuturesRateHelperPtr(
	            const RelinkableHandle<Quote>& price,
                const Date& immDate, const Date& matDate,
                const Calendar& calendar, BusinessDayConvention convention,
                const DayCounter& dayCounter) {
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,matDate,
                                      calendar,convention,dayCounter));
        }
    }
};

%rename(SwapRateHelper) SwapRateHelperPtr;
class SwapRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        SwapRateHelperPtr(
                const RelinkableHandle<Quote>& rate,
                Integer n, TimeUnit units, Integer settlementDays,
                const Calendar& calendar, 
                BusinessDayConvention rollingConvention,
                Frequency fixedFrequency, bool fixedIsAdjusted,
                const DayCounter& fixedDayCount, Frequency floatingFrequency) {
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, n, units, settlementDays,
                                   calendar, rollingConvention, 
                                   fixedFrequency, fixedIsAdjusted, 
                                   fixedDayCount, floatingFrequency));
        }
        SwapRateHelperPtr(
                Rate rate, Integer n, TimeUnit units, Integer settlementDays,
                const Calendar& calendar, 
                BusinessDayConvention rollingConvention,
                Frequency fixedFrequency, bool fixedIsAdjusted,
                const DayCounter& fixedDayCount, Frequency floatingFrequency) {
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, n, units, settlementDays,
                                   calendar, rollingConvention, 
                                   fixedFrequency, fixedIsAdjusted, 
                                   fixedDayCount, floatingFrequency));
        }
    }
};


// allow use of RateHelper vectors
namespace std {
    %template(RateHelperVector) vector<boost::shared_ptr<RateHelper> >;
}


// the curve itself

%{
using QuantLib::PiecewiseFlatForward;
typedef boost::shared_ptr<TermStructure> PiecewiseFlatForwardPtr;
%}

%rename(PiecewiseFlatForward) PiecewiseFlatForwardPtr;
class PiecewiseFlatForwardPtr : public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        PiecewiseFlatForwardPtr(
                const Date& todaysDate, 
                const Date& referenceDate, 
                const std::vector<boost::shared_ptr<RateHelper> >& instruments,
                const DayCounter& dayCounter, 
                Real accuracy = 1.0e-12) {
	        return new PiecewiseFlatForwardPtr(
	            new PiecewiseFlatForward(todaysDate, referenceDate, 
                                         instruments, dayCounter, accuracy));
        }
        PiecewiseFlatForwardPtr(
                const Date& todaysDate,
                const std::vector<Date>& dates,
                const std::vector<Rate>& forwards,
                const DayCounter& dayCounter) {
	        return new PiecewiseFlatForwardPtr(
                new PiecewiseFlatForward(todaysDate,
                                         dates, forwards, dayCounter));
        }
        const std::vector<Date>& dates() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->dates();
        }
        const std::vector<Time>& times() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->times();
        }
    }
};


#endif
