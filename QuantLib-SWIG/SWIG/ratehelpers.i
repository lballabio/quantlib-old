
/*
 Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2009 Joseph Malicki

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

#ifndef quantlib_rate_helpers_i
#define quantlib_rate_helpers_i

%include bonds.i
%include date.i
%include calendars.i
%include daycounters.i
%include marketelements.i
%include types.i
%include vectors.i

%{
using QuantLib::RateHelper;
using QuantLib::DepositRateHelper;
using QuantLib::FraRateHelper;
using QuantLib::FuturesRateHelper;
using QuantLib::SwapRateHelper;
using QuantLib::FixedRateBondHelper;
typedef boost::shared_ptr<RateHelper> DepositRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FraRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FuturesRateHelperPtr;
typedef boost::shared_ptr<RateHelper> SwapRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FixedRateBondHelperPtr;
%}

// rate helpers for curve bootstrapping
%template(RateHelper) boost::shared_ptr<RateHelper>;

%rename(DepositRateHelper) DepositRateHelperPtr;
class DepositRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        DepositRateHelperPtr(
                const Handle<Quote>& rate,
                const Period& tenor,
                Natural fixingDays,
                const Calendar& calendar,
                BusinessDayConvention convention,
                bool endOfMonth,
                const DayCounter& dayCounter) {
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate,tenor,fixingDays,
                                      calendar,convention,
                                      endOfMonth, dayCounter));
        }
        DepositRateHelperPtr(
                Rate rate,
                const Period& tenor,
                Natural fixingDays,
                const Calendar& calendar,
                BusinessDayConvention convention,
                bool endOfMonth,
                const DayCounter& dayCounter) {
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate, tenor, fixingDays,
                                      calendar, convention,
                                      endOfMonth, dayCounter));
        }
    }
};

%rename(FraRateHelper) FraRateHelperPtr;
class FraRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        FraRateHelperPtr(
                const Handle<Quote>& rate,
                Natural monthsToStart,
                Natural monthsToEnd,
                Natural fixingDays,
                const Calendar& calendar,
                BusinessDayConvention convention,
                bool endOfMonth,
                const DayCounter& dayCounter) {
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  fixingDays,calendar,convention,
                                  endOfMonth,dayCounter));
        }
        FraRateHelperPtr(
                Rate rate,
                Natural monthsToStart,
                Natural monthsToEnd,
                Natural fixingDays,
                const Calendar& calendar,
                BusinessDayConvention convention,
                bool endOfMonth,
                const DayCounter& dayCounter) {
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,monthsToEnd,
                                  fixingDays,calendar,convention,
                                  endOfMonth,dayCounter));
        }
    }
};

%rename(FuturesRateHelper) FuturesRateHelperPtr;
class FuturesRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        FuturesRateHelperPtr(
                const Handle<Quote>& price,
                const Date& immDate,
                Natural nMonths,
                const Calendar& calendar,
                BusinessDayConvention convention,
                bool endOfMonth,
                const DayCounter& dayCounter,
                const Handle<Quote>& convexityAdjustment) {
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,endOfMonth,
                                      dayCounter,convexityAdjustment));
        }
        FuturesRateHelperPtr(
                Real price, const Date& immDate, Natural nMonths,
                const Calendar& calendar, BusinessDayConvention convention,
                bool endOfMonth, const DayCounter& dayCounter,
                Rate convexityAdjustment = 0.0) {
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,nMonths,
                                      calendar,convention,endOfMonth,
                                      dayCounter,convexityAdjustment));
        }
    }
};

%rename(SwapRateHelper) SwapRateHelperPtr;
class SwapRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        SwapRateHelperPtr(
                const Handle<Quote>& rate,
                const Period& tenor,
                const Calendar& calendar,
                Frequency fixedFrequency,
                BusinessDayConvention fixedConvention,
                const DayCounter& fixedDayCount,
                const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, tenor, calendar,
                                   fixedFrequency, fixedConvention,
                                   fixedDayCount, libor));
        }
        SwapRateHelperPtr(
                Rate rate,
                const Period& tenor,
                const Calendar& calendar,
                Frequency fixedFrequency,
                BusinessDayConvention fixedConvention,
                const DayCounter& fixedDayCount,
                const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, tenor, calendar,
                                   fixedFrequency, fixedConvention,
                                   fixedDayCount, libor));
        }
    }
};

%rename(FixedRateBondHelper) FixedRateBondHelperPtr;
class FixedRateBondHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        FixedRateBondHelperPtr(
                      const Handle<Quote>& cleanPrice,
                      Size settlementDays,
                      Real faceAmount,
                      const Schedule& schedule,
                      const std::vector<Rate>& coupons,
                      const DayCounter& paymentDayCounter,
                      BusinessDayConvention paymentConvention = Following,
                      Real redemption = 100.0,
                      const Date& issueDate = Date()) {
            return new FixedRateBondHelperPtr(
                new FixedRateBondHelper(cleanPrice, settlementDays, faceAmount,
                                        schedule, coupons, paymentDayCounter,
                                        paymentConvention, redemption,
                                        issueDate));
        }

        FixedRateBondPtr bond() {
            return FixedRateBondPtr(boost::dynamic_pointer_cast<FixedRateBondHelper>(*self)->bond());
        }
    }
};


// allow use of RateHelper vectors
#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<RateHelper> )
#endif
namespace std {
    %template(RateHelperVector) vector<boost::shared_ptr<RateHelper> >;
}


#endif
