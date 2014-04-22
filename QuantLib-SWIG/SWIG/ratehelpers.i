
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
%include swap.i

%{
using QuantLib::RateHelper;
using QuantLib::DepositRateHelper;
using QuantLib::FraRateHelper;
using QuantLib::FuturesRateHelper;
using QuantLib::SwapRateHelper;
using QuantLib::FixedRateBondHelper;
using QuantLib::OISRateHelper;
using QuantLib::DatedOISRateHelper;
typedef boost::shared_ptr<RateHelper> DepositRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FraRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FuturesRateHelperPtr;
typedef boost::shared_ptr<RateHelper> SwapRateHelperPtr;
typedef boost::shared_ptr<RateHelper> FixedRateBondHelperPtr;
typedef boost::shared_ptr<RateHelper> OISRateHelperPtr;
typedef boost::shared_ptr<RateHelper> DatedOISRateHelperPtr;
%}

%ignore RateHelper;
class RateHelper {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("latest-date") latestDate;
    #endif
  public:
    Handle<Quote> quote() const;
    Date latestDate() const;
};

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
        DepositRateHelperPtr(const Handle<Quote>& rate,
                             const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate, libor));
        }
        DepositRateHelperPtr(Rate rate,
                             const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new DepositRateHelperPtr(
                new DepositRateHelper(rate, libor));
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
        FraRateHelperPtr(const Handle<Quote>& rate,
                         Natural monthsToStart,
                         const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,libor));
        }
        FraRateHelperPtr(Rate rate,
                         Natural monthsToStart,
                         const IborIndexPtr& index) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new FraRateHelperPtr(
                new FraRateHelper(rate,monthsToStart,libor));
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
        FuturesRateHelperPtr(
                const Handle<Quote>& price,
                const Date& immDate,
                const IborIndexPtr& index,
                const Handle<Quote>& convexityAdjustment) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,libor,
                                      convexityAdjustment));
        }
        FuturesRateHelperPtr(
                Real price,
                const Date& immDate,
                const IborIndexPtr& index,
                Real convexityAdjustment = 0.0) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new FuturesRateHelperPtr(
                new FuturesRateHelper(price,immDate,libor,
                                      convexityAdjustment));
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
                const IborIndexPtr& index,
                const Handle<Quote>& spread = Handle<Quote>(),
                const Period& fwdStart = 0*Days,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, tenor, calendar,
                                   fixedFrequency, fixedConvention,
                                   fixedDayCount, libor,
                                   spread, fwdStart,
                                   discountingCurve));
        }
        SwapRateHelperPtr(
                Rate rate,
                const Period& tenor,
                const Calendar& calendar,
                Frequency fixedFrequency,
                BusinessDayConvention fixedConvention,
                const DayCounter& fixedDayCount,
                const IborIndexPtr& index,
                const Handle<Quote>& spread = Handle<Quote>(),
                const Period& fwdStart = 0*Days,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, tenor, calendar,
                                   fixedFrequency, fixedConvention,
                                   fixedDayCount, libor,
                                   spread, fwdStart,
                                   discountingCurve));
        }
        SwapRateHelperPtr(
                const Handle<Quote>& rate,
                const SwapIndexPtr& index,
                const Handle<Quote>& spread = Handle<Quote>(),
                const Period& fwdStart = 0*Days,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<SwapIndex> swapIndex =
                boost::dynamic_pointer_cast<SwapIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, swapIndex,
                                   spread, fwdStart,
                                   discountingCurve));
        }
        SwapRateHelperPtr(
                Rate rate,
                const SwapIndexPtr& index,
                const Handle<Quote>& spread = Handle<Quote>(),
                const Period& fwdStart = 0*Days,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<SwapIndex> swapIndex =
                boost::dynamic_pointer_cast<SwapIndex>(index);
            return new SwapRateHelperPtr(
                new SwapRateHelper(rate, swapIndex,
                                   spread, fwdStart,
                                   discountingCurve));
        }
        VanillaSwapPtr swap() {
            return boost::dynamic_pointer_cast<SwapRateHelper>(*self)->swap();
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


%rename(OISRateHelper) OISRateHelperPtr;
class OISRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        OISRateHelperPtr(
                Natural settlementDays,
                const Period& tenor,
                const Handle<Quote>& rate,
                const OvernightIndexPtr& index,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<OvernightIndex> overnight =
                boost::dynamic_pointer_cast<OvernightIndex>(index);
            return new OISRateHelperPtr(
                new OISRateHelper(settlementDays,tenor,rate,
                                  overnight,discountingCurve));
        }
    }
};

%rename(DatedOISRateHelper) DatedOISRateHelperPtr;
class DatedOISRateHelperPtr : public boost::shared_ptr<RateHelper> {
  public:
    %extend {
        DatedOISRateHelperPtr(
                const Date& startDate,
                const Date& endDate,
                const Handle<Quote>& rate,
                const OvernightIndexPtr& index,
                const Handle<YieldTermStructure>& discountingCurve
                                            = Handle<YieldTermStructure>()) {
            boost::shared_ptr<OvernightIndex> overnight =
                boost::dynamic_pointer_cast<OvernightIndex>(index);
            return new DatedOISRateHelperPtr(
                new DatedOISRateHelper(startDate,endDate,rate,
                                       overnight,discountingCurve));
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
