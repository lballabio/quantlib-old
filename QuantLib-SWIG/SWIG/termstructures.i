
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

#ifndef quantlib_term_structures_i
#define quantlib_term_structures_i

%include common.i
%include date.i
%include calendars.i
%include daycounters.i
%include types.i
%include currencies.i
%include observer.i
%include marketelements.i
%include interpolation.i

%{
using QuantLib::YieldTermStructure;
%}

%ignore YieldTermStructure;
class YieldTermStructure : public Extrapolator {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-counter")     dayCounter;
    %rename("todays-date")     todaysDate;
    %rename("reference-date")  referenceDate;
    %rename("max-date")        maxDate;
    %rename("max-time")        maxTime;
    %rename("zero-yield")      zeroYield;
    %rename("instantaneous-forward") instantaneousForward;
    #endif
  public:
    DayCounter dayCounter() const;
    Calendar calendar() const;
	Date todaysDate() const;
	Date referenceDate() const;
	Date maxDate() const;
	Time maxTime() const;
	DiscountFactor discount(const Date&, bool extrapolate = false);
	DiscountFactor discount(Time, bool extrapolate = false);
	Rate zeroYield(const Date&, bool extrapolate = false);
	Rate zeroYield(Time, bool extrapolate = false);
	Rate forward(const Date&, const Date&, bool extrapolate = false);
	Rate forward(Time, Time, bool extrapolate = false);
	Rate instantaneousForward(const Date&, bool extrapolate = false);
	Rate instantaneousForward(Time, bool extrapolate = false);
	Rate compoundForward(const Date&, Integer, bool extrapolate = false);
	Rate compoundForward(Time, Integer, bool extrapolate = false);
	Rate zeroCoupon(const Date&, Integer, bool extrapolate = false);
	Rate zeroCoupon(Time, Integer, bool extrapolate = false);
};

%template(YieldTermStructure) boost::shared_ptr<YieldTermStructure>;
IsObservable(boost::shared_ptr<YieldTermStructure>);

%template(YieldTermStructureHandle) Handle<YieldTermStructure>;
IsObservable(Handle<YieldTermStructure>);

#if defined(SWIGPYTHON)
%pythoncode %{
    TermStructure = YieldTermStructure
    TermStructureHandle = YieldTermStructureHandle
%}
#endif


// implied term structure

%{
using QuantLib::ImpliedTermStructure;
typedef boost::shared_ptr<YieldTermStructure> ImpliedTermStructurePtr;
%}

%rename(ImpliedTermStructure) ImpliedTermStructurePtr;
class ImpliedTermStructurePtr: public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        ImpliedTermStructurePtr(const Handle<YieldTermStructure>& curveHandle,
                                const Date& todaysDate,
                                const Date& referenceDate) {
            return new ImpliedTermStructurePtr(
                new ImpliedTermStructure(curveHandle, todaysDate,
                                         referenceDate));
        }
        ImpliedTermStructurePtr(const Handle<YieldTermStructure>& curveHandle,
                                const Date& referenceDate) {
            return new ImpliedTermStructurePtr(
                new ImpliedTermStructure(curveHandle, referenceDate));
        }
    }
};


// spreaded term structures

%{
using QuantLib::ZeroSpreadedTermStructure;
using QuantLib::ForwardSpreadedTermStructure;
typedef boost::shared_ptr<YieldTermStructure> ZeroSpreadedTermStructurePtr;
typedef boost::shared_ptr<YieldTermStructure> ForwardSpreadedTermStructurePtr;
%}

%rename(ZeroSpreadedTermStructure) ZeroSpreadedTermStructurePtr;
class ZeroSpreadedTermStructurePtr
    : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        ZeroSpreadedTermStructurePtr(
                                const Handle<YieldTermStructure>& curveHandle,
                                const Handle<Quote>& spreadHandle) {
	        return new ZeroSpreadedTermStructurePtr(
	            new ZeroSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};

%rename(ForwardSpreadedTermStructure) ForwardSpreadedTermStructurePtr;
class ForwardSpreadedTermStructurePtr
    : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        ForwardSpreadedTermStructurePtr(
                                const Handle<YieldTermStructure>& curveHandle,
                                const Handle<Quote>& spreadHandle) {
	        return new ForwardSpreadedTermStructurePtr(
	            new ForwardSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};


// flat forward curve

%{
using QuantLib::FlatForward;
typedef boost::shared_ptr<YieldTermStructure> FlatForwardPtr;
%}

%rename(FlatForward) FlatForwardPtr;
class FlatForwardPtr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        FlatForwardPtr(const Date& todaysDate,
                       const Date& referenceDate,
                       const Handle<Quote>& forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }
        FlatForwardPtr(const Date& todaysDate,
                       const Date& referenceDate,
                       Rate forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }

        FlatForwardPtr(const Date& referenceDate,
                       const Handle<Quote>& forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                           new FlatForward(referenceDate,forward,dayCounter));
        }
        FlatForwardPtr(const Date& referenceDate,
                       Rate forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                           new FlatForward(referenceDate,forward,dayCounter));
        }

        FlatForwardPtr(Integer settlementDays, const Calendar& calendar,
                       const Handle<Quote>& forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                 new FlatForward(settlementDays,calendar,forward,dayCounter));
        }
        FlatForwardPtr(Integer settlementDays, const Calendar& calendar,
                       Rate forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                 new FlatForward(settlementDays,calendar,forward,dayCounter));
        }
    }
};


#endif
