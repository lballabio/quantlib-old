
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
%include daycounters.i
%include types.i
%include currencies.i
%include observer.i
%include marketelements.i

%{
using QuantLib::TermStructure;
%}

%ignore TermStructure;
class TermStructure {
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
	Rate compoundForward(const Date&, int, bool extrapolate = false);
	Rate compoundForward(Time, int, bool extrapolate = false);
	Rate zeroCoupon(const Date&, int, bool extrapolate = false);
	Rate zeroCoupon(Time, int, bool extrapolate = false);
};

%template(TermStructure) Handle<TermStructure>;
IsObservable(Handle<TermStructure>);


%template(TermStructureHandle) RelinkableHandle<TermStructure>;
IsObservable(RelinkableHandle<TermStructure>);


// implied term structure

%{
using QuantLib::ImpliedTermStructure;
typedef Handle<TermStructure> ImpliedTermStructureHandle;
%}

// fake inheritance between handles
%rename(ImpliedTermStructure) ImpliedTermStructureHandle;
class ImpliedTermStructureHandle: public Handle<TermStructure> {
  public:
    %extend {
        ImpliedTermStructureHandle(
                const RelinkableHandle<TermStructure>& curveHandle,
                const Date& todaysDate, const Date& referenceDate) {
            return new ImpliedTermStructureHandle(
                new ImpliedTermStructure(curveHandle, todaysDate, 
                                         referenceDate));
        }
    }
};


// spreaded term structures

%{
using QuantLib::ZeroSpreadedTermStructure;
using QuantLib::ForwardSpreadedTermStructure;
typedef Handle<TermStructure> ZeroSpreadedTermStructureHandle;
typedef Handle<TermStructure> ForwardSpreadedTermStructureHandle;
%}

%rename(ZeroSpreadedTermStructure) ZeroSpreadedTermStructureHandle;
class ZeroSpreadedTermStructureHandle : public Handle<TermStructure> {
  public:
    %extend {
        ZeroSpreadedTermStructureHandle(
                const RelinkableHandle<TermStructure>& curveHandle,
                const RelinkableHandle<MarketElement>& spreadHandle) {
	        return new ZeroSpreadedTermStructureHandle(
	            new ZeroSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};

%rename(ForwardSpreadedTermStructure) ForwardSpreadedTermStructureHandle;
class ForwardSpreadedTermStructureHandle : public Handle<TermStructure> {
  public:
    %extend {
        ForwardSpreadedTermStructureHandle(
                const RelinkableHandle<TermStructure>& curveHandle,
                const RelinkableHandle<MarketElement>& spreadHandle) {
	        return new ForwardSpreadedTermStructureHandle(
	            new ForwardSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};


// flat forward curve

%{
using QuantLib::FlatForward;
typedef Handle<TermStructure> FlatForwardHandle;
%}

%rename(FlatForward) FlatForwardHandle;
class FlatForwardHandle : public Handle<TermStructure> {
  public:
    %extend {
        FlatForwardHandle(const Date& todaysDate, 
                          const Date& referenceDate, 
                          const RelinkableHandle<MarketElement>& forward,
                          const DayCounter& dayCounter) {
            return new FlatForwardHandle(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }
        FlatForwardHandle(const Date& todaysDate, 
                          const Date& referenceDate, 
                          double forward,
                          const DayCounter& dayCounter) {
            return new FlatForwardHandle(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }
    }
};


#endif
