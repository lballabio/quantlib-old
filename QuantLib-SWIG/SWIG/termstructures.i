
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

// resolve overloadings
%rename(discount_vs_time)  Handle<TermStructure>::discount(Time,bool);
%rename(discount_vs_date)  Handle<TermStructure>::discount(const Date&,bool);
%rename(zeroYield_vs_time) Handle<TermStructure>::zeroYield(Time,bool);
%rename(zeroYield_vs_date) Handle<TermStructure>::zeroYield(const Date&,bool);
%rename(forward_vs_time)   Handle<TermStructure>::forward(Time,Time,bool);
%rename(forward_vs_date)   Handle<TermStructure>::forward(const Date&,
                                                          const Date&,bool);
%rename(instantaneousForward_vs_time) 
    Handle<TermStructure>::instantaneousForward(Time,bool);
%rename(instantaneousForward_vs_date) 
    Handle<TermStructure>::instantaneousForward(const Date&,bool);
#if defined(SWIGPYTHON)
%feature("shadow") Handle<TermStructure>::discount() %{
    def discount(self,x,extrapolate=0):
        if type(x) == type(0.0) or type(x) == type(0):
            return self.discount_vs_time(x,extrapolate)
        else:
            return self.discount_vs_date(x,extrapolate)
%}
%feature("shadow") Handle<TermStructure>::zeroYield() %{
    def zeroYield(self,x,extrapolate=0):
        if type(x) == type(0.0) or type(x) == type(0):
            return self.zeroYield_vs_time(x,extrapolate)
        else:
            return self.zeroYield_vs_date(x,extrapolate)
%}
%feature("shadow") Handle<TermStructure>::forward() %{
    def forward(self,x1,x2,extrapolate=0):
        if type(x1) == type(0.0) or type(x1) == type(0):
            return self.forward_vs_time(x1,x2,extrapolate)
        else:
            return self.forward_vs_date(x1,x2,extrapolate)
%}
%feature("shadow") Handle<TermStructure>::instantaneousForward() %{
    def instantaneousForward(self,x,extrapolate=0):
        if type(x) == type(0.0) or type(x) == type(0):
            return self.instantaneousForward_vs_time(x,extrapolate)
        else:
            return self.instantaneousForward_vs_date(x,extrapolate)
%}
#elif defined(SWIGGUILE)
%scheme%{
    (define (TermStructure-discount self x . extrapolate)
      (let ((method #f))
        (if (number? x)
            (set! method TermStructure-discount-vs-time)
            (set! method TermStructure-discount-vs-date))
        (apply method self x extrapolate)))
    (define (TermStructure-zero-yield self x . extrapolate)
      (let ((method #f))
        (if (number? x)
            (set! method TermStructure-zeroYield-vs-time)
            (set! method TermStructure-zeroYield-vs-date))
        (apply method self x extrapolate)))
    (define (TermStructure-forward self x1 x2 . extrapolate)
      (let ((method #f))
        (if (number? x1)
            (set! method TermStructure-forward-vs-time)
            (set! method TermStructure-forward-vs-date))
        (apply method self x1 x2 extrapolate)))
    (define (TermStructure-instantaneous-forward self x . extrapolate)
      (let ((method #f))
        (if (number? x)
            (set! method TermStructure-instantaneousForward-vs-time)
            (set! method TermStructure-instantaneousForward-vs-date))
        (apply method self x extrapolate)))
    (export TermStructure-discount
            TermStructure-zero-yield
            TermStructure-forward
            TermStructure-instantaneous-forward)
%}
#endif

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("todays-date")     Handle<TermStructure>::todaysDate;
%rename("day-counter")     Handle<TermStructure>::dayCounter;
%rename("settlement-date") Handle<TermStructure>::settlementDate;
%rename("max-date")        Handle<TermStructure>::maxDate;
%rename("max-time")        Handle<TermStructure>::maxTime;
%rename("zero-yield")      Handle<TermStructure>::zeroYield;
#endif

%template(TermStructure) Handle<TermStructure>;
IsObservable(Handle<TermStructure>);
%extend Handle<TermStructure> {
	Date todaysDate() {
		return (*self)->todaysDate();
	}
	DayCounter dayCounter() {
		return (*self)->dayCounter();
	}
	Date settlementDate() {
		return (*self)->settlementDate();
	}
	Date maxDate() {
		return (*self)->maxDate();
	}
	Time maxTime() {
		return (*self)->maxTime();
	}
	DiscountFactor discount(const Date& d, bool extrapolate = false) {
		return (*self)->discount(d, extrapolate);
	}
	DiscountFactor discount(Time t, bool extrapolate = false) {
		return (*self)->discount(t, extrapolate);
	}
	Rate zeroYield(const Date& d, bool extrapolate = false) {
		return (*self)->zeroYield(d, extrapolate);
	}
	Rate zeroYield(Time t, bool extrapolate = false) {
		return (*self)->zeroYield(t, extrapolate);
	}
	Rate forward(const Date& d1, const Date& d2, bool extrapolate = false) {
		return (*self)->forward(d1, d2, extrapolate);
	}
	Rate forward(Time t1, Time t2, bool extrapolate = false) {
		return (*self)->forward(t1, t2, extrapolate);
	}
	Rate instantaneousForward(const Date& d, bool extrapolate = false) {
		return (*self)->instantaneousForward(d, extrapolate);
	}
	Rate instantaneousForward(Time t, bool extrapolate = false) {
		return (*self)->instantaneousForward(t, extrapolate);
	}
    #if defined(SWIGPYTHON)
    // Hooks for shadow methods
    void discount() {}
    void zeroYield() {}
    void forward() {}
    void instantaneousForward() {}
    #endif
}


%template(TermStructureHandle) RelinkableHandle<TermStructure>;
IsObservable(RelinkableHandle<TermStructure>);
#if defined(SWIGGUILE)
%scheme%{
    (define TermStructureHandle-old-init new-TermStructureHandle)
    (define (new-TermStructureHandle . args)
      (let ((h (TermStructureHandle-old-init)))
        (if (not (null? args))
          (TermStructureHandle-link-to! h (car args)))
        h))
%}
#endif



// implied term structure

%{
using QuantLib::TermStructures::ImpliedTermStructure;
typedef Handle<TermStructure> ImpliedTermStructureHandle;
%}

// fake inheritance between handles
%rename(ImpliedTermStructure) ImpliedTermStructureHandle;

class ImpliedTermStructureHandle: public Handle<TermStructure> {};
%extend ImpliedTermStructureHandle {
    ImpliedTermStructureHandle(
        const RelinkableHandle<TermStructure>& curveHandle,
        const Date& todaysDate, const Date& settlementDate) {
            return new ImpliedTermStructureHandle(
                new ImpliedTermStructure(curveHandle, todaysDate,
                                         settlementDate));
    }
}


// spreaded term structures

%{
using QuantLib::TermStructures::ZeroSpreadedTermStructure;
using QuantLib::TermStructures::ForwardSpreadedTermStructure;
typedef Handle<TermStructure> ZeroSpreadedTermStructureHandle;
typedef Handle<TermStructure> ForwardSpreadedTermStructureHandle;
%}

%rename(ZeroSpreadedTermStructure) ZeroSpreadedTermStructureHandle;
%rename(ForwardSpreadedTermStructure) ForwardSpreadedTermStructureHandle;

class ZeroSpreadedTermStructureHandle : public Handle<TermStructure> {};
%extend ZeroSpreadedTermStructureHandle {
    ZeroSpreadedTermStructureHandle(
        const RelinkableHandle<TermStructure>& curveHandle,
        const RelinkableHandle<MarketElement>& spreadHandle) {
	        return new ZeroSpreadedTermStructureHandle(
	            new ZeroSpreadedTermStructure(curveHandle,spreadHandle));
    }
}

class ForwardSpreadedTermStructureHandle : public Handle<TermStructure> {};
%extend ForwardSpreadedTermStructureHandle {
    ForwardSpreadedTermStructureHandle(
        const RelinkableHandle<TermStructure>& curveHandle,
        const RelinkableHandle<MarketElement>& spreadHandle) {
	        return new ForwardSpreadedTermStructureHandle(
	            new ForwardSpreadedTermStructure(curveHandle,spreadHandle));
    }
}


// flat forward curve

%{
using QuantLib::TermStructures::FlatForward;
typedef Handle<TermStructure> FlatForwardHandle;
%}

%rename(FlatForward) FlatForwardHandle;
class FlatForwardHandle : public Handle<TermStructure> {};
%extend FlatForwardHandle {
    FlatForwardHandle(const Date& todaysDate, const Date& settlementDate, 
                      const RelinkableHandle<MarketElement>& forward,
                      const DayCounter& dayCounter) {
	    return new FlatForwardHandle(
	        new FlatForward(todaysDate, settlementDate,forward,dayCounter));
    }
}
#if defined(SWIGGUILE)
%scheme %{
    (define FlatForward-old-init new-FlatForward)
    (define (new-FlatForward today settlement forward dayCounter)
      (if (number? forward)
          (deleting-let* ((m (new-SimpleMarketElement forward)
                           delete-MarketElement)
                          (h (new-MarketElementHandle m)
                           delete-MarketElementHandle))
           (FlatForward-old-init today settlement h dayCounter))
          (FlatForward-old-init today settlement forward dayCounter)))
%}
#endif


#endif
