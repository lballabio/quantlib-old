
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

%template(TermStructure) boost::shared_ptr<TermStructure>;
IsObservable(boost::shared_ptr<TermStructure>);


%template(TermStructureHandle) RelinkableHandle<TermStructure>;
IsObservable(RelinkableHandle<TermStructure>);


// implied term structure

%{
using QuantLib::ImpliedTermStructure;
typedef boost::shared_ptr<TermStructure> ImpliedTermStructurePtr;
%}

%rename(ImpliedTermStructure) ImpliedTermStructurePtr;
class ImpliedTermStructurePtr: public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        ImpliedTermStructurePtr(
                const RelinkableHandle<TermStructure>& curveHandle,
                const Date& todaysDate, const Date& referenceDate) {
            return new ImpliedTermStructurePtr(
                new ImpliedTermStructure(curveHandle, todaysDate, 
                                         referenceDate));
        }
    }
};


// spreaded term structures

%{
using QuantLib::ZeroSpreadedTermStructure;
using QuantLib::ForwardSpreadedTermStructure;
typedef boost::shared_ptr<TermStructure> ZeroSpreadedTermStructurePtr;
typedef boost::shared_ptr<TermStructure> ForwardSpreadedTermStructurePtr;
%}

%rename(ZeroSpreadedTermStructure) ZeroSpreadedTermStructurePtr;
class ZeroSpreadedTermStructurePtr : public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        ZeroSpreadedTermStructurePtr(
                const RelinkableHandle<TermStructure>& curveHandle,
                const RelinkableHandle<Quote>& spreadHandle) {
	        return new ZeroSpreadedTermStructurePtr(
	            new ZeroSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};

%rename(ForwardSpreadedTermStructure) ForwardSpreadedTermStructurePtr;
class ForwardSpreadedTermStructurePtr 
    : public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        ForwardSpreadedTermStructurePtr(
                const RelinkableHandle<TermStructure>& curveHandle,
                const RelinkableHandle<Quote>& spreadHandle) {
	        return new ForwardSpreadedTermStructurePtr(
	            new ForwardSpreadedTermStructure(curveHandle,spreadHandle));
        }
    }
};


// flat forward curve

%{
using QuantLib::FlatForward;
typedef boost::shared_ptr<TermStructure> FlatForwardPtr;
%}

%rename(FlatForward) FlatForwardPtr;
class FlatForwardPtr : public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        FlatForwardPtr(const Date& todaysDate, 
                       const Date& referenceDate, 
                       const RelinkableHandle<Quote>& forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }
        FlatForwardPtr(const Date& todaysDate, 
                       const Date& referenceDate, 
                       double forward,
                       const DayCounter& dayCounter) {
            return new FlatForwardPtr(
                new FlatForward(todaysDate,referenceDate,forward,dayCounter));
        }
    }
};


#endif
