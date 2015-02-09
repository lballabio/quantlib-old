
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2014 StatPro Italia srl

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

#ifndef quantlib_term_structures_i
#define quantlib_term_structures_i

%include common.i
%include types.i
%include interestrate.i
%include date.i
%include calendars.i
%include daycounters.i
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
    %rename("reference-date")  referenceDate;
    %rename("max-date")        maxDate;
    %rename("max-time")        maxTime;
    %rename("zero-rate")       zeroRate;
    %rename("forward-rate")    forwardRate;
    #endif
  public:
    DayCounter dayCounter() const;
    Calendar calendar() const;
    Date referenceDate() const;
    Date maxDate() const;
    Time maxTime() const;
    DiscountFactor discount(const Date&, bool extrapolate = false);
    DiscountFactor discount(Time, bool extrapolate = false);
    InterestRate zeroRate(const Date& d,
                          const DayCounter&, Compounding, Frequency f = Annual,
                          bool extrapolate = false) const;
    InterestRate zeroRate(Time t,
                          Compounding, Frequency f = Annual,
                          bool extrapolate = false) const;
    InterestRate forwardRate(const Date& d1, const Date& d2,
                             const DayCounter&, Compounding,
                             Frequency f = Annual,
                             bool extrapolate = false) const;
    InterestRate forwardRate(Time t1, Time t2,
                             Compounding, Frequency f = Annual,
                             bool extrapolate = false) const;
};

%template(YieldTermStructure) boost::shared_ptr<YieldTermStructure>;
IsObservable(boost::shared_ptr<YieldTermStructure>);

%template(YieldTermStructureHandle) Handle<YieldTermStructure>;
IsObservable(Handle<YieldTermStructure>);
%template(RelinkableYieldTermStructureHandle)
RelinkableHandle<YieldTermStructure>;


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

%{
using QuantLib::InterpolatedPiecewiseZeroSpreadedTermStructure;
%}

%define export_piecewise_zero_spreaded_term_structure(Name,Interpolator)

%fragment("Name","header") {
typedef boost::shared_ptr<YieldTermStructure> Name##Ptr;
}
%fragment("Name");

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        Name##Ptr(
                const Handle<YieldTermStructure>& curveHandle,
                const std::vector< Handle<Quote> >& spreadHandles,
                const std::vector<Date>& dates) {
            return new Name##Ptr(
                new InterpolatedPiecewiseZeroSpreadedTermStructure<Interpolator>(
                          curveHandle,spreadHandles,dates));
        }
    }
};

%enddef

export_piecewise_zero_spreaded_term_structure(SpreadedLinearZeroInterpolatedTermStructure,Linear);


// flat forward curve

%{
using QuantLib::FlatForward;
typedef boost::shared_ptr<YieldTermStructure> FlatForwardPtr;
%}

%rename(FlatForward) FlatForwardPtr;
class FlatForwardPtr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        FlatForwardPtr(const Date& referenceDate,
                       const Handle<Quote>& forward,
                       const DayCounter& dayCounter,
                       Compounding compounding = QuantLib::Continuous,
                       Frequency frequency = QuantLib::Annual) {
            return new FlatForwardPtr(
                           new FlatForward(referenceDate,forward,dayCounter,
                                           compounding,frequency));
        }
        FlatForwardPtr(const Date& referenceDate,
                       Rate forward,
                       const DayCounter& dayCounter,
                       Compounding compounding = QuantLib::Continuous,
                       Frequency frequency = QuantLib::Annual) {
            return new FlatForwardPtr(
                           new FlatForward(referenceDate,forward,dayCounter,
                                           compounding,frequency));
        }
        FlatForwardPtr(Integer settlementDays, const Calendar& calendar,
                       const Handle<Quote>& forward,
                       const DayCounter& dayCounter,
                       Compounding compounding = QuantLib::Continuous,
                       Frequency frequency = QuantLib::Annual) {
            return new FlatForwardPtr(
                 new FlatForward(settlementDays,calendar,forward,dayCounter,
                                 compounding,frequency));
        }
        FlatForwardPtr(Integer settlementDays, const Calendar& calendar,
                       Rate forward,
                       const DayCounter& dayCounter,
                       Compounding compounding = QuantLib::Continuous,
                       Frequency frequency = QuantLib::Annual) {
            return new FlatForwardPtr(
                 new FlatForward(settlementDays,calendar,forward,dayCounter,
                                 compounding,frequency));
        }
    }
};


#endif
