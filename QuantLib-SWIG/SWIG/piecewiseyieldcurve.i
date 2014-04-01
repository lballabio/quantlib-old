
/*
 Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl

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

#ifndef quantlib_piecewise_yield_curve_i
#define quantlib_piecewise_yield_curve_i

%include termstructures.i
%include ratehelpers.i
%include interpolation.i

#if !defined(VC6)

// bootstrap traits

%{
using QuantLib::Discount;
using QuantLib::ZeroYield;
using QuantLib::ForwardRate;
%}

struct Discount {};
struct ZeroYield {};
struct ForwardRate {};

// curve

%{
using QuantLib::PiecewiseYieldCurve;
%}

%define export_piecewise_curve(Name,Base,Interpolator)

%{
typedef boost::shared_ptr<YieldTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        Name##Ptr(
                const Date& referenceDate,
                const std::vector<boost::shared_ptr<RateHelper> >& instruments,
                const DayCounter& dayCounter,
                const std::vector<Handle<Quote> >& jumps =
                                                std::vector<Handle<Quote> >(),
                const std::vector<Date>& jumpDates = std::vector<Date>(),
                Real accuracy = 1.0e-12,
                const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseYieldCurve<Base,Interpolator>(
                                                 referenceDate,instruments,
                                                 dayCounter, jumps, jumpDates,
                                                 accuracy,i));
        }
        Name##Ptr(
                Integer settlementDays, const Calendar& calendar,
                const std::vector<boost::shared_ptr<RateHelper> >& instruments,
                const DayCounter& dayCounter,
                const std::vector<Handle<Quote> >& jumps =
                                                std::vector<Handle<Quote> >(),
                const std::vector<Date>& jumpDates = std::vector<Date>(),
                Real accuracy = 1.0e-12,
                const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseYieldCurve<Base,Interpolator>(
                                        settlementDays, calendar, instruments,
                                        dayCounter, jumps, jumpDates,
                                        accuracy, i));
        }
        const std::vector<Date>& dates() {
            typedef PiecewiseYieldCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Time>& times() {
            typedef PiecewiseYieldCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->times();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef PiecewiseYieldCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef


export_piecewise_curve(PiecewiseFlatForward,ForwardRate,BackwardFlat);
export_piecewise_curve(PiecewiseLinearForward,ForwardRate,Linear);
export_piecewise_curve(PiecewiseLinearZero,ZeroYield,Linear);
export_piecewise_curve(PiecewiseCubicZero,ZeroYield,Cubic);
// combine other traits as you wish


#endif


#endif
