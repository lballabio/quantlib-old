
/*
 Copyright (C) 2005, 2006 StatPro Italia srl

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

#ifndef quantlib_zero_curve_i
#define quantlib_zero_curve_i

%include termstructures.i
%include interpolation.i

%{
using QuantLib::InterpolatedZeroCurve;
%}

%define export_zero_curve(Name,Interpolator)

%{
typedef boost::shared_ptr<YieldTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        Name##Ptr(const std::vector<Date>& dates,
                  const std::vector<Rate>& yields,
                  const DayCounter& dayCounter,
                  const Calendar& calendar = Calendar(),
                  const Interpolator& i = Interpolator(),
                  Compounding compounding = Continuous,
                  Frequency frequency = Annual) {
            return new Name##Ptr(
                new InterpolatedZeroCurve<Interpolator>(dates,yields,
                                                        dayCounter,
                                                        calendar,i,
                                                        compounding,
                                                        frequency));
        }
        const std::vector<Date>& dates() {
            typedef InterpolatedZeroCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Rate>& zeroRates() {
            typedef InterpolatedZeroCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->zeroRates();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Rate> > nodes() {
            typedef InterpolatedZeroCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef


export_zero_curve(ZeroCurve,Linear);

// add interpolations as you wish, e.g.,
// export_zero_curve(CubicZeroCurve,Cubic);



#endif
