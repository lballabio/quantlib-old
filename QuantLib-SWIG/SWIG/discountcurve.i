
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2015 Matthias Groncki

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

#ifndef quantlib_discount_curve_i
#define quantlib_discount_curve_i

%include termstructures.i
%include interpolation.i

%{
using QuantLib::InterpolatedDiscountCurve;
%}

%define export_discount_curve(Name,Interpolator)

%{
typedef boost::shared_ptr<YieldTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        Name##Ptr(const std::vector<Date>& dates,
                  const std::vector<DiscountFactor>& discounts,
                  const DayCounter& dayCounter,
                  const Calendar& calendar = Calendar(),
                  const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new InterpolatedDiscountCurve<Interpolator>(dates,discounts,
                                                            dayCounter,
                                                            calendar,i));
        }
        const std::vector<Time>& times() {
            typedef InterpolatedDiscountCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->times();
        }
        const std::vector<Real>& data() {
            typedef InterpolatedDiscountCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->data();
        }
        const std::vector<Date>& dates() {
            typedef InterpolatedDiscountCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<DiscountFactor>& discounts() {
            typedef InterpolatedDiscountCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->discounts();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,DiscountFactor> > nodes() {
            typedef InterpolatedDiscountCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef


export_discount_curve(DiscountCurve,LogLinear);

// add interpolations as you wish, e.g.,
// export_discount_curve(LinearDiscountCurve,Linear);



#endif
