
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_discount_curve_i
#define quantlib_discount_curve_i

%include termstructures.i

%{
using QuantLib::TermStructures::DiscountCurve;
typedef Handle<TermStructure> DiscountCurveHandle;
%}

%rename(DiscountCurve) DiscountCurveHandle;
class DiscountCurveHandle : public Handle<TermStructure> {
  public:
    %extend {
        DiscountCurveHandle(const Date& todaysDate, 
                            const std::vector<Date>& dates,
                            const std::vector<double>& discounts,
                            const DayCounter& dayCounter = Actual365()) {
	        return new DiscountCurveHandle(
                new DiscountCurve(todaysDate, dates, discounts, dayCounter));
        }
    }
};


#endif
