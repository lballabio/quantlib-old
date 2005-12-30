
/*
 Copyright (C) 2000-2003 StatPro Italia srl
 Copyright (c) 2005 Dominic Thuillier

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

#ifndef quantlib_piecewise_flat_forward_i
#define quantlib_piecewise_flat_forward_i

%include termstructures.i
%include ratehelpers.i

#if defined(VC6)

%{
using QuantLib::PiecewiseFlatForward;
typedef boost::shared_ptr<YieldTermStructure> PiecewiseFlatForwardPtr;
%}

%rename(PiecewiseFlatForward) PiecewiseFlatForwardPtr;
class PiecewiseFlatForwardPtr : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        PiecewiseFlatForwardPtr(
                const Date& referenceDate,
                const std::vector<boost::shared_ptr<RateHelper> >& instruments,
                const DayCounter& dayCounter,
                Real accuracy = 1.0e-12) {
	        return new PiecewiseFlatForwardPtr(
	            new PiecewiseFlatForward(referenceDate,instruments,
                                         dayCounter,accuracy));
        }
        PiecewiseFlatForwardPtr(
                Integer settlementDays, const Calendar& calendar,
                const std::vector<boost::shared_ptr<RateHelper> >& instruments,
                const DayCounter& dayCounter,
                Real accuracy = 1.0e-12) {
	        return new PiecewiseFlatForwardPtr(
	            new PiecewiseFlatForward(settlementDays, calendar,
                                         instruments, dayCounter, accuracy));
        }
        const std::vector<Date>& dates() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->dates();
        }
        const std::vector<Time>& times() {
            return boost::dynamic_pointer_cast<PiecewiseFlatForward>(*self)
                 ->times();
        }
    }
};

#endif


#endif
