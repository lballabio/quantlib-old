
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

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

// $Id$

#ifndef quantlib_compoundforward_i
#define quantlib_compoundforward_i

%include date.i
%include termstructures.i
%include marketelements.i

%{
using QuantLib::TermStructures::CompoundForward;
typedef Handle<TermStructure> CompoundForwardHandle;
//using QuantLib::TermStructures::CompoundSpreadedTermStructure;
//typedef Handle<TermStructure> CompoundSpreadedTermStructureHandle;
%}

%rename(CompoundForward) CompoundForwardHandle;
class CompoundForwardHandle : public Handle<TermStructure> {
  public:
    %extend {
        CompoundForwardHandle(const Date& todaysDate,
			      const Date& settlementDate,
			      const std::vector<Date>& dates,
			      const std::vector<double>& rates,
			      Calendar calendar,
			      RollingConvention roll,
			      int compounding,
			      const DayCounter& dayCounter = Actual365()) {
	        return new CompoundForwardHandle(
		   new CompoundForward(todaysDate,
				       settlementDate,
				       dates, rates,
				       calendar, roll,
				       compounding, dayCounter));
        }
        const std::vector<Date>& dates() {
	   return Handle<CompoundForward>(*self)->dates();
	}
    }
};

/*
%rename(CompoundSpreadedTermStructure) CompoundSpreadedTermStructureHandle;
class CompoundSpreadedTermStructureHandle : public Handle<TermStructure> {
  public:
    %extend {
        CompoundSpreadedTermStructureHandle(
                const RelinkableHandle<TermStructure>& curveHandle,
		const Date& todaysDate, const Date& settlementDate,
		const std::vector<Date>& dates,
		const std::vector<double>& spreads,
		const Calendar & calendar,
		const RollingConvention roll,
		const int compounding,
		const DayCounter & dayCounter = Actual365()) {
	        return new CompoundSpreadedTermStructureHandle(
	            new CompoundSpreadedTermStructure(
		       curveHandle,
		       todaysDate, settlementDate,
		       dates, spreads,
		       calendar, roll,
		       compounding, dayCounter));
        }
    }
};
*/


#endif
