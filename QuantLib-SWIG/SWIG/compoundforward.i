
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

#ifndef quantlib_compoundforward_i
#define quantlib_compoundforward_i

%include date.i
%include termstructures.i
%include marketelements.i

%{
using QuantLib::CompoundForward;
typedef boost::shared_ptr<TermStructure> CompoundForwardPtr;
%}

%rename(CompoundForward) CompoundForwardPtr;
class CompoundForwardPtr : public boost::shared_ptr<TermStructure> {
  public:
    %extend {
        CompoundForwardPtr(const Date& todaysDate,
                           const Date& settlementDate,
                           const std::vector<Date>& dates,
                           const std::vector<Rate>& rates,
                           Calendar calendar,
                           RollingConvention roll,
                           Integer compounding,
                           const DayCounter& dayCounter = Actual365()) {
	        return new CompoundForwardPtr(
                new CompoundForward(todaysDate, settlementDate,
                                    dates, rates, calendar, roll,
                                    compounding, dayCounter));
        }
        const std::vector<Date>& dates() {
            return boost::dynamic_pointer_cast<CompoundForward>(*self)
                 ->dates();
        }
    }
};


#endif
