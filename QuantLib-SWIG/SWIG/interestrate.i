
/*
 Copyright (C) 2004 StatPro Italia srl

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

#ifndef quantlib_interest_rate_i
#define quantlib_interest_rate_i

%include common.i
%include types.i
%include daycounters.i

%{
using QuantLib::Compounding;

Compounding compoundingFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "simple")
        return QuantLib::Simple;
    else if (s == "compounded")
        return QuantLib::Compounded;
    else if (s == "continuous")
        return QuantLib::Continuous;
    else if (s == "simplethencompounded")
        return QuantLib::SimpleThenCompounded;
    else
        QL_FAIL("unknown compounding");
}

std::string stringFromCompounding(Compounding c) {
    switch (c) {
      case QuantLib::Simple:                return "simple";
      case QuantLib::Compounded:            return "compounded";
      case QuantLib::Continuous:            return "continuous";
      case QuantLib::SimpleThenCompounded:  return "simplethencompounded";
      default:                              QL_FAIL("unknown compounding");
    }
}
%}

MapToString(Compounding,compoundingFromString,stringFromCompounding);

%{
using QuantLib::InterestRate;
using QuantLib::Annual;
%}

class InterestRate {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-counter")              dayCounter;
    %rename("discount-factor")          discountFactor;
    %rename("compound-factor")          compoundFactor;
    %rename("implied-rate")             impliedRate;
    %rename("implied-interest-rate")    impliedInterestRate;
    %rename("equivalent-rate")          equivalentRate;
    %rename("equivalent-interest-rate") equivalentInterestRate;
    %rename(">string")                  __str__;
    #endif
  public:
    InterestRate();
    InterestRate(Rate r,
                 const DayCounter& dc,
                 Compounding comp,
                 Frequency freq = Annual);
    Rate rate() const;
    DayCounter dayCounter() const;
    Compounding compounding() const;
    Frequency frequency() const;
    DiscountFactor discountFactor(Time t) const;
    DiscountFactor discountFactor(Date d1, Date d2) const;
    Real compoundFactor(Time t) const;
    Real compoundFactor(Date d1, Date d2) const;
    static Rate impliedRate(Real compound,
                            Time t,
                            Compounding comp,
                            Frequency freq = Annual);
    static InterestRate impliedInterestRate(Real compound,
                                            Time t,
                                            const DayCounter& resultDC,
                                            Compounding comp,
                                            Frequency freq = Annual);
    static Rate impliedRate(Real compound,
                            Date d1,
                            Date d2,
                            const DayCounter& resultDayCounter,
                            Compounding comp,
                            Frequency freq = Annual);
    static InterestRate impliedInterestRate(Real compound,
                                            Date d1,
                                            Date d2,
                                            const DayCounter& resultDC,
                                            Compounding comp,
                                            Frequency freq = Annual);
    Rate equivalentRate(Time t,
                        Compounding comp,
                        Frequency freq = Annual) const;
    InterestRate equivalentInterestRate(Time t,
                                        Compounding comp,
                                        Frequency freq = Annual) const;
    Rate equivalentRate(Date d1,
                        Date d2,
                        const DayCounter& resultDayCounter,
                        Compounding comp,
                        Frequency freq = Annual) const;
    InterestRate equivalentInterestRate(Date d1,
                                        Date d2,
                                        const DayCounter& resultDayCounter,
                                        Compounding comp,
                                        Frequency freq = Annual) const;
    %extend {
        std::string __str__() {
            return QuantLib::InterestRateFormatter::toString(*self);
        }
    }
};


#endif
