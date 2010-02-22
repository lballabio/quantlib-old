
/*
 Copyright (C) 2004, 2005 StatPro Italia srl

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

#ifndef quantlib_interest_rate_i
#define quantlib_interest_rate_i

%include common.i
%include types.i
%include daycounters.i

%{
using QuantLib::Compounding;
using QuantLib::Simple;
using QuantLib::Compounded;
using QuantLib::Continuous;
using QuantLib::SimpleThenCompounded;
%}

enum Compounding {
    Simple,
    Compounded,
    Continuous,
    SimpleThenCompounded
};


%{
using QuantLib::InterestRate;
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
    #endif
  public:
    InterestRate();
    InterestRate(Rate r,
                 const DayCounter& dc,
                 Compounding comp,
                 Frequency freq);
    Rate rate() const;
    DayCounter dayCounter() const;
    Compounding compounding() const;
    Frequency frequency() const;
    DiscountFactor discountFactor(Time t) const;
    DiscountFactor discountFactor(const Date& d1, const Date& d2,
                                  const Date& refStart = Date(),
                                  const Date& refEnd = Date()) const;
    Real compoundFactor(Time t) const;
    Real compoundFactor(const Date& d1, const Date& d2,
                        const Date& refStart = Date(),
                        const Date& refEnd = Date()) const;
    static InterestRate impliedRate(Real compound,
                                    const DayCounter& resultDC,
                                    Compounding comp,
                                    Frequency freq,
                                    Time t);
    static InterestRate impliedRate(Real compound,
                                    const DayCounter& resultDC,
                                    Compounding comp,
                                    Frequency freq,
                                    const Date& d1,
                                    const Date& d2,
                                    const Date& refStart = Date(),
                                    const Date& refEnd = Date());
    InterestRate equivalentRate(Compounding comp,
                                Frequency freq,
                                Time t) const;
    InterestRate equivalentRate(const DayCounter& resultDayCounter,
                                Compounding comp,
                                Frequency freq,
                                const Date& d1,
                                const Date& d2,
                                const Date& refStart = Date(),
                                const Date& refEnd = Date()) const;
    %extend {
        std::string __str__() {
            std::ostringstream out;
            out << *self;
            return out.str();
        }
    }
};


#endif
