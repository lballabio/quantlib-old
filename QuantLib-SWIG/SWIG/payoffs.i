
/*
 Copyright (C) 2003 StatPro Italia srl

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

#ifndef quantlib_payoffs_i
#define quantlib_payoffs_i

%include options.i

// payoffs

%{
using QuantLib::PlainVanillaPayoff;
using QuantLib::PercentageStrikePayoff;
using QuantLib::CashOrNothingPayoff;
using QuantLib::AssetOrNothingPayoff;
using QuantLib::SuperSharePayoff;
typedef Handle<Payoff> PlainVanillaPayoffHandle;
typedef Handle<Payoff> PercentageStrikePayoffHandle;
typedef Handle<Payoff> CashOrNothingPayoffHandle;
typedef Handle<Payoff> AssetOrNothingPayoffHandle;
typedef Handle<Payoff> SuperSharePayoffHandle;
%}

%rename(PlainVanillaPayoff) PlainVanillaPayoffHandle;
class PlainVanillaPayoffHandle : public Handle<Payoff> {
  public:
    %extend {
        PlainVanillaPayoffHandle(OptionType type,
                                 double strike) {
            return new PlainVanillaPayoffHandle(
                                        new PlainVanillaPayoff(type, strike));
        }
    }
};

%rename(PercentageStrikePayoff) PercentageStrikePayoffHandle;
class PercentageStrikePayoffHandle : public Handle<Payoff> {
  public:
    %extend {
        PercentageStrikePayoffHandle(OptionType type,
                                     double moneyness) {
            return new PercentageStrikePayoffHandle(
                                 new PercentageStrikePayoff(type, moneyness));
        }
    }
};

%rename(CashOrNothingPayoff) CashOrNothingPayoffHandle;
class CashOrNothingPayoffHandle : public Handle<Payoff> {
  public:
    %extend {
        CashOrNothingPayoffHandle(OptionType type,
                                  double strike,
                                  double payoff) {
            return new CashOrNothingPayoffHandle(
                               new CashOrNothingPayoff(type, strike, payoff));
        }
    }
};

%rename(AssetOrNothingPayoff) AssetOrNothingPayoffHandle;
class AssetOrNothingPayoffHandle : public Handle<Payoff> {
  public:
    %extend {
        AssetOrNothingPayoffHandle(OptionType type,
                                   double strike) {
            return new AssetOrNothingPayoffHandle(
                                      new AssetOrNothingPayoff(type, strike));
        }
    }
};

%rename(SuperSharePayoff) SuperSharePayoffHandle;
class SuperSharePayoffHandle : public Handle<Payoff> {
  public:
    %extend {
        SuperSharePayoffHandle(OptionType type,
                               double strike,
                               double increment) {
            return new SuperSharePayoffHandle(
                               new SuperSharePayoff(type, strike, increment));
        }
    }
};


#endif
