
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
typedef boost::shared_ptr<Payoff> PlainVanillaPayoffPtr;
typedef boost::shared_ptr<Payoff> PercentageStrikePayoffPtr;
typedef boost::shared_ptr<Payoff> CashOrNothingPayoffPtr;
typedef boost::shared_ptr<Payoff> AssetOrNothingPayoffPtr;
typedef boost::shared_ptr<Payoff> SuperSharePayoffPtr;
%}

%rename(PlainVanillaPayoff) PlainVanillaPayoffPtr;
class PlainVanillaPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        PlainVanillaPayoffPtr(OptionType type,
                              double strike) {
            return new PlainVanillaPayoffPtr(
                                        new PlainVanillaPayoff(type, strike));
        }
    }
};

%rename(PercentageStrikePayoff) PercentageStrikePayoffPtr;
class PercentageStrikePayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        PercentageStrikePayoffPtr(OptionType type,
                                  double moneyness) {
            return new PercentageStrikePayoffPtr(
                                 new PercentageStrikePayoff(type, moneyness));
        }
    }
};

%rename(CashOrNothingPayoff) CashOrNothingPayoffPtr;
class CashOrNothingPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        CashOrNothingPayoffPtr(OptionType type,
                               double strike,
                               double payoff) {
            return new CashOrNothingPayoffPtr(
                               new CashOrNothingPayoff(type, strike, payoff));
        }
    }
};

%rename(AssetOrNothingPayoff) AssetOrNothingPayoffPtr;
class AssetOrNothingPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        AssetOrNothingPayoffPtr(OptionType type,
                                double strike) {
            return new AssetOrNothingPayoffPtr(
                                      new AssetOrNothingPayoff(type, strike));
        }
    }
};

%rename(SuperSharePayoff) SuperSharePayoffPtr;
class SuperSharePayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        SuperSharePayoffPtr(OptionType type,
                            double strike,
                            double increment) {
            return new SuperSharePayoffPtr(
                               new SuperSharePayoff(type, strike, increment));
        }
    }
};


#endif
