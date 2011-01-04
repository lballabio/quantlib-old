
/*
 Copyright (C) 2003 StatPro Italia srl

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
using QuantLib::GapPayoff;
typedef boost::shared_ptr<Payoff> PlainVanillaPayoffPtr;
typedef boost::shared_ptr<Payoff> PercentageStrikePayoffPtr;
typedef boost::shared_ptr<Payoff> CashOrNothingPayoffPtr;
typedef boost::shared_ptr<Payoff> AssetOrNothingPayoffPtr;
typedef boost::shared_ptr<Payoff> SuperSharePayoffPtr;
typedef boost::shared_ptr<Payoff> GapPayoffPtr;
%}

%rename(PlainVanillaPayoff) PlainVanillaPayoffPtr;
class PlainVanillaPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        PlainVanillaPayoffPtr(Option::Type type,
                              Real strike) {
            return new PlainVanillaPayoffPtr(
                                        new PlainVanillaPayoff(type, strike));
        }

		Option::Type optionType() {
          	return boost::dynamic_pointer_cast<
									PlainVanillaPayoff>(*self)->optionType();
		}

		Real strike() {
          	return boost::dynamic_pointer_cast<
									PlainVanillaPayoff>(*self)->strike();
		}
    }
};

%rename(PercentageStrikePayoff) PercentageStrikePayoffPtr;
class PercentageStrikePayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        PercentageStrikePayoffPtr(Option::Type type,
                                  Real moneyness) {
            return new PercentageStrikePayoffPtr(
                                 new PercentageStrikePayoff(type, moneyness));
        }
    }
};

%rename(CashOrNothingPayoff) CashOrNothingPayoffPtr;
class CashOrNothingPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        CashOrNothingPayoffPtr(Option::Type type,
                               Real strike,
                               Real payoff) {
            return new CashOrNothingPayoffPtr(
                               new CashOrNothingPayoff(type, strike, payoff));
        }
    }
};

%rename(AssetOrNothingPayoff) AssetOrNothingPayoffPtr;
class AssetOrNothingPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        AssetOrNothingPayoffPtr(Option::Type type,
                                Real strike) {
            return new AssetOrNothingPayoffPtr(
                                      new AssetOrNothingPayoff(type, strike));
        }
    }
};

%rename(SuperSharePayoff) SuperSharePayoffPtr;
class SuperSharePayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        SuperSharePayoffPtr(Option::Type type,
                            Real strike,
                            Real increment) {
            return new SuperSharePayoffPtr(
                               new SuperSharePayoff(type, strike, increment));
        }
    }
};

%rename(GapPayoff) GapPayoffPtr;
class GapPayoffPtr : public boost::shared_ptr<Payoff> {
  public:
    %extend {
        GapPayoffPtr(Option::Type type,
                            Real strike,
                            Real strikePayoff) {
            return new GapPayoffPtr(
                               new GapPayoff(type, strike, strikePayoff));
        }
    }
};


#endif
