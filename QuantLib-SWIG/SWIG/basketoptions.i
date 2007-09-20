
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2007 Joseph Wang

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

#ifndef quantlib_basket_options_i
#define quantlib_basket_options_i

%include date.i
%include options.i
%include payoffs.i
%{
using QuantLib::BasketOption;
using QuantLib::BasketPayoff;
using QuantLib::MinBasketPayoff;
using QuantLib::MaxBasketPayoff;
using QuantLib::AverageBasketPayoff;
typedef boost::shared_ptr<Instrument> BasketOptionPtr;
typedef boost::shared_ptr<Payoff> BasketPayoffPtr;
typedef boost::shared_ptr<Payoff> MinBasketPayoffPtr;
typedef boost::shared_ptr<Payoff> MaxBasketPayoffPtr;
typedef boost::shared_ptr<Payoff> AverageBasketPayoffPtr;
%}

%rename(BasketPayoff) BasketPayoffPtr;
class BasketPayoffPtr : public boost::shared_ptr<Payoff> {};

%rename(MinBasketPayoff) MinBasketPayoffPtr;
class MinBasketPayoffPtr : public BasketPayoffPtr  {
  public:
    %extend {
        MinBasketPayoffPtr(const boost::shared_ptr<Payoff> p) {
            return new MinBasketPayoffPtr(new MinBasketPayoff(p));
        }
    }
};

%rename(MaxBasketPayoff) MaxBasketPayoffPtr;
class MaxBasketPayoffPtr : public BasketPayoffPtr  {
  public:
    %extend {
        MaxBasketPayoffPtr(const boost::shared_ptr<Payoff> p) {
            return new MaxBasketPayoffPtr(new MaxBasketPayoff(p));
        }
    }
};

%rename(AverageBasketPayoff) AverageBasketPayoffPtr;
class AverageBasketPayoffPtr :
      public BasketPayoffPtr  {
  public:
    %extend {
        AverageBasketPayoffPtr(const boost::shared_ptr<Payoff> p,
                               const Array &a) {
            return new AverageBasketPayoffPtr(new AverageBasketPayoff(p, a));
        }
        AverageBasketPayoffPtr(const boost::shared_ptr<Payoff> p,
                               Size n) {
            return new AverageBasketPayoffPtr(new AverageBasketPayoff(p, n));
        }
    }
};


%rename(BasketOption) BasketOptionPtr;
class BasketOptionPtr : public MultiAssetOptionPtr {
  public:
    %extend {
        BasketOptionPtr(
                const boost::shared_ptr<StochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<BasketPayoff> stPayoff =
                 boost::dynamic_pointer_cast<BasketPayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new BasketOptionPtr(
                          new BasketOption(process,stPayoff,exercise,engine));
        }
    }
};


%{
using QuantLib::MCBasketEngine;
typedef boost::shared_ptr<PricingEngine> MCBasketEnginePtr;
%}

%rename(MCBasketEngine) MCBasketEnginePtr;
class MCBasketEnginePtr : public boost::shared_ptr<PricingEngine> {
    %feature("kwargs") MCBasketEnginePtr;
  public:
    %extend {
        MCBasketEnginePtr(const std::string& traits,
                           Size timeStepsPerYear = Null<Size>(),
                           bool brownianBridge = false,
                           bool antitheticVariate = false,
                           bool controlVariate = false,
                           intOrNull requiredSamples = Null<Size>(),
                           doubleOrNull requiredTolerance = Null<Real>(),
                           intOrNull maxSamples = Null<Size>(),
                           BigInteger seed = 0) {
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCBasketEnginePtr(
                         new MCBasketEngine<PseudoRandom>(timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           controlVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCBasketEnginePtr(
                       new MCBasketEngine<LowDiscrepancy>(timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           controlVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           seed));
            else
                QL_FAIL("unknown Monte Carlo engine type: "+s);
        }
    }
};

%{
using QuantLib::MCAmericanBasketEngine;
typedef boost::shared_ptr<PricingEngine> MCAmericanBasketEnginePtr;
%}

%rename(MCAmericanBasketEngine) MCAmericanBasketEnginePtr;
class MCAmericanBasketEnginePtr : public boost::shared_ptr<PricingEngine> {
    %feature("kwargs") MCAmericanBasketEnginePtr;
  public:
    %extend {
        MCAmericanBasketEnginePtr(const std::string& traits,
                           Size timeSteps = Null<Size>(),
                           Size timeStepsPerYear = Null<Size>(),
                           bool brownianBridge = false,
                           bool antitheticVariate = false,
                           bool controlVariate = false,
                           intOrNull requiredSamples = Null<Size>(),
                           doubleOrNull requiredTolerance = Null<Real>(),
                           intOrNull maxSamples = Null<Size>(),
                           BigInteger seed = 0) {
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                  return new MCAmericanBasketEnginePtr(
                  new MCAmericanBasketEngine<PseudoRandom>(timeSteps,
                                                           timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           controlVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCAmericanBasketEnginePtr(
                new MCAmericanBasketEngine<LowDiscrepancy>(timeSteps,
                                                           timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           controlVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           seed));
            else
                QL_FAIL("unknown Monte Carlo engine type: "+s);
        }
    }
};


%{
using QuantLib::StulzEngine;
typedef boost::shared_ptr<PricingEngine> StulzEnginePtr;
%}

%rename(StulzEngine) StulzEnginePtr;
class StulzEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        StulzEnginePtr() {
            return new StulzEnginePtr(new StulzEngine);
        }
    }
};

#endif
