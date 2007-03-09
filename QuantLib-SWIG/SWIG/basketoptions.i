
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier

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

#ifndef quantlib_basket_options_i
#define quantlib_basket_options_i

%include date.i
%include options.i
%include payoffs.i
%{
using QuantLib::BasketOption;
using QuantLib::BasketOptionType;
using QuantLib::MinBasketOptionType;
using QuantLib::MaxBasketOptionType;
using QuantLib::AverageBasketOptionType;
typedef boost::shared_ptr<Instrument> BasketOptionPtr;
typedef boost::shared_ptr<BasketOptionType> BasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> MinBasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> MaxBasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> AverageBasketOptionTypePtr;
%}

%template(BasketOptionType) boost::shared_ptr<BasketOptionType>;

%rename(MinBasketOptionType) MinBasketOptionTypePtr;
class MinBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        MinBasketOptionTypePtr() {
            return new MinBasketOptionTypePtr(new MinBasketOptionType);
        }
    }
};

%rename(MaxBasketOptionType) MaxBasketOptionTypePtr;
class MaxBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        MaxBasketOptionTypePtr() {
            return new MaxBasketOptionTypePtr(new MaxBasketOptionType);
        }
    }
};

%rename(AverageBasketOptionType) AverageBasketOptionTypePtr;
class AverageBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        AverageBasketOptionTypePtr(const Array &a) {
            return new AverageBasketOptionTypePtr(new 
	    AverageBasketOptionType(a));
        }
        AverageBasketOptionTypePtr(Size n) {
            return new AverageBasketOptionTypePtr(new 
	    AverageBasketOptionType(n));
        }
    }
};


%rename(BasketOption) BasketOptionPtr;
class BasketOptionPtr : public MultiAssetOptionPtr {
  public:
    %extend {
        BasketOptionPtr(
	        const boost::shared_ptr<BasketOptionType>& btype,
                const boost::shared_ptr<StochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<PlainVanillaPayoff> stPayoff =
                 boost::dynamic_pointer_cast<PlainVanillaPayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new BasketOptionPtr(
                        new BasketOption(btype,
			process,stPayoff,exercise,
			engine));
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
//            std::string s = QuantLib::lowercase(traits);
        std::string s = traits;
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
//            std::string s = QuantLib::lowercase(traits);
        std::string s = traits;
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
