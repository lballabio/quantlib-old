
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_options_i
#define quantlib_options_i

%include common.i
%include exercise.i
%include stochasticprocess.i
%include instruments.i
%include stl.i

// option types
%{
using QuantLib::Option;
typedef Option::Type OptionType;

Option::Type optionTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "c" || s == "call")
        return Option::Call;
    else if (s == "p" || s == "put")
        return Option::Put;
    else if (s == "s" || s == "straddle")
        return Option::Straddle;
    else
        throw Error("unknown option type: "+s);
}

std::string optionTypeToString(Option::Type t) {
    switch (t) {
      case Option::Call:
        return "Call";
      case Option::Put:
        return "Put";
      case Option::Straddle:
        return "Straddle";
      default:
        throw Error("unknown option type");
    }
}
%}

MapToString(OptionType,optionTypeFromString,optionTypeToString);


// payoff

%{
using QuantLib::Payoff;
using QuantLib::StrikedTypePayoff;
%}

%ignore Payoff;
class Payoff {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call) operator();
    #endif
  public:
    double operator()(double price) const;
};

%template(Payoff) boost::shared_ptr<Payoff>;


// plain options and engines

%{
using QuantLib::VanillaOption;
typedef boost::shared_ptr<Instrument> VanillaOptionPtr;
%}


%rename(VanillaOption) VanillaOptionPtr;
class VanillaOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        VanillaOptionPtr(
                const boost::shared_ptr<StochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "Wrong payoff given");
            boost::shared_ptr<BlackScholesStochasticProcess> bsProcess =
                boost::dynamic_pointer_cast<BlackScholesStochasticProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Wrong stochastic process given");
            return new VanillaOptionPtr(
                new VanillaOption(bsProcess,stPayoff,exercise,engine));
        }
        double errorEstimate() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->errorEstimate();
        }
        double delta() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->delta();
        }
        double gamma() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->gamma();
        }
        double theta() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->theta();
        }
        double vega() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->vega();
        }
        double rho() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->rho();
        }
        double dividendRho() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->dividendRho();
        }
        double strikeSensitivity() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->strikeSensitivity();
        }
        double impliedVolatility(double targetValue, double accuracy = 1.0e-4,
                                 Size maxEvaluations = 100,
                                 double minVol = 1.0e-4, double maxVol = 4.0) {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->impliedVolatility(targetValue,accuracy,maxEvaluations,
                                     minVol,maxVol);
        }
    }
};


// European engines

%{
using QuantLib::AnalyticEuropeanEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticEuropeanEnginePtr;
%}

%rename(AnalyticEuropeanEngine) AnalyticEuropeanEnginePtr;
class AnalyticEuropeanEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticEuropeanEnginePtr() {
            return new AnalyticEuropeanEnginePtr(new AnalyticEuropeanEngine);
        }
    }
};


%{
using QuantLib::IntegralEngine;
typedef boost::shared_ptr<PricingEngine> IntegralEnginePtr;
%}

%rename(IntegralEngine) IntegralEnginePtr;
class IntegralEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        IntegralEnginePtr() {
            return new IntegralEnginePtr(new IntegralEngine);
        }
    }
};


%{
using QuantLib::BinomialVanillaEngine;
using QuantLib::CoxRossRubinstein;
using QuantLib::JarrowRudd;
using QuantLib::AdditiveEQPBinomialTree;
using QuantLib::Trigeorgis;
using QuantLib::Tian;
using QuantLib::LeisenReimer;
typedef boost::shared_ptr<PricingEngine> BinomialVanillaEnginePtr;
%}

%rename(BinomialEuropeanEngine) BinomialVanillaEnginePtr;
class BinomialVanillaEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BinomialVanillaEnginePtr(const std::string& type,
                                 Size steps) {
            std::string s = StringFormatter::toLowercase(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<CoxRossRubinstein>(steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<JarrowRudd>(steps));
            else if (s == "eqp")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<AdditiveEQPBinomialTree>(steps));
            else if (s == "trigeorgis")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<Trigeorgis>(steps));
            else if (s == "tian")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<Tian>(steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<LeisenReimer>(steps));
            else
                throw Error("unknown binomial engine type: "+s);
        }
    }
};


%{
using QuantLib::MCEuropeanEngine;
using QuantLib::PseudoRandom;
using QuantLib::LowDiscrepancy;
typedef boost::shared_ptr<PricingEngine> MCEuropeanEnginePtr;
%}

%rename(MCEuropeanEngine) MCEuropeanEnginePtr;
class MCEuropeanEnginePtr : public boost::shared_ptr<PricingEngine> {
    %feature("kwargs") MCEuropeanEnginePtr;
  public:
    %extend {
        MCEuropeanEnginePtr(const std::string& traits,
                            Size timeSteps,
                            bool antitheticVariate = false,
                            bool controlVariate = false,
                            intOrNull requiredSamples = Null<int>(),
                            doubleOrNull requiredTolerance = Null<double>(),
                            intOrNull maxSamples = Null<int>(),
                            long seed = 0) {
            std::string s = StringFormatter::toLowercase(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCEuropeanEnginePtr(
                         new MCEuropeanEngine<PseudoRandom>(timeSteps,
                                                            antitheticVariate,
                                                            controlVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCEuropeanEnginePtr(
                       new MCEuropeanEngine<LowDiscrepancy>(timeSteps,
                                                            antitheticVariate,
                                                            controlVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else
                throw Error("unknown Monte Carlo engine type: "+s);
        }
    }
};


// American engines

%{
using QuantLib::BaroneAdesiWhaleyApproximationEngine;
typedef boost::shared_ptr<PricingEngine> 
    BaroneAdesiWhaleyApproximationEnginePtr;
%}

%rename(BaroneAdesiWhaleyEngine) BaroneAdesiWhaleyApproximationEnginePtr;
class BaroneAdesiWhaleyApproximationEnginePtr 
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BaroneAdesiWhaleyApproximationEnginePtr() {
            return new BaroneAdesiWhaleyApproximationEnginePtr(
                new BaroneAdesiWhaleyApproximationEngine);
        }
    }
};


%{
using QuantLib::BjerksundStenslandApproximationEngine;
typedef boost::shared_ptr<PricingEngine> 
    BjerksundStenslandApproximationEnginePtr;
%}

%rename(BjerksundStenslandEngine) BjerksundStenslandApproximationEnginePtr;
class BjerksundStenslandApproximationEnginePtr 
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BjerksundStenslandApproximationEnginePtr() {
            return new BjerksundStenslandApproximationEnginePtr(
                new BjerksundStenslandApproximationEngine);
        }
    }
};


#endif
