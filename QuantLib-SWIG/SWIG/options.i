
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

%template(Payoff) Handle<Payoff>;


// plain options and engines

%{
using QuantLib::VanillaOption;
typedef Handle<Instrument> VanillaOptionHandle;
%}


%rename(VanillaOption) VanillaOptionHandle;
class VanillaOptionHandle : public Handle<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        VanillaOptionHandle(
                const Handle<StochasticProcess>& process,
                const Handle<Payoff>& payoff,
                const Handle<Exercise>& exercise,
                const Handle<PricingEngine>& engine
                   = Handle<PricingEngine>()) {
            Handle<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "Wrong payoff given");
            Handle<BlackScholesStochasticProcess> bsProcess =
                boost::dynamic_pointer_cast<BlackScholesStochasticProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Wrong stochastic process given");
            return new VanillaOptionHandle(
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
typedef Handle<PricingEngine> AnalyticEuropeanEngineHandle;
%}

%rename(AnalyticEuropeanEngine) AnalyticEuropeanEngineHandle;
class AnalyticEuropeanEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        AnalyticEuropeanEngineHandle() {
            return new AnalyticEuropeanEngineHandle(
                new AnalyticEuropeanEngine);
        }
    }
};


%{
using QuantLib::IntegralEngine;
typedef Handle<PricingEngine> IntegralEngineHandle;
%}

%rename(IntegralEngine) IntegralEngineHandle;
class IntegralEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        IntegralEngineHandle() {
            return new IntegralEngineHandle(new IntegralEngine);
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
typedef Handle<PricingEngine> BinomialVanillaEngineHandle;
%}

%rename(BinomialEuropeanEngine) BinomialVanillaEngineHandle;
class BinomialVanillaEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        BinomialVanillaEngineHandle(const std::string& type,
                                    Size steps) {
            std::string s = StringFormatter::toLowercase(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialVanillaEngineHandle(
                    new BinomialVanillaEngine<CoxRossRubinstein>(steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialVanillaEngineHandle(
                    new BinomialVanillaEngine<JarrowRudd>(steps));
            else if (s == "eqp")
                return new BinomialVanillaEngineHandle(
                    new BinomialVanillaEngine<AdditiveEQPBinomialTree>(steps));
            else if (s == "trigeorgis")
                return new BinomialVanillaEngineHandle(
                    new BinomialVanillaEngine<Trigeorgis>(steps));
            else if (s == "tian")
                return new BinomialVanillaEngineHandle(
                    new BinomialVanillaEngine<Tian>(steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialVanillaEngineHandle(
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
typedef Handle<PricingEngine> MCEuropeanEngineHandle;
%}

%rename(MCEuropeanEngine) MCEuropeanEngineHandle;
class MCEuropeanEngineHandle : public Handle<PricingEngine> {
    %feature("kwargs") MCEuropeanEngineHandle;
  public:
    %extend {
        MCEuropeanEngineHandle(const std::string& traits,
                               Size timeSteps,
                               bool antitheticVariate = false,
                               bool controlVariate = false,
                               intOrNull requiredSamples = Null<int>(),
                               doubleOrNull requiredTolerance = Null<double>(),
                               intOrNull maxSamples = Null<int>(),
                               long seed = 0) {
            std::string s = StringFormatter::toLowercase(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCEuropeanEngineHandle(
                         new MCEuropeanEngine<PseudoRandom>(timeSteps,
                                                            antitheticVariate,
                                                            controlVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCEuropeanEngineHandle(
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
typedef Handle<PricingEngine> BaroneAdesiWhaleyApproximationEngineHandle;
%}

%rename(BaroneAdesiWhaleyEngine) 
    BaroneAdesiWhaleyApproximationEngineHandle;
class BaroneAdesiWhaleyApproximationEngineHandle 
    : public Handle<PricingEngine> {
  public:
    %extend {
        BaroneAdesiWhaleyApproximationEngineHandle() {
            return new BaroneAdesiWhaleyApproximationEngineHandle(
                new BaroneAdesiWhaleyApproximationEngine);
        }
    }
};


%{
using QuantLib::BjerksundStenslandApproximationEngine;
typedef Handle<PricingEngine> BjerksundStenslandApproximationEngineHandle;
%}

%rename(BjerksundStenslandEngine) 
    BjerksundStenslandApproximationEngineHandle;
class BjerksundStenslandApproximationEngineHandle 
    : public Handle<PricingEngine> {
  public:
    %extend {
        BjerksundStenslandApproximationEngineHandle() {
            return new BjerksundStenslandApproximationEngineHandle(
                new BjerksundStenslandApproximationEngine);
        }
    }
};


#endif
