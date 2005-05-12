
/*
 Copyright (C) 2000-2004 StatPro Italia srl

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

// option and barrier types
%{
using QuantLib::Option;
typedef Option::Type OptionType;
using QuantLib::Barrier;
typedef Barrier::Type BarrierType;

Option::Type optionTypeFromString(std::string s) {
    s = QuantLib::lowercase(s);
    if (s == "c" || s == "call")
        return Option::Call;
    else if (s == "p" || s == "put")
        return Option::Put;
    else
        QL_FAIL("unknown option type: "+s);
}

std::string optionTypeToString(Option::Type t) {
    switch (t) {
      case Option::Call:
        return "Call";
      case Option::Put:
        return "Put";
      default:
        QL_FAIL("unknown option type");
    }
}

BarrierType barrierTypeFromString(std::string s) {
    s = QuantLib::lowercase(s);
    if (s == "downin")
        return Barrier::DownIn;
    else if (s == "downout")
        return Barrier::DownOut;
    else if (s == "upin")
        return Barrier::UpIn;
    else if (s == "upout")
        return Barrier::UpOut;
    else
        QL_FAIL("unknown barrier type: "+s);
}

std::string barrierTypeToString(BarrierType t) {
    switch (t) {
      case Barrier::DownIn:
        return "DownIn";
      case Barrier::DownOut:
        return "DownOut";
      case Barrier::UpIn:
        return "UpIn";
      case Barrier::UpOut:
        return "UpOut";
      default:
        QL_FAIL("unknown barrier type");
    }
}
%}

MapToString(OptionType,optionTypeFromString,optionTypeToString);
MapToString(BarrierType,barrierTypeFromString,barrierTypeToString);


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
    Real operator()(Real price) const;
};

%template(Payoff) boost::shared_ptr<Payoff>;


// plain option and engines

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
                const boost::shared_ptr<GenericStochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new VanillaOptionPtr(
                         new VanillaOption(process,stPayoff,exercise,engine));
        }
        Real delta() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->delta();
        }
        Real gamma() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->gamma();
        }
        Real theta() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->theta();
        }
        Real vega() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->vega();
        }
        Real rho() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)->rho();
        }
        Real dividendRho() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->dividendRho();
        }
        Real strikeSensitivity() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->strikeSensitivity();
        }
        Volatility impliedVolatility(Real targetValue,
                                     Real accuracy = 1.0e-4,
                                     Size maxEvaluations = 100,
                                     Volatility minVol = 1.0e-4,
                                     Volatility maxVol = 4.0) {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                 ->impliedVolatility(targetValue,accuracy,maxEvaluations,
                                     minVol,maxVol);
        }
    }
};


%{
using QuantLib::EuropeanOption;
typedef boost::shared_ptr<Instrument> EuropeanOptionPtr;
%}


%rename(EuropeanOption) EuropeanOptionPtr;
class EuropeanOptionPtr : public VanillaOptionPtr {
  public:
    %extend {
        EuropeanOptionPtr(
                const boost::shared_ptr<GenericStochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new EuropeanOptionPtr(
                        new EuropeanOption(process,stPayoff,exercise,engine));
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
using QuantLib::FDEuropeanEngine;
typedef boost::shared_ptr<PricingEngine> FDEuropeanEnginePtr;
%}

%rename(FDEuropeanEngine) FDEuropeanEnginePtr;
class FDEuropeanEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDEuropeanEnginePtr(Size timeSteps = 100, Size gridPoints = 100,
                            bool timeDependent = false) {
            return new FDEuropeanEnginePtr(
                    new FDEuropeanEngine(timeSteps,gridPoints,timeDependent));
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
            std::string s = QuantLib::lowercase(type);
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
                QL_FAIL("unknown binomial engine type: "+s);
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
                            Size timeSteps = Null<Size>(),
                            Size timeStepsPerYear = Null<Size>(),
                            bool brownianBridge = false,
                            bool antitheticVariate = false,
                            bool controlVariate = false,
                            intOrNull requiredSamples = Null<Integer>(),
                            doubleOrNull requiredTolerance = Null<Real>(),
                            intOrNull maxSamples = Null<Integer>(),
                            BigInteger seed = 0) {
            std::string s = QuantLib::lowercase(traits);
            QL_REQUIRE(timeSteps != Null<Size>() ||
                       timeStepsPerYear != Null<Size>(),
                       "number of steps not specified");
            if (s == "pseudorandom" || s == "pr")
                return new MCEuropeanEnginePtr(
                         new MCEuropeanEngine<PseudoRandom>(timeSteps,
                                                            timeStepsPerYear,
                                                            brownianBridge,
                                                            antitheticVariate,
                                                            controlVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCEuropeanEnginePtr(
                       new MCEuropeanEngine<LowDiscrepancy>(timeSteps,
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


// American engines

%{
using QuantLib::FDAmericanEngine;
 using QuantLib::FDShoutEngine;
typedef boost::shared_ptr<PricingEngine> FDAmericanEnginePtr;
typedef boost::shared_ptr<PricingEngine> FDShoutEnginePtr;
%}

%rename(FDAmericanEngine) FDAmericanEnginePtr;
class FDAmericanEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDAmericanEnginePtr(Size timeSteps = 100, Size gridPoints = 100,
                            bool timeDependent = false) {
            return new FDAmericanEnginePtr(
                    new FDAmericanEngine(timeSteps,gridPoints,timeDependent));
        }
    }
};

%rename(FDShoutEngine) FDShoutEnginePtr;
class FDShoutEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDShoutEnginePtr(Size timeSteps = 100, Size gridPoints = 100,
                         bool timeDependent = false) {
            return new FDShoutEnginePtr(
                       new FDShoutEngine(timeSteps,gridPoints,timeDependent));
        }
    }
};


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

%{
using QuantLib::AnalyticDigitalAmericanEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDigitalAmericanEnginePtr;
%}

%rename(AnalyticDigitalAmericanEngine) AnalyticDigitalAmericanEnginePtr;
class AnalyticDigitalAmericanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDigitalAmericanEnginePtr() {
            return new AnalyticDigitalAmericanEnginePtr(
                new AnalyticDigitalAmericanEngine);
        }
    }
};


// Dividend option

%{
using QuantLib::DividendVanillaOption;
typedef boost::shared_ptr<Instrument> DividendVanillaOptionPtr;
%}


%rename(DividendVanillaOption) DividendVanillaOptionPtr;
class DividendVanillaOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        DividendVanillaOptionPtr(
                const boost::shared_ptr<GenericStochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const std::vector<Date>& dividendDates,
                const std::vector<Real>& dividends,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new DividendVanillaOptionPtr(
                   new DividendVanillaOption(process,stPayoff,exercise,
                                             dividendDates,dividends,engine));
        }
        Real delta() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->delta();
        }
        Real gamma() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->gamma();
        }
        Real theta() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->theta();
        }
        Real vega() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->vega();
        }
        Real rho() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->rho();
        }
        Real dividendRho() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                 ->dividendRho();
        }
        Real strikeSensitivity() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                 ->strikeSensitivity();
        }
        Volatility impliedVolatility(Real targetValue,
                                     Real accuracy = 1.0e-4,
                                     Size maxEvaluations = 100,
                                     Volatility minVol = 1.0e-4,
                                     Volatility maxVol = 4.0) {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                 ->impliedVolatility(targetValue,accuracy,maxEvaluations,
                                     minVol,maxVol);
        }
    }
};

%{
using QuantLib::AnalyticDividendEuropeanEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDividendEuropeanEnginePtr;
%}

%rename(AnalyticDividendEuropeanEngine) AnalyticDividendEuropeanEnginePtr;
class AnalyticDividendEuropeanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDividendEuropeanEnginePtr() {
            return new AnalyticDividendEuropeanEnginePtr(
                new AnalyticDividendEuropeanEngine);
        }
    }
};

%{
using QuantLib::FDDividendEuropeanEngine;
using QuantLib::FDDividendAmericanEngine;
typedef boost::shared_ptr<PricingEngine> FDDividendEuropeanEnginePtr;
typedef boost::shared_ptr<PricingEngine> FDDividendAmericanEnginePtr;
%}

%rename(FDDividendEuropeanEngine) FDDividendEuropeanEnginePtr;
class FDDividendEuropeanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDDividendEuropeanEnginePtr(Size timeSteps = 100,
                                    Size gridPoints = 100,
                                    bool timeDependent = false) {
            return new FDDividendEuropeanEnginePtr(
                new FDDividendEuropeanEngine(timeSteps,gridPoints,
                                             timeDependent));
        }
    }
};

%rename(FDDividendAmericanEngine) FDDividendAmericanEnginePtr;
class FDDividendAmericanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDDividendAmericanEnginePtr(Size timeSteps = 100,
                                    Size gridPoints = 100,
                                    bool timeDependent = false) {
            return new FDDividendAmericanEnginePtr(
                new FDDividendAmericanEngine(timeSteps,gridPoints,
                                             timeDependent));
        }
    }
};


// Barrier option

%{
using QuantLib::BarrierOption;
typedef boost::shared_ptr<Instrument> BarrierOptionPtr;
%}

%rename(BarrierOption) BarrierOptionPtr;
class BarrierOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        BarrierOptionPtr(
                   BarrierType barrierType,
                   Real barrier,
                   Real rebate,
                   const boost::shared_ptr<GenericStochasticProcess>& process,
                   const boost::shared_ptr<Payoff>& payoff,
                   const boost::shared_ptr<Exercise>& exercise,
                   const boost::shared_ptr<PricingEngine>& engine
                                     = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new BarrierOptionPtr(
                         new BarrierOption(barrierType, barrier, rebate,
                                           process,stPayoff,exercise,engine));
        }
        Real errorEstimate() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                 ->errorEstimate();
        }
        Real delta() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)->delta();
        }
        Real gamma() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)->gamma();
        }
        Real theta() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)->theta();
        }
        Real vega() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)->vega();
        }
        Real rho() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)->rho();
        }
        Real dividendRho() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                 ->dividendRho();
        }
        Real strikeSensitivity() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                 ->strikeSensitivity();
        }
        Volatility impliedVolatility(Real targetValue,
                                     Real accuracy = 1.0e-4,
                                     Size maxEvaluations = 100,
                                     Volatility minVol = 1.0e-4,
                                     Volatility maxVol = 4.0) {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                 ->impliedVolatility(targetValue,accuracy,maxEvaluations,
                                     minVol,maxVol);
        }
    }
};

// Barrier engines

%{
using QuantLib::AnalyticBarrierEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticBarrierEnginePtr;
%}

%rename(AnalyticBarrierEngine) AnalyticBarrierEnginePtr;
class AnalyticBarrierEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticBarrierEnginePtr() {
            return new AnalyticBarrierEnginePtr(new AnalyticBarrierEngine);
        }
    }
};


#endif
