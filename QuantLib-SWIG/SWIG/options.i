
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_options_i
#define quantlib_options_i

%include common.i
%include instruments.i
%include marketelements.i
%include termstructures.i
%include stl.i

// typemap option types to corresponding strings

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


// option pricing engines

%{
using QuantLib::PricingEngine;
%}

%template(PricingEngine) Handle<PricingEngine>;


// plain options and engines

%{
using QuantLib::Instruments::VanillaOption;
typedef Handle<Instrument> VanillaOptionHandle;
%}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("dividend-rho") dividendRho;
%rename("implied-volatility") impliedVolatility;
#endif

%rename(VanillaOption) VanillaOptionHandle;
class VanillaOptionHandle : public Handle<Instrument> {};

%extend VanillaOptionHandle {
    VanillaOptionHandle(OptionType type,
                        const RelinkableHandle<MarketElement>& underlying, 
                        double strike,
                        const RelinkableHandle<TermStructure>& dividendYield,
                        const RelinkableHandle<TermStructure>& riskFreeRate,
                        const Date& exerciseDate,
                        const RelinkableHandle<MarketElement>& volatility,
                        const Handle<PricingEngine>& engine,
                        const std::string& isinCode = "unknown", 
                        const std::string& desc = "option") {
        return new VanillaOptionHandle(
            new VanillaOption(type,underlying,strike,dividendYield,
                              riskFreeRate,exerciseDate,volatility,
                              engine,isinCode,desc));
    }
    double delta() {
        return Handle<VanillaOption>(*self)->delta();
    }
    double gamma() {
        return Handle<VanillaOption>(*self)->gamma();
    }
    double theta() {
        return Handle<VanillaOption>(*self)->theta();
    }
    double vega() {
        return Handle<VanillaOption>(*self)->vega();
    }
    double rho() {
        return Handle<VanillaOption>(*self)->rho();
    }
    double dividendRho() {
        return Handle<VanillaOption>(*self)->dividendRho();
    }
    double impliedVolatility(double targetValue, double accuracy = 1.0e-4,
                             Size maxEvaluations = 100,
                             double minVol = 1.0e-4, double maxVol = 4.0) {
        return Handle<VanillaOption>(*self)->impliedVolatility(
            targetValue,accuracy,maxEvaluations,minVol,maxVol);
    }
}


// European engines

%{
using QuantLib::PricingEngines::EuropeanAnalyticalEngine;
typedef Handle<PricingEngine> EuropeanAnalyticEngineHandle;
%}

%rename(EuropeanAnalyticEngine) EuropeanAnalyticEngineHandle;
class EuropeanAnalyticEngineHandle : public Handle<PricingEngine> {};

%extend EuropeanAnalyticEngineHandle {
    EuropeanAnalyticEngineHandle() {
        return new EuropeanAnalyticEngineHandle(new EuropeanAnalyticalEngine);
    }
}


%{
using QuantLib::PricingEngines::EuropeanBinomialEngine;
typedef EuropeanBinomialEngine::Type BinomialEngineType;
typedef Handle<PricingEngine> EuropeanBinomialEngineHandle;

EuropeanBinomialEngine::Type binomialEngineTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "crr" || s == "coxrossrubinstein")
        return EuropeanBinomialEngine::CoxRossRubinstein;
    else if (s == "jr" || s == "jarrowrudd")
        return EuropeanBinomialEngine::JarrowRudd;
    else if (s == "lr" || s == "leisenreimer")
        return EuropeanBinomialEngine::LeisenReimer;
    else
        throw Error("unknown binomial engine type: "+s);
}

std::string binomialEngineTypeToString(EuropeanBinomialEngine::Type t) {
    switch (t) {
      case EuropeanBinomialEngine::CoxRossRubinstein:
        return "CoxRossRubinstein";
      case EuropeanBinomialEngine::JarrowRudd:
        return "JarrowRudd";
      case EuropeanBinomialEngine::LeisenReimer:
        return "LeisenReimer";
      default:
        throw Error("unknown binomial engine type");
    }
}
%}

MapToString(BinomialEngineType,binomialEngineTypeFromString,
            binomialEngineTypeToString);

%rename(EuropeanBinomialEngine) EuropeanBinomialEngineHandle;
class EuropeanBinomialEngineHandle : public Handle<PricingEngine> {};

%extend EuropeanBinomialEngineHandle {
    EuropeanBinomialEngineHandle(BinomialEngineType type,
                                 Size steps) {
        return new EuropeanBinomialEngineHandle
            (new EuropeanBinomialEngine(type,steps));
    }
}



#endif
