
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
using QuantLib::OptionPricingEngine;
%}

%template(OptionEngine) Handle<OptionPricingEngine>;


// plain options and engines

%{
using QuantLib::Instruments::PlainOption;
typedef Handle<Instrument> PlainOptionHandle;
std::string OptionDefaultIsinCode = "unknown";
std::string OptionDefaultDescription = "option";
%}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("dividend-rho") dividendRho;
%rename("implied-volatility") impliedVolatility;
#endif

%rename(PlainOption) PlainOptionHandle;
class PlainOptionHandle : public Handle<Instrument> {};

%extend PlainOptionHandle {
    PlainOptionHandle(OptionType type,
                      const RelinkableHandle<MarketElement>& underlying, 
                      double strike,
                      const RelinkableHandle<TermStructure>& dividendYield,
                      const RelinkableHandle<TermStructure>& riskFreeRate,
                      const Date& exerciseDate,
                      const RelinkableHandle<MarketElement>& volatility,
                      const Handle<OptionPricingEngine>& engine,
                      const std::string& isinCode = OptionDefaultIsinCode, 
                      const std::string& desc = OptionDefaultDescription) {
        return new PlainOptionHandle(
            new PlainOption(type,underlying,strike,dividendYield,
                            riskFreeRate,exerciseDate,volatility,
                            engine,isinCode,desc));
    }
    double delta() {
        return Handle<PlainOption>(*self)->delta();
    }
    double gamma() {
        return Handle<PlainOption>(*self)->gamma();
    }
    double theta() {
        return Handle<PlainOption>(*self)->theta();
    }
    double vega() {
        return Handle<PlainOption>(*self)->vega();
    }
    double rho() {
        return Handle<PlainOption>(*self)->rho();
    }
    double dividendRho() {
        return Handle<PlainOption>(*self)->dividendRho();
    }
    double impliedVolatility(double targetValue, double accuracy = 1.0e-4,
                             Size maxEvaluations = 100,
                             double minVol = 1.0e-4, double maxVol = 4.0) {
        return Handle<PlainOption>(*self)->impliedVolatility(
            targetValue,accuracy,maxEvaluations,minVol,maxVol);
    }
}

%{
using QuantLib::Pricers::EuropeanEngine;
typedef Handle<OptionPricingEngine> EuropeanEngineHandle;
%}

%rename(EuropeanEngine) EuropeanEngineHandle;
class EuropeanEngineHandle : public Handle<OptionPricingEngine> {};

%extend EuropeanEngineHandle {
    EuropeanEngineHandle() {
        return new EuropeanEngineHandle(new EuropeanEngine);
    }
}


#endif
