
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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
%include volatilities.i
%include stl.i

// exercise conditions
%{
using QuantLib::Exercise;
typedef Exercise::Type ExerciseType;
using QuantLib::EuropeanExercise;
using QuantLib::AmericanExercise;
using QuantLib::BermudanExercise;

Exercise::Type exerciseTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "e" || s == "european")
        return Exercise::European;
    else if (s == "a" || s == "american")
        return Exercise::American;
    else if (s == "b" || s == "bermudan")
        return Exercise::Bermudan;
    else
        throw Error("unknown exercise type: "+s);
}

std::string exerciseTypeToString(Exercise::Type t) {
    switch (t) {
      case Exercise::European:
        return "European";
      case Exercise::American:
        return "American";
      case Exercise::Bermudan:
        return "Bermudan";
      default:
        throw Error("unknown exercise type");
    }
}
%}

MapToString(ExerciseType,exerciseTypeFromString,exerciseTypeToString);

class Exercise {
  public:
    ExerciseType type() const;
    std::vector<Date> dates() const;
  private:
    Exercise();
};
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
// no virtual destructor in base class - prevent mismatch
%inline %{
    Exercise* new_EuropeanExercise(const Date& date) {
        return new Exercise(EuropeanExercise(date));
    }
    Exercise* new_AmericanExercise(const Date& earliestDate, 
                                   const Date& latestDate) {
        std::vector<Date> v(2);
        v[0] = earliestDate; v[1] = latestDate;
        return new Exercise(AmericanExercise(earliestDate,latestDate));
    }
    Exercise* new_BermudanExercise(const std::vector<Date>& dates) {
        return new Exercise(BermudanExercise(dates));
    }
%}
#else
// use inheritance
class EuropeanExercise : public Exercise {
  public:
    EuropeanExercise(const Date& date);
};
class AmericanExercise : public Exercise {
  public:
    AmericanExercise(const Date& earliestDate, 
                     const Date& latestDate);
};
class BermudanExercise : public Exercise {
  public:
    BermudanExercise(const std::vector<Date>& dates);
};
#endif

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


%rename(VanillaOption) VanillaOptionHandle;
class VanillaOptionHandle : public Handle<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        VanillaOptionHandle(
                OptionType type,
                const RelinkableHandle<MarketElement>& underlying, 
                double strike,
                const RelinkableHandle<TermStructure>& dividendYield,
                const RelinkableHandle<TermStructure>& riskFreeRate,
                const Exercise& exerciseDate,
                const RelinkableHandle<BlackVolTermStructure>& volatility,
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
};


// European engines

%{
using QuantLib::PricingEngines::AnalyticalVanillaEngine;
typedef Handle<PricingEngine> AnalyticalVanillaEngineHandle;
%}

%rename(AnalyticEuropeanEngine) AnalyticalVanillaEngineHandle;
class AnalyticalVanillaEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        AnalyticalVanillaEngineHandle() {
            return new AnalyticalVanillaEngineHandle(
                new AnalyticalVanillaEngine);
        }
    }
};


%{
using QuantLib::PricingEngines::BinomialVanillaEngine;
typedef BinomialVanillaEngine::Type BinomialEngineType;
typedef Handle<PricingEngine> BinomialVanillaEngineHandle;

BinomialVanillaEngine::Type binomialEngineTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "crr" || s == "coxrossrubinstein")
        return BinomialVanillaEngine::CoxRossRubinstein;
    else if (s == "jr" || s == "jarrowrudd")
        return BinomialVanillaEngine::JarrowRudd;
    else if (s == "eqp")
        return BinomialVanillaEngine::EQP;
    else if (s == "trigeorgis")
        return BinomialVanillaEngine::Trigeorgis;
    else if (s == "tian")
        return BinomialVanillaEngine::Tian;
    else
        throw Error("unknown binomial engine type: "+s);
}

std::string binomialEngineTypeToString(BinomialVanillaEngine::Type t) {
    switch (t) {
      case BinomialVanillaEngine::CoxRossRubinstein:
        return "CoxRossRubinstein";
      case BinomialVanillaEngine::JarrowRudd:
        return "JarrowRudd";
      case BinomialVanillaEngine::EQP:
        return "EQP";
      case BinomialVanillaEngine::Trigeorgis:
        return "Trigeorgis";
      case BinomialVanillaEngine::Tian:
        return "Tian";
      default:
        throw Error("unknown binomial engine type");
    }
}
%}

MapToString(BinomialEngineType,binomialEngineTypeFromString,
            binomialEngineTypeToString);

%rename(BinomialEuropeanEngine) BinomialVanillaEngineHandle;
class BinomialVanillaEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        BinomialVanillaEngineHandle(BinomialEngineType type,
                                    Size steps) {
            return new BinomialVanillaEngineHandle(
                new BinomialVanillaEngine(type,steps));
        }
    }
};


#endif
