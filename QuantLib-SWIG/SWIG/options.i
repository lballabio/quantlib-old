
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2008 Tito Ingargiola
 Copyright (C) 2010, 2012 Klaus Spanderen
 Copyright (C) 2015 Thema Consulting SA

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

#ifndef quantlib_options_i
#define quantlib_options_i

%include common.i
%include exercise.i
%include stochasticprocess.i
%include instruments.i
%include stl.i
%include linearalgebra.i
%include calibrationhelpers.i

// option and barrier types
%{
using QuantLib::Option;
using QuantLib::Barrier;
%}

// declared out of its hierarchy just to export the inner enumeration
class Option {
  public:
    enum Type { Put = -1, Call = 1};
  private:
    Option();
};

struct Barrier {
    enum Type { DownIn, UpIn, DownOut, UpOut };
};

struct DoubleBarrier {
    enum Type { KnockIn, KnockOut, KIKO, KOKI };
};

// payoff

%{
using QuantLib::Payoff;
using QuantLib::StrikedTypePayoff;
%}

%ignore Payoff;
class Payoff {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    Real operator()(Real price) const;
};

%template(Payoff) boost::shared_ptr<Payoff>;

#if defined(SWIGR)
%Rruntime %{
setMethod("summary", "_p_VanillaOptionPtr",
function(object) {object$freeze()
ans <- c(value=object$NPV(), delta=object$delta(),
gamma=object$gamma(), vega=object$vega(),
theta=object$theta(), rho=object$rho(),
divRho=object$dividendRho())
object$unfreeze()
ans
})

setMethod("summary", "_p_DividendVanillaOptionPtr",
function(object) {object$freeze()
ans <- c(value=object$NPV(), delta=object$delta(),
gamma=object$gamma(), vega=object$vega(),
theta=object$theta(), rho=object$rho(),
divRho=object$dividendRho())
object$unfreeze()
ans
})

%}
#endif


%define add_greeks_to(Type)

%extend Type##Ptr {
    Real delta() {
        return boost::dynamic_pointer_cast<Type>(*self)->delta();
    }
    Real gamma() {
        return boost::dynamic_pointer_cast<Type>(*self)->gamma();
    }
    Real theta() {
        return boost::dynamic_pointer_cast<Type>(*self)->theta();
    }
    Real thetaPerDay() {
        return boost::dynamic_pointer_cast<Type>(*self)->thetaPerDay();
    }
    Real vega() {
        return boost::dynamic_pointer_cast<Type>(*self)->vega();
    }
    Real rho() {
        return boost::dynamic_pointer_cast<Type>(*self)->rho();
    }
    Real dividendRho() {
        return boost::dynamic_pointer_cast<Type>(*self)->dividendRho();
    }
    Real strikeSensitivity() {
        return boost::dynamic_pointer_cast<Type>(*self)->strikeSensitivity();
    }
}

%enddef


// plain option and engines

%{
using QuantLib::VanillaOption;
using QuantLib::ForwardVanillaOption;
typedef boost::shared_ptr<Instrument> VanillaOptionPtr;
typedef boost::shared_ptr<Instrument> MultiAssetOptionPtr;
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
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new VanillaOptionPtr(new VanillaOption(stPayoff,exercise));
        }
        SampledCurve priceCurve() {
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                ->result<SampledCurve>("priceCurve");
        }
        Volatility impliedVolatility(
                             Real targetValue,
                             const GeneralizedBlackScholesProcessPtr& process,
                             Real accuracy = 1.0e-4,
                             Size maxEvaluations = 100,
                             Volatility minVol = 1.0e-4,
                             Volatility maxVol = 4.0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return boost::dynamic_pointer_cast<VanillaOption>(*self)
                ->impliedVolatility(targetValue, bsProcess, accuracy,
                                    maxEvaluations, minVol, maxVol);
        }
    }
};

add_greeks_to(VanillaOption);


%{
using QuantLib::EuropeanOption;
typedef boost::shared_ptr<Instrument> EuropeanOptionPtr;
%}


%rename(EuropeanOption) EuropeanOptionPtr;
class EuropeanOptionPtr : public VanillaOptionPtr {
  public:
    %extend {
        EuropeanOptionPtr(
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new EuropeanOptionPtr(new EuropeanOption(stPayoff,exercise));
        }
    }
};

// ForwardVanillaOption

%{
using QuantLib::ForwardVanillaOption;
typedef boost::shared_ptr<Instrument> ForwardVanillaOptionPtr;
%}

%rename(ForwardVanillaOption) ForwardVanillaOptionPtr;
class ForwardVanillaOptionPtr : public VanillaOptionPtr {
  public:
    %extend {
        ForwardVanillaOptionPtr(
                Real moneyness,
                Date resetDate,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new ForwardVanillaOptionPtr(
                                new ForwardVanillaOption(moneyness, resetDate,
                                                         stPayoff, exercise));
        }
    }
};

// QuantoVanillaOption

%{
using QuantLib::QuantoVanillaOption;
typedef boost::shared_ptr<Instrument> QuantoVanillaOptionPtr;
%}

%rename(QuantoVanillaOption) QuantoVanillaOptionPtr;
class QuantoVanillaOptionPtr : public VanillaOptionPtr {
  public:
    %extend {
        QuantoVanillaOptionPtr(
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new QuantoVanillaOptionPtr(
                                 new QuantoVanillaOption(stPayoff, exercise));
        }
        Real qvega() {
            return boost::dynamic_pointer_cast<QuantoVanillaOption>(*self)
                ->qvega();
        }
        Real qrho() {
            return boost::dynamic_pointer_cast<QuantoVanillaOption>(*self)
                ->qrho();
        }
        Real qlambda() {
            return boost::dynamic_pointer_cast<QuantoVanillaOption>(*self)
                ->qlambda();
        }
    }
};

%{
using QuantLib::QuantoForwardVanillaOption;
typedef boost::shared_ptr<Instrument> QuantoForwardVanillaOptionPtr;
%}

%rename(QuantoForwardVanillaOption) QuantoForwardVanillaOptionPtr;
class QuantoForwardVanillaOptionPtr : public QuantoVanillaOptionPtr {
  public:
    %extend {
        QuantoForwardVanillaOptionPtr(
                Real moneyness,
                Date resetDate,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new QuantoForwardVanillaOptionPtr(
                          new QuantoForwardVanillaOption(moneyness, resetDate,
                                                         stPayoff, exercise));
        }
    }
};

%{
using QuantLib::MultiAssetOption;
%}
%rename(MultiAssetOption) MultiAssetOptionPtr;
class MultiAssetOptionPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        Real delta() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)
                ->delta();
        }
        Real gamma() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)
                ->gamma();
        }
        Real theta() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)
                ->theta();
        }
        Real vega() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)->vega();
        }
        Real rho() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)->rho();
        }
        Real dividendRho() {
            return boost::dynamic_pointer_cast<MultiAssetOption>(*self)
                ->dividendRho();
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
        AnalyticEuropeanEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticEuropeanEnginePtr(
                                       new AnalyticEuropeanEngine(bsProcess));
        }
    }
};


%{
using QuantLib::HestonModel;
typedef boost::shared_ptr<CalibratedModel> HestonModelPtr;
%}

%rename(HestonModel) HestonModelPtr;
class HestonModelPtr : public boost::shared_ptr<CalibratedModel> {
  public:
    %extend {
        HestonModelPtr(const HestonProcessPtr&  process) {

            boost::shared_ptr<HestonProcess> hProcess =
                 boost::dynamic_pointer_cast<HestonProcess>(process);
            QL_REQUIRE(hProcess, "Heston process required");

            return new HestonModelPtr(new HestonModel(hProcess));
        }
        Real theta() const {
            return boost::dynamic_pointer_cast<HestonModel>(*self)->theta();
        }
        Real kappa() const {
            return boost::dynamic_pointer_cast<HestonModel>(*self)->kappa();
        }
        Real sigma() const {
            return boost::dynamic_pointer_cast<HestonModel>(*self)->sigma();
        }
        Real rho() const {
            return boost::dynamic_pointer_cast<HestonModel>(*self)->rho();
        }
        Real v0() const {
            return boost::dynamic_pointer_cast<HestonModel>(*self)->v0();
        }
    }
};

%{
using QuantLib::AnalyticHestonEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticHestonEnginePtr;
%}

%rename(AnalyticHestonEngine) AnalyticHestonEnginePtr;
class AnalyticHestonEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticHestonEnginePtr(const HestonModelPtr& model, 
                                Size integrationOrder = 144) {
            boost::shared_ptr<HestonModel> hModel =
                 boost::dynamic_pointer_cast<HestonModel>(model);
            QL_REQUIRE(hModel, "Heston model required");

            return new AnalyticHestonEnginePtr(
                new AnalyticHestonEngine(hModel, integrationOrder));
        }

        AnalyticHestonEnginePtr(const HestonModelPtr& model, 
                                Real relTolerance,
                                Size maxEvaluations) {
            boost::shared_ptr<HestonModel> hModel =
                 boost::dynamic_pointer_cast<HestonModel>(model);
            QL_REQUIRE(hModel, "Heston model required");

            return new AnalyticHestonEnginePtr(
                new AnalyticHestonEngine(hModel, relTolerance,maxEvaluations));
        }
    }
};


#if defined(SWIGPYTHON)
%rename(lambda_parameter) lambda;
#endif

%{
using QuantLib::BatesModel;
typedef boost::shared_ptr<CalibratedModel> BatesModelPtr;
%}

%rename(BatesModel) BatesModelPtr;
class BatesModelPtr : public HestonModelPtr {
  public:
    %extend {
        BatesModelPtr(const BatesProcessPtr&  process) {

            boost::shared_ptr<BatesProcess> bProcess =
                 boost::dynamic_pointer_cast<BatesProcess>(process);
            QL_REQUIRE(bProcess, "Bates process required");

            return new BatesModelPtr(new BatesModel(bProcess));
        }
        Real nu() const {
            return boost::dynamic_pointer_cast<BatesModel>(*self)->nu();
        }
        Real delta() const {
            return boost::dynamic_pointer_cast<BatesModel>(*self)->delta();
        }
        Real lambda() const {
            return boost::dynamic_pointer_cast<BatesModel>(*self)->lambda();
        }
    }
};


%{
using QuantLib::BatesEngine;
typedef boost::shared_ptr<PricingEngine> BatesEnginePtr;
%}

%rename(BatesEngine) BatesEnginePtr;
class BatesEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BatesEnginePtr(const BatesModelPtr& model, 
                       Size integrationOrder = 144) {
            boost::shared_ptr<BatesModel> bModel =
                 boost::dynamic_pointer_cast<BatesModel>(model);
            QL_REQUIRE(bModel, "Bates model required");

            return new BatesEnginePtr(
                new BatesEngine(bModel, integrationOrder));
        }

        BatesEnginePtr(const BatesModelPtr& model, 
                       Real relTolerance,
                       Size maxEvaluations) {
            boost::shared_ptr<BatesModel> bModel =
                 boost::dynamic_pointer_cast<BatesModel>(model);
            QL_REQUIRE(bModel, "Bates model required");

            return new BatesEnginePtr(
                new BatesEngine(bModel, relTolerance,maxEvaluations));
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
        IntegralEnginePtr(const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new IntegralEnginePtr(new IntegralEngine(bsProcess));
        }
    }
};


%{
using QuantLib::FDBermudanEngine;
typedef boost::shared_ptr<PricingEngine> FDBermudanEnginePtr;
%}

%rename(FDBermudanEngine) FDBermudanEnginePtr;
class FDBermudanEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDBermudanEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                            Size timeSteps = 100, Size gridPoints = 100,
                            bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDBermudanEnginePtr(
                            new FDBermudanEngine<>(bsProcess,timeSteps,
                                                   gridPoints,timeDependent));
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
        FDEuropeanEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                            Size timeSteps = 100, Size gridPoints = 100,
                            bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDEuropeanEnginePtr(
                            new FDEuropeanEngine<>(bsProcess,timeSteps,
                                                   gridPoints,timeDependent));
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
using QuantLib::Joshi4;
typedef boost::shared_ptr<PricingEngine> BinomialVanillaEnginePtr;
%}

%rename(BinomialVanillaEngine) BinomialVanillaEnginePtr;
class BinomialVanillaEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BinomialVanillaEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             const std::string& type,
                             Size steps) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<CoxRossRubinstein>(
                                                            bsProcess,steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<JarrowRudd>(bsProcess,steps));
            else if (s == "eqp" || s == "additiveeqpbinomialtree")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<AdditiveEQPBinomialTree>(
                                                            bsProcess,steps));
            else if (s == "trigeorgis")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<Trigeorgis>(bsProcess,steps));
            else if (s == "tian")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<Tian>(bsProcess,steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<LeisenReimer>(bsProcess,steps));
            else if (s == "j4" || s == "joshi4")
                return new BinomialVanillaEnginePtr(
                    new BinomialVanillaEngine<Joshi4>(bsProcess,steps));
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
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") MCEuropeanEnginePtr;
    #endif
  public:
    %extend {
        MCEuropeanEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                            const std::string& traits,
                            intOrNull timeSteps = Null<Size>(),
                            intOrNull timeStepsPerYear = Null<Size>(),
                            bool brownianBridge = false,
                            bool antitheticVariate = false,
                            intOrNull requiredSamples = Null<Size>(),
                            doubleOrNull requiredTolerance = Null<Real>(),
                            intOrNull maxSamples = Null<Size>(),
                            BigInteger seed = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(traits);
            QL_REQUIRE(Size(timeSteps) != Null<Size>() ||
                       Size(timeStepsPerYear) != Null<Size>(),
                       "number of steps not specified");
            if (s == "pseudorandom" || s == "pr")
                return new MCEuropeanEnginePtr(
                         new MCEuropeanEngine<PseudoRandom>(bsProcess,
                                                            timeSteps,
                                                            timeStepsPerYear,
                                                            brownianBridge,
                                                            antitheticVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCEuropeanEnginePtr(
                       new MCEuropeanEngine<LowDiscrepancy>(bsProcess,
                                                            timeSteps,
                                                            timeStepsPerYear,
                                                            brownianBridge,
                                                            antitheticVariate,
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
        FDAmericanEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                            Size timeSteps = 100, Size gridPoints = 100,
                            bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDAmericanEnginePtr(
                            new FDAmericanEngine<>(bsProcess,timeSteps,
                                                   gridPoints,timeDependent));
        }
    }
};

%{
using QuantLib::FdBlackScholesVanillaEngine;
typedef boost::shared_ptr<PricingEngine> FdBlackScholesVanillaEnginePtr;
%}

%rename(FdBlackScholesVanillaEngine) FdBlackScholesVanillaEnginePtr;
class FdBlackScholesVanillaEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FdBlackScholesVanillaEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                            Size tGrid = 100, Size xGrid = 100, Size dampingSteps = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FdBlackScholesVanillaEnginePtr(
                            new FdBlackScholesVanillaEngine( bsProcess,tGrid, xGrid, dampingSteps));
        }
    }
};


%{
using QuantLib::ContinuousArithmeticAsianLevyEngine;
typedef boost::shared_ptr<PricingEngine> ContinuousArithmeticAsianLevyEnginePtr;
%}

%rename(ContinuousArithmeticAsianLevyEngine) ContinuousArithmeticAsianLevyEnginePtr;
class ContinuousArithmeticAsianLevyEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        ContinuousArithmeticAsianLevyEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                                               const Handle<Quote>& runningAverage,
                                               const Date& startDate) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new ContinuousArithmeticAsianLevyEnginePtr(
                            new ContinuousArithmeticAsianLevyEngine(bsProcess,runningAverage, startDate));
        }
    }
};

%{
using QuantLib::FdBlackScholesAsianEngine;
typedef boost::shared_ptr<PricingEngine> FdBlackScholesAsianEnginePtr;
%}

%rename(FdBlackScholesAsianEngine) FdBlackScholesAsianEnginePtr;
class FdBlackScholesAsianEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FdBlackScholesAsianEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                                     Size tGrid, Size xGrid, Size aGrid) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FdBlackScholesAsianEnginePtr(
                            new FdBlackScholesAsianEngine(bsProcess,tGrid, xGrid, aGrid));
        }
    }
};



%rename(FDShoutEngine) FDShoutEnginePtr;
class FDShoutEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDShoutEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                         Size timeSteps = 100, Size gridPoints = 100,
                         bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDShoutEnginePtr(
                               new FDShoutEngine<>(bsProcess,timeSteps,
                                                   gridPoints,timeDependent));
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
        BaroneAdesiWhaleyApproximationEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new BaroneAdesiWhaleyApproximationEnginePtr(
                         new BaroneAdesiWhaleyApproximationEngine(bsProcess));
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
        BjerksundStenslandApproximationEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new BjerksundStenslandApproximationEnginePtr(
                        new BjerksundStenslandApproximationEngine(bsProcess));
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
        AnalyticDigitalAmericanEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDigitalAmericanEnginePtr(
                                new AnalyticDigitalAmericanEngine(bsProcess));
        }
    }
};

%{
using QuantLib::AnalyticDigitalAmericanKOEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDigitalAmericanKOEnginePtr;
%}

%rename(AnalyticDigitalAmericanKOEngine) AnalyticDigitalAmericanKOEnginePtr;
class AnalyticDigitalAmericanKOEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDigitalAmericanKOEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDigitalAmericanKOEnginePtr(
                                new AnalyticDigitalAmericanKOEngine(bsProcess));
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
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const std::vector<Date>& dividendDates,
                const std::vector<Real>& dividends) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new DividendVanillaOptionPtr(
                          new DividendVanillaOption(stPayoff,exercise,
                                                    dividendDates,dividends));
        }
        SampledCurve priceCurve() {
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->result<SampledCurve>("priceCurve");
        }
        Volatility impliedVolatility(
                             Real targetValue,
                             const GeneralizedBlackScholesProcessPtr& process,
                             Real accuracy = 1.0e-4,
                             Size maxEvaluations = 100,
                             Volatility minVol = 1.0e-4,
                             Volatility maxVol = 4.0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return boost::dynamic_pointer_cast<DividendVanillaOption>(*self)
                ->impliedVolatility(targetValue, bsProcess, accuracy,
                                    maxEvaluations, minVol, maxVol);
        }
    }
};

add_greeks_to(DividendVanillaOption);


%{
using QuantLib::AnalyticDividendEuropeanEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDividendEuropeanEnginePtr;
%}

%rename(AnalyticDividendEuropeanEngine) AnalyticDividendEuropeanEnginePtr;
class AnalyticDividendEuropeanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDividendEuropeanEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDividendEuropeanEnginePtr(
                               new AnalyticDividendEuropeanEngine(bsProcess));
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
        FDDividendEuropeanEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             Size timeSteps = 100,
                             Size gridPoints = 100,
                             bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDDividendEuropeanEnginePtr(
                   new FDDividendEuropeanEngine<>(bsProcess,timeSteps,
                                                  gridPoints, timeDependent));
        }
    }
};

%rename(FDDividendAmericanEngine) FDDividendAmericanEnginePtr;
class FDDividendAmericanEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FDDividendAmericanEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             Size timeSteps = 100,
                             Size gridPoints = 100,
                             bool timeDependent = false) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new FDDividendAmericanEnginePtr(
                   new FDDividendAmericanEngine<>(bsProcess,timeSteps,
                                                  gridPoints, timeDependent));
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
                   Barrier::Type barrierType,
                   Real barrier,
                   Real rebate,
                   const boost::shared_ptr<Payoff>& payoff,
                   const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new BarrierOptionPtr(
                               new BarrierOption(barrierType, barrier, rebate,
                                                 stPayoff,exercise));
        }
        SampledCurve priceCurve() {
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                ->result<SampledCurve>("priceCurve");
        }
        Volatility impliedVolatility(
                             Real targetValue,
                             const GeneralizedBlackScholesProcessPtr& process,
                             Real accuracy = 1.0e-4,
                             Size maxEvaluations = 100,
                             Volatility minVol = 1.0e-4,
                             Volatility maxVol = 4.0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return boost::dynamic_pointer_cast<BarrierOption>(*self)
                 ->impliedVolatility(targetValue, bsProcess, accuracy,
                                     maxEvaluations, minVol, maxVol);
        }
    }
};

add_greeks_to(BarrierOption);


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
        AnalyticBarrierEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticBarrierEnginePtr(
                                        new AnalyticBarrierEngine(bsProcess));
        }
    }
};

%{
using QuantLib::MCBarrierEngine;
typedef boost::shared_ptr<PricingEngine> MCBarrierEnginePtr;
%}

%rename(MCBarrierEngine) MCBarrierEnginePtr;
class MCBarrierEnginePtr : public boost::shared_ptr<PricingEngine> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") MCBarrierEnginePtr;
    #endif
  public:
    %extend {
        MCBarrierEnginePtr(const GeneralizedBlackScholesProcessPtr& process,
                           const std::string& traits,
                           Size timeSteps = Null<Size>(),
                           Size timeStepsPerYear = Null<Size>(),
                           bool brownianBridge = false,
                           bool antitheticVariate = false,
                           intOrNull requiredSamples = Null<Size>(),
                           doubleOrNull requiredTolerance = Null<Real>(),
                           intOrNull maxSamples = Null<Size>(),
                           bool isBiased = false,
                           BigInteger seed = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCBarrierEnginePtr(
                         new MCBarrierEngine<PseudoRandom>(bsProcess,
                                                           timeSteps,
                                                           timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           isBiased,
                                                           seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCBarrierEnginePtr(
                       new MCBarrierEngine<LowDiscrepancy>(bsProcess,
                                                           timeSteps,
                                                           timeStepsPerYear,
                                                           brownianBridge,
                                                           antitheticVariate,
                                                           requiredSamples,
                                                           requiredTolerance,
                                                           maxSamples,
                                                           isBiased,
                                                           seed));
            else
                QL_FAIL("unknown Monte Carlo engine type: "+s);
        }
    }
};

%{
using QuantLib::AnalyticBinaryBarrierEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticBinaryBarrierEnginePtr;
%}

%rename(AnalyticBinaryBarrierEngine) AnalyticBinaryBarrierEnginePtr;
class AnalyticBinaryBarrierEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticBinaryBarrierEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticBinaryBarrierEnginePtr(
                                        new AnalyticBinaryBarrierEngine(bsProcess));
        }
    }
};


%{
using QuantLib::BinomialBarrierEngine;
using QuantLib::DiscretizedDermanKaniBarrierOption;
typedef boost::shared_ptr<PricingEngine> BinomialBarrierEnginePtr;
%}

#if defined(SWIGPYTHON)
%feature("docstring") BinomialBarrierEnginePtr "Binomial Engine for barrier options.
Features different binomial models, selected by the type parameters.
Uses Boyle-Lau adjustment for optimize steps and Derman-Kani optimization to speed
up convergence.
Type values:
    crr or coxrossrubinstein:        Cox-Ross-Rubinstein model
    jr  or jarrowrudd:               Jarrow-Rudd model
    eqp or additiveeqpbinomialtree:  Additive EQP model
    trigeorgis:                      Trigeorgis model
    tian:                            Tian model
    lr  or leisenreimer              Leisen-Reimer model
    j4  or joshi4:                   Joshi 4th (smoothed) model

Boyle-Lau adjustment is controlled by parameter max_steps.
If max_steps is equal to steps Boyle-Lau is disabled.
Il max_steps is 0 (default value), max_steps is calculated by capping it to 
5*steps when Boyle-Lau would need more than 1000 steps.
If max_steps is specified, it would limit binomial steps to this value.
"
#endif
%rename(BinomialBarrierEngine) BinomialBarrierEnginePtr;
class BinomialBarrierEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BinomialBarrierEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             const std::string& type,
                             Size steps,
                             Size max_steps = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<CoxRossRubinstein,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<JarrowRudd,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "eqp" || s == "additiveeqpbinomialtree")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<AdditiveEQPBinomialTree,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "trigeorgis")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<Trigeorgis,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "tian")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<Tian,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<LeisenReimer,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else if (s == "j4" || s == "joshi4")
                return new BinomialBarrierEnginePtr(
                    new BinomialBarrierEngine<Joshi4,
                                          DiscretizedDermanKaniBarrierOption>(
                                                  bsProcess,steps,max_steps));
            else
                QL_FAIL("unknown binomial barrier engine type: "+s);
        }
    }
};

%{
using QuantLib::QuantoEngine;
using QuantLib::ForwardVanillaEngine;
typedef boost::shared_ptr<PricingEngine> ForwardEuropeanEnginePtr;
typedef boost::shared_ptr<PricingEngine> QuantoEuropeanEnginePtr;
typedef boost::shared_ptr<PricingEngine> QuantoForwardEuropeanEnginePtr;
%}


%rename(ForwardEuropeanEngine) ForwardEuropeanEnginePtr;
class ForwardEuropeanEnginePtr: public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        ForwardEuropeanEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new ForwardEuropeanEnginePtr(
                 new ForwardVanillaEngine<AnalyticEuropeanEngine>(bsProcess));
        }
    }
};


%rename(QuantoEuropeanEngine) QuantoEuropeanEnginePtr;
class QuantoEuropeanEnginePtr: public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        QuantoEuropeanEnginePtr(
                  const GeneralizedBlackScholesProcessPtr& process,
                  const Handle<YieldTermStructure>& foreignRiskFreeRate,
                  const Handle<BlackVolTermStructure>& exchangeRateVolatility,
                  const Handle<Quote>& correlation) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new QuantoEuropeanEnginePtr(
                new QuantoEngine<VanillaOption,AnalyticEuropeanEngine>(
                                                       bsProcess,
                                                       foreignRiskFreeRate,
                                                       exchangeRateVolatility,
                                                       correlation));
        }
    }
};

%rename(QuantoForwardEuropeanEngine) QuantoForwardEuropeanEnginePtr;
class QuantoForwardEuropeanEnginePtr: public boost::shared_ptr<PricingEngine> {
public:
    %extend {
        QuantoForwardEuropeanEnginePtr(
                  const GeneralizedBlackScholesProcessPtr& process,
                  const Handle<YieldTermStructure>& foreignRiskFreeRate,
                  const Handle<BlackVolTermStructure>& exchangeRateVolatility,
                  const Handle<Quote>& correlation) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new QuantoForwardEuropeanEnginePtr(
                new QuantoEngine<ForwardVanillaOption,AnalyticEuropeanEngine>(
                                                       bsProcess,
                                                       foreignRiskFreeRate,
                                                       exchangeRateVolatility,
                                                       correlation));
        }
    }
};

%{
using QuantLib::BlackCalculator;
%}

class BlackCalculator {
  public:
    %extend {
        BlackCalculator (
                   const boost::shared_ptr<Payoff>& payoff,
                   Real forward,
                   Real stdDev,
                   Real discount = 1.0) {

            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);

            QL_REQUIRE(stPayoff, "wrong payoff given");

            return new BlackCalculator(stPayoff,forward,stdDev,discount);
        }
    }
    Real value() const;
    Real deltaForward() const;
    Real delta(Real spot) const;
    Real elasticityForward() const;
    Real elasticity(Real spot) const;
    Real gammaForward() const;
    Real gamma(Real spot) const;
    Real theta(Real spot, Time maturity) const;
    Real thetaPerDay(Real spot, Time maturity) const;
    Real vega(Time maturity) const;
    Real rho(Time maturity) const;
    Real dividendRho(Time maturity) const;
    Real itmCashProbability() const;
    Real itmAssetProbability() const;
    Real strikeSensitivity() const;
    Real alpha() const;
    Real beta() const;
};



// Asian options

%{
using QuantLib::Average;
using QuantLib::ContinuousAveragingAsianOption;
using QuantLib::DiscreteAveragingAsianOption;
typedef boost::shared_ptr<Instrument> ContinuousAveragingAsianOptionPtr;
typedef boost::shared_ptr<Instrument> DiscreteAveragingAsianOptionPtr;
%}

struct Average {
    enum Type { Arithmetic, Geometric };
};

%rename(ContinuousAveragingAsianOption) ContinuousAveragingAsianOptionPtr;
class ContinuousAveragingAsianOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    #endif
  public:
    %extend {
        ContinuousAveragingAsianOptionPtr(
                Average::Type averageType,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new ContinuousAveragingAsianOptionPtr(
                       new ContinuousAveragingAsianOption(averageType,
                                                          stPayoff,exercise));
        }
    }
};

add_greeks_to(ContinuousAveragingAsianOption);


%rename(DiscreteAveragingAsianOption) DiscreteAveragingAsianOptionPtr;
class DiscreteAveragingAsianOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    #endif
  public:
    %extend {
        DiscreteAveragingAsianOptionPtr(
                Average::Type averageType,
                Real runningAccumulator,
                Size pastFixings,
                const std::vector<Date>& fixingDates,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new DiscreteAveragingAsianOptionPtr(
                          new DiscreteAveragingAsianOption(averageType,
                                                           runningAccumulator,
                                                           pastFixings,
                                                           fixingDates,
                                                           stPayoff,
                                                           exercise));
        }
    }
};

add_greeks_to(DiscreteAveragingAsianOption);


// Asian engines


%{
using QuantLib::AnalyticContinuousGeometricAveragePriceAsianEngine;
typedef boost::shared_ptr<PricingEngine>
    AnalyticContinuousGeometricAveragePriceAsianEnginePtr;
%}

%rename(AnalyticContinuousGeometricAveragePriceAsianEngine)
        AnalyticContinuousGeometricAveragePriceAsianEnginePtr;
class AnalyticContinuousGeometricAveragePriceAsianEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticContinuousGeometricAveragePriceAsianEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticContinuousGeometricAveragePriceAsianEnginePtr(
                new AnalyticContinuousGeometricAveragePriceAsianEngine(
                                                                  bsProcess));
        }
    }
};


%{
using QuantLib::AnalyticDiscreteGeometricAveragePriceAsianEngine;
typedef boost::shared_ptr<PricingEngine>
    AnalyticDiscreteGeometricAveragePriceAsianEnginePtr;
%}

%rename(AnalyticDiscreteGeometricAveragePriceAsianEngine)
        AnalyticDiscreteGeometricAveragePriceAsianEnginePtr;
class AnalyticDiscreteGeometricAveragePriceAsianEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDiscreteGeometricAveragePriceAsianEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDiscreteGeometricAveragePriceAsianEnginePtr(
                new AnalyticDiscreteGeometricAveragePriceAsianEngine(
                                                                  bsProcess));
        }
    }
};


%{
using QuantLib::AnalyticDiscreteGeometricAverageStrikeAsianEngine;
typedef boost::shared_ptr<PricingEngine>
    AnalyticDiscreteGeometricAverageStrikeAsianEnginePtr;
%}

%rename(AnalyticDiscreteGeometricAverageStrikeAsianEngine)
        AnalyticDiscreteGeometricAverageStrikeAsianEnginePtr;
class AnalyticDiscreteGeometricAverageStrikeAsianEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDiscreteGeometricAverageStrikeAsianEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDiscreteGeometricAverageStrikeAsianEnginePtr(
                new AnalyticDiscreteGeometricAverageStrikeAsianEngine(
                                                                  bsProcess));
        }
    }
};



%{
using QuantLib::MCDiscreteArithmeticAPEngine;
typedef boost::shared_ptr<PricingEngine> MCDiscreteArithmeticAPEnginePtr;
%}

%rename(MCDiscreteArithmeticAPEngine) MCDiscreteArithmeticAPEnginePtr;
class MCDiscreteArithmeticAPEnginePtr
    : public boost::shared_ptr<PricingEngine> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") MCDiscreteArithmeticAPEnginePtr;
    #endif
  public:
    %extend {
        MCDiscreteArithmeticAPEnginePtr(
                            const GeneralizedBlackScholesProcessPtr& process,
                            const std::string& traits,
                            bool brownianBridge = false,
                            bool antitheticVariate = false,
                            bool controlVariate = false,
                            intOrNull requiredSamples = Null<Size>(),
                            doubleOrNull requiredTolerance = Null<Real>(),
                            intOrNull maxSamples = Null<Size>(),
                            BigInteger seed = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCDiscreteArithmeticAPEnginePtr(
                         new MCDiscreteArithmeticAPEngine<PseudoRandom>(
                                                            bsProcess,
                                                            brownianBridge,
                                                            antitheticVariate,
                                                            controlVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCDiscreteArithmeticAPEnginePtr(
                       new MCDiscreteArithmeticAPEngine<LowDiscrepancy>(
                                                            bsProcess,
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
using QuantLib::MCDiscreteArithmeticASEngine;
typedef boost::shared_ptr<PricingEngine> MCDiscreteArithmeticASEnginePtr;
%}

%rename(MCDiscreteArithmeticASEngine) MCDiscreteArithmeticASEnginePtr;
class MCDiscreteArithmeticASEnginePtr
    : public boost::shared_ptr<PricingEngine> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") MCDiscreteArithmeticASEnginePtr;
    #endif
  public:
    %extend {
        MCDiscreteArithmeticASEnginePtr(
                            const GeneralizedBlackScholesProcessPtr& process,
                            const std::string& traits,
                            bool brownianBridge = false,
                            bool antitheticVariate = false,
                            intOrNull requiredSamples = Null<Size>(),
                            doubleOrNull requiredTolerance = Null<Real>(),
                            intOrNull maxSamples = Null<Size>(),
                            BigInteger seed = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCDiscreteArithmeticASEnginePtr(
                         new MCDiscreteArithmeticASEngine<PseudoRandom>(
                                                            bsProcess,
                                                            brownianBridge,
                                                            antitheticVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCDiscreteArithmeticASEnginePtr(
                       new MCDiscreteArithmeticASEngine<LowDiscrepancy>(
                                                            bsProcess,
                                                            brownianBridge,
                                                            antitheticVariate,
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
using QuantLib::MCDiscreteGeometricAPEngine;
typedef boost::shared_ptr<PricingEngine> MCDiscreteGeometricAPEnginePtr;
%}

%rename(MCDiscreteGeometricAPEngine) MCDiscreteGeometricAPEnginePtr;
class MCDiscreteGeometricAPEnginePtr
    : public boost::shared_ptr<PricingEngine> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") MCDiscreteGeometricAPEnginePtr;
    #endif
  public:
    %extend {
        MCDiscreteGeometricAPEnginePtr(
                            const GeneralizedBlackScholesProcessPtr& process,
                            const std::string& traits,
                            bool brownianBridge = false,
                            bool antitheticVariate = false,
                            intOrNull requiredSamples = Null<Size>(),
                            doubleOrNull requiredTolerance = Null<Real>(),
                            intOrNull maxSamples = Null<Size>(),
                            BigInteger seed = 0) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(traits);
            if (s == "pseudorandom" || s == "pr")
                return new MCDiscreteGeometricAPEnginePtr(
                         new MCDiscreteGeometricAPEngine<PseudoRandom>(
                                                            bsProcess,
                                                            brownianBridge,
                                                            antitheticVariate,
                                                            requiredSamples,
                                                            requiredTolerance,
                                                            maxSamples,
                                                            seed));
            else if (s == "lowdiscrepancy" || s == "ld")
                return new MCDiscreteGeometricAPEnginePtr(
                       new MCDiscreteGeometricAPEngine<LowDiscrepancy>(
                                                            bsProcess,
                                                            brownianBridge,
                                                            antitheticVariate,
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
using QuantLib::VarianceGammaEngine;
typedef boost::shared_ptr<PricingEngine>
    VarianceGammaEnginePtr;
%}

%rename(VarianceGammaEngine) VarianceGammaEnginePtr;
class VarianceGammaEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        VarianceGammaEnginePtr(const VarianceGammaProcessPtr& process) {
            boost::shared_ptr<VarianceGammaProcess> vgProcess =
                boost::dynamic_pointer_cast<VarianceGammaProcess>(process);
            QL_REQUIRE(vgProcess, "Variance-Gamma process required");
            return new VarianceGammaEnginePtr(new VarianceGammaEngine(vgProcess));
        }
    }
};

%{
using QuantLib::FFTVarianceGammaEngine;
typedef boost::shared_ptr<PricingEngine>
    FFTVarianceGammaEnginePtr;
%}

%rename(FFTVarianceGammaEngine) FFTVarianceGammaEnginePtr;
class FFTVarianceGammaEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        FFTVarianceGammaEnginePtr(const VarianceGammaProcessPtr& process, Real logStrikeSpacing = 0.001) {
            boost::shared_ptr<VarianceGammaProcess> vgProcess =
                boost::dynamic_pointer_cast<VarianceGammaProcess>(process);
            QL_REQUIRE(vgProcess, "Variance Gamma process required");
            return new FFTVarianceGammaEnginePtr(new FFTVarianceGammaEngine(vgProcess, logStrikeSpacing));
        }
        void precalculate(const std::vector<boost::shared_ptr<Instrument> >& optionList)
        {
            boost::dynamic_pointer_cast<FFTVarianceGammaEngine>(*self)->precalculate(optionList);
        }
    }
};

// Double barrier options
%{
using QuantLib::DoubleBarrierOption;
using QuantLib::DoubleBarrier;
%}

%{
using QuantLib::DoubleBarrierOption;
typedef boost::shared_ptr<Instrument> DoubleBarrierOptionPtr;
%}

%rename(DoubleBarrierOption) DoubleBarrierOptionPtr;
class DoubleBarrierOptionPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
    %extend {
        DoubleBarrierOptionPtr(
                   DoubleBarrier::Type barrierType,
                   Real barrier_lo,
                   Real barrier_hi,
                   Real rebate,
                   const boost::shared_ptr<Payoff>& payoff,
                   const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new DoubleBarrierOptionPtr(
                               new DoubleBarrierOption(barrierType, barrier_lo, 
                                                 barrier_hi, rebate,
                                                 stPayoff,exercise));
        }
    }
};

add_greeks_to(DoubleBarrierOption);

// QuantoVanillaOption

%{
using QuantLib::QuantoDoubleBarrierOption;
typedef boost::shared_ptr<Instrument> QuantoDoubleBarrierOptionPtr;
%}

%rename(QuantoDoubleBarrierOption) QuantoDoubleBarrierOptionPtr;
class QuantoDoubleBarrierOptionPtr : public DoubleBarrierOptionPtr {
  public:
    %extend {
        QuantoDoubleBarrierOptionPtr(
                DoubleBarrier::Type barrierType,
                Real barrier_lo,
                Real barrier_hi,
                Real rebate,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise) {
            boost::shared_ptr<StrikedTypePayoff> stPayoff =
                 boost::dynamic_pointer_cast<StrikedTypePayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new QuantoDoubleBarrierOptionPtr(
                         new QuantoDoubleBarrierOption(barrierType, barrier_lo, 
                                      barrier_hi, rebate, stPayoff, exercise));
        }
        Real qvega() {
            return boost::dynamic_pointer_cast<QuantoDoubleBarrierOption>(*self)
                ->qvega();
        }
        Real qrho() {
            return boost::dynamic_pointer_cast<QuantoDoubleBarrierOption>(*self)
                ->qrho();
        }
        Real qlambda() {
            return boost::dynamic_pointer_cast<QuantoDoubleBarrierOption>(*self)
                ->qlambda();
        }
    }
};

// Double Barrier engines

%{
using QuantLib::AnalyticDoubleBarrierEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDoubleBarrierEnginePtr;
%}

#if defined(SWIGPYTHON)
%feature("docstring") AnalyticDoubleBarrierEnginePtr "Double barrier engine implementing Ikeda-Kunitomo series."
#endif
%rename(AnalyticDoubleBarrierEngine) AnalyticDoubleBarrierEnginePtr;
class AnalyticDoubleBarrierEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDoubleBarrierEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process,
                           int series = 5) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDoubleBarrierEnginePtr(
                            new AnalyticDoubleBarrierEngine(bsProcess, series));
        }
    }
};

%{
using QuantLib::WulinYongDoubleBarrierEngine;
typedef boost::shared_ptr<PricingEngine> WulinYongDoubleBarrierEnginePtr;
%}

%rename(WulinYongDoubleBarrierEngine) WulinYongDoubleBarrierEnginePtr;
class WulinYongDoubleBarrierEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        WulinYongDoubleBarrierEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process,
                           int series = 5) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new WulinYongDoubleBarrierEnginePtr(
                            new WulinYongDoubleBarrierEngine(bsProcess, series));
        }
    }
};

%{
using QuantLib::VannaVolgaDoubleBarrierEngine;
using QuantLib::DeltaVolQuote;
typedef boost::shared_ptr<PricingEngine> VannaVolgaDoubleBarrierEnginePtr;
%}

%template(DeltaVolQuoteHandle) Handle<DeltaVolQuote>;
IsObservable(Handle<DeltaVolQuote>);
%template(RelinkableDeltaVolQuoteHandle)
RelinkableHandle<DeltaVolQuote>;

#if defined(SWIGPYTHON)
%feature("docstring") VannaVolgaDoubleBarrierEnginePtr "
Vanna-Volga engine for double barrier options.
Supports different double barrier engines, selected by the type parameters.
Type values:
    ik or analytic:  Ikeda-Kunitomo standard engine (default)
    wo:              Wulin-Yong engine
"
#endif
%rename(VannaVolgaDoubleBarrierEngine) VannaVolgaDoubleBarrierEnginePtr;
class VannaVolgaDoubleBarrierEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        VannaVolgaDoubleBarrierEnginePtr(
                           const Handle<DeltaVolQuote> atmVol,
                           const Handle<DeltaVolQuote> vol25Put,
                           const Handle<DeltaVolQuote> vol25Call,
                           const Handle<Quote> spotFX,
                           const Handle<YieldTermStructure> domesticTS,
                           const Handle<YieldTermStructure> foreignTS,
                           const std::string& type = "ik",
                           const bool adaptVanDelta = false,
                           const Real bsPriceWithSmile = 0.0,
                           int series = 5) {
            std::string s = boost::algorithm::to_lower_copy(type);
            if (s == "ik" || s == "analytic")
                return new VannaVolgaDoubleBarrierEnginePtr(
                   new VannaVolgaDoubleBarrierEngine<AnalyticDoubleBarrierEngine>(
                                        atmVol, vol25Put, vol25Call, spotFX, 
                                        domesticTS, foreignTS, adaptVanDelta, 
                                        bsPriceWithSmile, series));
            else if (s == "wo")
                return new VannaVolgaDoubleBarrierEnginePtr(
                   new VannaVolgaDoubleBarrierEngine<WulinYongDoubleBarrierEngine>(
                                        atmVol, vol25Put, vol25Call, spotFX, 
                                        domesticTS, foreignTS, adaptVanDelta, 
                                        bsPriceWithSmile, series));
            else
                QL_FAIL("unknown binomial engine type: "+s);
        }
    }
};

%{
using QuantLib::AnalyticDoubleBarrierBinaryEngine;
typedef boost::shared_ptr<PricingEngine> AnalyticDoubleBarrierBinaryEnginePtr;
%}

%rename(AnalyticDoubleBarrierBinaryEngine) AnalyticDoubleBarrierBinaryEnginePtr;
class AnalyticDoubleBarrierBinaryEnginePtr
    : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticDoubleBarrierBinaryEnginePtr(
                           const GeneralizedBlackScholesProcessPtr& process) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            return new AnalyticDoubleBarrierBinaryEnginePtr(
                            new AnalyticDoubleBarrierBinaryEngine(bsProcess));
        }
    }
};

%{
using QuantLib::BinomialDoubleBarrierEngine;
using QuantLib::DiscretizedDermanKaniDoubleBarrierOption;
typedef boost::shared_ptr<PricingEngine> BinomialDoubleBarrierEnginePtr;
%}

#if defined(SWIGPYTHON)
%feature("docstring") BinomialDoubleBarrierEnginePtr "Binomial Engine for double barrier options.
Features different binomial models, selected by the type parameters.
Uses Derman-Kani optimization to speed up convergence.
Type values:
    crr or coxrossrubinstein:        Cox-Ross-Rubinstein model
    jr  or jarrowrudd:               Jarrow-Rudd model
    eqp or additiveeqpbinomialtree:  Additive EQP model
    trigeorgis:                      Trigeorgis model
    tian:                            Tian model
    lr  or leisenreimer              Leisen-Reimer model
    j4  or joshi4:                   Joshi 4th (smoothed) model
"
#endif
%rename(BinomialDoubleBarrierEngine) BinomialDoubleBarrierEnginePtr;
class BinomialDoubleBarrierEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BinomialDoubleBarrierEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             const std::string& type,
                             Size steps) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<CoxRossRubinstein,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<JarrowRudd,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "eqp" || s == "additiveeqpbinomialtree")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<AdditiveEQPBinomialTree,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "trigeorgis")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<Trigeorgis,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "tian")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<Tian,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<LeisenReimer,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else if (s == "j4" || s == "joshi4")
                return new BinomialDoubleBarrierEnginePtr(
                    new BinomialDoubleBarrierEngine<Joshi4,
                                     DiscretizedDermanKaniDoubleBarrierOption>(
                                                  bsProcess,steps));
            else
                QL_FAIL("unknown binomial double barrier engine type: "+s);
        }
    }
};

#endif
