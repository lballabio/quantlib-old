/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006 Marco Bianchetti
 Copyright (C) 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Chiara Fornarola
 Copyright (C) 2007 Katiuscia Manzoni
 Copyright (C) 2005 Plamen Neykov

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

#include <qlo/Enumerations/Constructors/enumeratedclasses.hpp>
#include <qlo/Conversions/conversions.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/indexes/ibor/eurlibor.hpp>
#include <ql/indexes/swap/euriborswapfixa.hpp>
#include <ql/indexes/swap/euriborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixa.hpp>
#include <ql/indexes/swap/eurliborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixifr.hpp>
#include <ql/indexes/swap/euriborswapfixifr.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/instruments/vanillaoption.hpp>

namespace QuantLibAddin {

    /* *** StrikedTypePayoff *** */
    /* *** Option::Type + 1 parameter *** */
    boost::shared_ptr<QuantLib::Payoff> VANILLA_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::PlainVanillaPayoff(optionType, strike));
    }
    boost::shared_ptr<QuantLib::Payoff> ASSETORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::AssetOrNothingPayoff(optionType, strike));
    }
    boost::shared_ptr<QuantLib::Payoff> PERCENTAGESTRIKE_Payoff(
            const QuantLib::Option::Type& optionType,
            const double moneyness) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::PercentageStrikePayoff(optionType, moneyness));
    }
    /* *** Option::Type + 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> CASHORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double cashPayoff) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::CashOrNothingPayoff(optionType, strike, cashPayoff));
    }
    boost::shared_ptr<QuantLib::Payoff> GAP_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double secondStrike) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::GapPayoff(optionType, strike, secondStrike));
    }
    /* *** 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERFUND_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::SuperFundPayoff(strike, secondStrike));
    }
    /* *** 3 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERSHARE_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike,
            const double cashPayoff) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::SuperSharePayoff(strike, secondStrike, cashPayoff));
    }

    /* *** PricingEngines - without timesteps *** */
    boost::shared_ptr<QuantLib::PricingEngine> AB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticBarrierEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> AC_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticCliquetEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> ACGAPA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticContinuousGeometricAveragePriceAsianEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDigitalAmericanEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDividendEuropeanEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADGAPA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDiscreteGeometricAveragePriceAsianEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> AE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticEuropeanEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> AP_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticPerformanceEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> BAWA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BaroneAdesiWhaleyApproximationEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> BSA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BjerksundStenslandApproximationEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> I_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::IntegralEngine(process));
    }
    boost::shared_ptr<QuantLib::PricingEngine> PE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine>();
    }
    boost::shared_ptr<QuantLib::PricingEngine> SE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::StulzEngine(process, process, 0));//FIXME dummy inputs
    }
    // FIXME these seem not to work following pricing engines redesign?
    //boost::shared_ptr<QuantLib::PricingEngine> FE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
    //    boost::shared_ptr<QuantLib::VanillaOption::engine>
    //        underlyingEngine(new QuantLib::AnalyticEuropeanEngine(process));
    //    return boost::shared_ptr<QuantLib::PricingEngine> (
    //        new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
    //            QuantLib::VanillaOption::results>(underlyingEngine));
    //}
    //boost::shared_ptr<QuantLib::PricingEngine> FPE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
    //    boost::shared_ptr<QuantLib::VanillaOption::engine>
    //        underlyingEngine(new QuantLib::AnalyticEuropeanEngine(process));
    //    return boost::shared_ptr<QuantLib::PricingEngine> (
    //        new QuantLib::ForwardPerformanceEngine
    //            <QuantLib::VanillaOption::arguments,
    //            QuantLib::VanillaOption::results>(underlyingEngine));
    //}
    //boost::shared_ptr<QuantLib::PricingEngine> QE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
    //    boost::shared_ptr<QuantLib::VanillaOption::engine>
    //        underlyingEngine(new QuantLib::AnalyticEuropeanEngine(process));
    //    return boost::shared_ptr<QuantLib::PricingEngine> (
    //        new QuantLib::QuantoEngine<QuantLib::VanillaOption,
    //            QuantLib::PricingEngine>(underlyingEngine));
    //}
    //boost::shared_ptr<QuantLib::PricingEngine> QFE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {
    //    boost::shared_ptr<QuantLib::VanillaOption::engine>
    //        underlyingEngine(new QuantLib::AnalyticEuropeanEngine(process));
    //    boost::shared_ptr<QuantLib::ForwardVanillaOption::engine> forwardEngine(
    //            new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
    //            QuantLib::VanillaOption::results>(underlyingEngine));
    //    return boost::shared_ptr<QuantLib::PricingEngine> (
    //        new QuantLib::QuantoEngine<QuantLib::ForwardVanillaOption::arguments,
    //            QuantLib::ForwardVanillaOption::results>(forwardEngine));
    //}

    /* *** PricingEngines - with timesteps *** */
    boost::shared_ptr<QuantLib::PricingEngine> AEQPB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::AdditiveEQPBinomialTree>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> CRR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::CoxRossRubinstein>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDAmericanEngine(process, timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDBermudanEngine(process, timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDEuropeanEngine(process, timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> JOSHI_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Joshi4>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> JR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::JarrowRudd>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> LR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::LeisenReimer>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TIAN_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Tian>(process, timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TRI_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Trigeorgis>(process, timeSteps));
    }

    /* *** Linear 1D Interpolation *** */
    boost::shared_ptr<QuantLib::Interpolation> LINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::LinearInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> BACKWARDFLAT_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::BackwardFlatInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> FORWARDFLAT_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::ForwardFlatInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> LOGLINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::LogLinearInterpolation(
                xBegin, xEnd, yBegin));
    }

    /* *** Interpolation2D *** */
    boost::shared_ptr<QuantLib::Interpolation2D> BILINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin, ObjectHandler::dbl_itr& yEnd,
            const QuantLib::Matrix& zData) {
        return boost::shared_ptr<QuantLib::Interpolation2D>(
            new QuantLib::BilinearInterpolation(
                xBegin, xEnd, yBegin, yEnd, zData));
    }
    boost::shared_ptr<QuantLib::Interpolation2D> BICUBICSPLINE(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin, ObjectHandler::dbl_itr& yEnd,
            const QuantLib::Matrix& zData) {
        return boost::shared_ptr<QuantLib::Interpolation2D>(
            new QuantLib::BicubicSpline(
                xBegin, xEnd, yBegin, yEnd, zData));
    }

    /* *** Pricers *** */
    /* *** IborCouponPricer *** */
    boost::shared_ptr<QuantLib::IborCouponPricer> IBOR_BY_BLACK_Pricer(
        const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>& capletVol){
        return boost::shared_ptr<QuantLib::IborCouponPricer>(
            new QuantLib::BlackIborCouponPricer(capletVol));
    };
    /* *** CmsCouponPricer **** */
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_BLACK_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
        const QuantLib::Handle<QuantLib::Quote>& meanReversion){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::AnalyticHaganPricer(swaptionVol, modelOfYieldCurve, meanReversion));
    };
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_NUMERICAL_INTEGRATION_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
        const QuantLib::Handle<QuantLib::Quote>& meanReversion){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::NumericHaganPricer(swaptionVol, modelOfYieldCurve, meanReversion));
    };

}
