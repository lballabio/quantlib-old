
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
#include <qlo/Enumerations/Factories/indexfactory.hpp>
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
#include <ql/math/interpolations/loglinearinterpolation.hpp>

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

    /* *** PricingEngines *** */
    /* *** Timesteps ignored *** */
    boost::shared_ptr<QuantLib::PricingEngine> AB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticBarrierEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AC_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticCliquetEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ACGAPA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticContinuousGeometricAveragePriceAsianEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDigitalAmericanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADGAPA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDiscreteGeometricAveragePriceAsianEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDividendEuropeanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticEuropeanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AP_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticPerformanceEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> BAWA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BaroneAdesiWhaleyApproximationEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> I_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::IntegralEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> BSA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BjerksundStenslandApproximationEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> PE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine>();
    }
    boost::shared_ptr<QuantLib::PricingEngine> SE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::StulzEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> FE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine>
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FPE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine>
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::ForwardPerformanceEngine
                <QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> QE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine>
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::QuantoEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> QFE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine>
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);
        boost::shared_ptr<QuantLib::ForwardVanillaOption::engine> forwardEngine(
                new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::QuantoEngine<QuantLib::ForwardVanillaOption::arguments,
                QuantLib::ForwardVanillaOption::results>(forwardEngine));
    }
    /* *** Timesteps required *** */
    boost::shared_ptr<QuantLib::PricingEngine> AEQPB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::AdditiveEQPBinomialTree>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> CRR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::CoxRossRubinstein>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDAmericanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDEuropeanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDBermudanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> JR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::JarrowRudd>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> LR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::LeisenReimer>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> JOSHI_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Joshi4>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TIAN_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Tian>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TRI_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Trigeorgis>(timeSteps));
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
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& capletVol){
        return boost::shared_ptr<QuantLib::IborCouponPricer>(
            new QuantLib::BlackIborCouponPricer(capletVol));
    };
    /* *** CmsCouponPricer **** */
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_BLACK_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
        const QuantLib::Handle<QuantLib::Quote>& meanReversion){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::ConundrumPricerByBlack(swaptionVol, modelOfYieldCurve, meanReversion));
    };
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_NUMERICAL_INTEGRATION_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
        const QuantLib::Handle<QuantLib::Quote>& meanReversion){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::ConundrumPricerByNumericalIntegration(swaptionVol, modelOfYieldCurve, meanReversion));
    };

    /* *** Indexes *** */
    /* *** Euribor *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSW(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor2W(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_3W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor3W(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor1M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor2M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor3M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor4M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor5M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor6M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor7M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor8M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor9M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor10M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor11M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** Euribor365 *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR365_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_SW(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_2W(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_3W(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_1M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_2M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_3M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_4M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_5M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_6M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_7M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_8M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_9M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_10M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_11M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurLibor *** */
    boost::shared_ptr<QuantLib::Index> EURLIBOR_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLiborSW(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor2W(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor1M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor2M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor3M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor4M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor5M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor6M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor7M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor8M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor9M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor10M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor11M(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EuriborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA2Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA3Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA4Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA5Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA6Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA7Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA8Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA9Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA12Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA15Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA20Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA25Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EuriborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB2Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB3Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB4Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB5Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB6Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB7Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB8Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB9Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB12Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB15Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB20Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB25Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurliborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA2Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA3Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA4Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA5Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA6Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA7Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA8Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA9Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA12Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA15Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA20Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA25Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurliborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB1Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB2Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB3Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB4Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB5Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB6Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB7Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB8Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB9Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB12Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB15Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB20Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB25Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurliborSwapFixIFR *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixIFR10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixIFR30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EuriborSwapFixIFR *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR2Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR5Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR10Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR30Y(
                QuantLibAddin::EuriborHandle::instance().handleYieldTermStructure()));
    }

}
