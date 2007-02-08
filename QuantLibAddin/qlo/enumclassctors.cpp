
/*
 Copyright (C) 2006 Marco Bianchetti
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Chiara Fornarola
 Copyright (C) 2005 Plamen Neykov

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

#include <qlo/enumclassctors.hpp>
#include <qlo/Conversions/conversions.hpp>
#include <qlo/termstructures.hpp>

namespace QuantLibAddin {

    /* *** StrikedTypePayoff - Option::Type + 1 parameter *** */
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
    /* *** StrikedTypePayoff - Option::Type + 2 parameters *** */
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
    /* *** StrikedTypePayoff - 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERFUND_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::SuperFundPayoff(strike, secondStrike));
    }
    /* *** StrikedTypePayoff - 3 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERSHARE_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike,
            const double cashPayoff) {
        return boost::shared_ptr<QuantLib::Payoff> (
            new QuantLib::SuperSharePayoff(strike, secondStrike, cashPayoff));
    }

    /* *** PricingEngine - timesteps ignored *** */
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

    /* *** PricingEngine - timesteps required *** */
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
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::LinearInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> BACKWARDFLAT_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::BackwardFlatInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> FORWARDFLAT_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::ForwardFlatInterpolation(
                xBegin, xEnd, yBegin));
    }
    boost::shared_ptr<QuantLib::Interpolation> LOGLINEAR_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin) {
        return boost::shared_ptr<QuantLib::Interpolation>(
            new QuantLib::LogLinearInterpolation(
                xBegin, xEnd, yBegin));
    }

    /* *** Interpolation2D *** */
    boost::shared_ptr<QuantLib::Interpolation2D> BILINEAR_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
            const QuantLib::Matrix& zData) {
        return boost::shared_ptr<QuantLib::Interpolation2D>(
            new QuantLib::BilinearInterpolation(
                xBegin, xEnd, yBegin, yEnd, zData));
    }
    boost::shared_ptr<QuantLib::Interpolation2D> BICUBICSPLINE(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
            const QuantLib::Matrix& zData) {
        return boost::shared_ptr<QuantLib::Interpolation2D>(
            new QuantLib::BicubicSpline(
                xBegin, xEnd, yBegin, yEnd, zData));
    }

    /* *** YieldTermStructure *** */
    //Discount based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                              QuantLib::BackwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }    
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                              QuantLib::ForwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                              QuantLib::Linear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                              QuantLib::LogLinear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                              QuantLib::Cubic>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    //ZeroYield based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                              QuantLib::BackwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                              QuantLib::ForwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                              QuantLib::Linear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                              QuantLib::LogLinear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                              QuantLib::Cubic>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    //ForwardRate based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                              QuantLib::BackwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                              QuantLib::ForwardFlat>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                              QuantLib::Linear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                              QuantLib::LogLinear>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                              QuantLib::Cubic>(
                nDays, calendar,
                rateHelpers,
                dayCounter,
                1.0e-6));
    }

    //VanillaCmsCouponPricer
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_BLACK_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
        QuantLib::Real meanReversion){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::ConundrumPricerByBlack(swaptionVol, modelOfYieldCurve, meanReversion));
    };
    boost::shared_ptr<QuantLib::CmsCouponPricer> CONUNDRUM_BY_NUMERICAL_INTEGRATION_Pricer(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
        const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
        QuantLib::Real meanReversion ){
        return boost::shared_ptr<QuantLib::CmsCouponPricer>(
            new QuantLib::ConundrumPricerByNumericalIntegration(swaptionVol, modelOfYieldCurve, meanReversion));
    };

    /* *** Index *** */
    /* *** Euribor *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSW(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor2W(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_3W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor3W(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor1M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor2M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor3M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor4M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor5M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor6M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor7M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor8M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor9M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor10M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor11M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
        /* *** Euribor365 *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR365_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_SW(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_2W(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_3W(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_1M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_2M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_3M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_4M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_5M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_6M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_7M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_8M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_9M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_10M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_11M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::Euribor365_1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurLibor *** */
    boost::shared_ptr<QuantLib::Index> EURLIBOR_SW() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLiborSW(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2W() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor2W(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor1M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor2M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_3M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor3M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_4M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor4M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_5M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor5M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_6M() {
        return  boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor6M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_7M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor7M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_8M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor8M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_9M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor9M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_10M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor10M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_11M() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor11M(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EURLibor1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EuriborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA2Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA3Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA4Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA5Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA6Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA7Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA8Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA9Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA12Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA15Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA20Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA25Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixA30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EuriborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB2Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB3Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB4Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB5Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB6Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB7Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB8Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB9Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB12Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB15Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB20Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB25Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixB30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    /* *** EurliborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA2Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA3Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA4Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA5Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA6Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA7Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA8Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA9Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA12Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA15Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA20Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA25Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixA30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
        /* *** EurliborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_1Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB1Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB2Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_3Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB3Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_4Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB4Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB5Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_6Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB6Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_7Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB7Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_8Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB8Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_9Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB9Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_12Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB12Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_15Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB15Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_20Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB20Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_25Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB25Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixB30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
       /* *** EurliborSwapFixIFR *** */
      boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixIFR10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
      boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EurliborSwapFixIFR30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
      /* *** EuriborSwapFixIFR *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_2Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR2Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_5Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR5Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_10Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR10Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_30Y() {
        return boost::shared_ptr<QuantLib::Index>(
            new QuantLib::EuriborSwapFixIFR30Y(
                EuriborHandle::instance().handleYieldTermStructure()));
    }

}
