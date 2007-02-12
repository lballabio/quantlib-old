
/*
 Copyright (C) 2007 Katiuscia Manzoni
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

#ifndef qla_enumclassctors_hpp
#define qla_enumclassctors_hpp

#include <qlo/typefactory.hpp>
#include <ql/option.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Instruments/quantoforwardvanillaoption.hpp>
#include <ql/PricingEngines/all.hpp>
#include <ql/Math/backwardflatinterpolation.hpp>
#include <ql/Math/forwardflatinterpolation.hpp>
#include <ql/Math/linearinterpolation.hpp>
#include <ql/Math/bilinearinterpolation.hpp>
#include <ql/Math/bicubicsplineinterpolation.hpp>
#include <ql/Math/sabrinterpolation.hpp>
#include <ql/CashFlows/cmscoupon.hpp>
#include <ql/CashFlows/couponpricer.hpp>

namespace QuantLibAddin {

    /* *** StrikedTypePayoff - Option::Type + 1 parameter *** */
    boost::shared_ptr<QuantLib::Payoff> VANILLA_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike);
    boost::shared_ptr<QuantLib::Payoff> ASSETORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike);
    boost::shared_ptr<QuantLib::Payoff> PERCENTAGESTRIKE_Payoff(
            const QuantLib::Option::Type& optionType,
            const double moneyness);

    /* *** StrikedTypePayoff - Option::Type + 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> CASHORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double cashPayoff);
    boost::shared_ptr<QuantLib::Payoff> GAP_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double secondStrike);

    /* *** StrikedTypePayoff - 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERFUND_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike);
   /* *** StrikedTypePayoff - 3 parameters *** */
   boost::shared_ptr<QuantLib::Payoff> SUPERSHARE_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike,
            const double cashPayoff);

    /* *** PricingEngine - timesteps ignored *** */
    boost::shared_ptr<QuantLib::PricingEngine> AB_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> AC_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> ACGAPA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> ADA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> ADGAPA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> ADE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> AE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> AP_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> BAWA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> I_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> BSA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> PE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> SE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FPE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> QE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> QFE_Engine(const long& timeSteps);

    /* *** PricingEngine - timesteps required *** */
    boost::shared_ptr<QuantLib::PricingEngine> AEQPB_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> CRR_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDA_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDE_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDB_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> JR_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> LR_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> TIAN_Engine(const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> TRI_Engine(const long& timeSteps);

    /* *** Linear 1D Interpolation *** */
    boost::shared_ptr<QuantLib::Interpolation> LINEAR_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> BACKWARDFLAT_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> FORWARDFLAT_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> LOGLINEAR_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin);

    /* *** Interpolation2D *** */
    boost::shared_ptr<QuantLib::Interpolation2D> BILINEAR_Interpolation(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
            const QuantLib::Matrix& zData);
    boost::shared_ptr<QuantLib::Interpolation2D> BICUBICSPLINE(
            dbl_itr& xBegin, dbl_itr& xEnd, dbl_itr& yBegin, dbl_itr& yEnd,
            const QuantLib::Matrix& zData);

    /* *** YieldTermStructure *** */
    //Discount based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    //ZeroYield based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    //ForwardRate based yield term structures
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);

    /* *** Pricers *** */
    /* *** IborCouponPricer *** */
    boost::shared_ptr<QuantLib::IborCouponPricer> 
        IBOR_BY_BLACK_Pricer(
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& capletVol);
    /* *** CmsCouponPricer **** */
    boost::shared_ptr<QuantLib::CmsCouponPricer> 
        CONUNDRUM_BY_BLACK_Pricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);
    boost::shared_ptr<QuantLib::CmsCouponPricer>
        CONUNDRUM_BY_NUMERICAL_INTEGRATION_Pricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);

    /* *** Index *** */
    /* *** Euribor *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR_SW();
    boost::shared_ptr<QuantLib::Index> EURIBOR_2W();
    boost::shared_ptr<QuantLib::Index> EURIBOR_3W();
    boost::shared_ptr<QuantLib::Index> EURIBOR_1M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_2M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_3M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_4M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_5M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_6M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_7M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_8M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_9M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_10M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_11M();
    boost::shared_ptr<QuantLib::Index> EURIBOR_1Y();
     /* *** Euribor365 *** */
    boost::shared_ptr<QuantLib::Index> EURIBOR365_SW();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2W();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3W();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_2M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_3M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_4M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_5M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_6M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_7M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_8M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_9M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_10M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_11M();
    boost::shared_ptr<QuantLib::Index> EURIBOR365_1Y();
    /* *** Eurlibor *** */
    boost::shared_ptr<QuantLib::Index> EURLIBOR_SW();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2W();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_2M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_3M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_4M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_5M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_6M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_7M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_8M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_9M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_10M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_11M();
    boost::shared_ptr<QuantLib::Index> EURLIBOR_1Y();
    /* *** EuriborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_1Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_2Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_3Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_4Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_5Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_6Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_7Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_8Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_9Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_10Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_12Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_15Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_20Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_25Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXA_30Y();
    /* *** EuriborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_1Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_2Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_3Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_4Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_5Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_6Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_7Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_8Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_9Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_10Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_12Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_15Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_20Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_25Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXB_30Y();
    /* *** EurliborSwapFixA *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_1Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_2Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_3Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_4Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_5Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_6Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_7Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_8Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_9Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_10Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_12Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_15Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_20Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_25Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXA_30Y();
       /* *** EurliborSwapFixB *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_1Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_2Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_3Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_4Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_5Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_6Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_7Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_8Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_9Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_10Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_12Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_15Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_20Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_25Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXB_30Y();
       /* *** EurliborSwapFixIFR *** */
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_10Y();
    boost::shared_ptr<QuantLib::Index> EURLIBORSWAPFIXIFR_30Y();
    /* *** EuriborSwapFixIFR *** */
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_2Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_5Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_10Y();
    boost::shared_ptr<QuantLib::Index> EURIBORSWAPFIXIFR_30Y();
}

#endif
