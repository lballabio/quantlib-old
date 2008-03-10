/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2008 Ferdinando Ametrano
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

#ifndef qla_enumclassctors_hpp
#define qla_enumclassctors_hpp

#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/conundrumpricerfactory.hpp>
#include <qlo/enumerations/factories/interpolationsfactory.hpp>
#include <ql/option.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/instruments/quantoforwardvanillaoption.hpp>
#include <ql/pricingengines/all.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/cashflows/couponpricer.hpp>

namespace QuantLibAddin {

    /* *** StrikedTypePayoff *** */
    /* *** Option::Type + 1 parameter *** */
    boost::shared_ptr<QuantLib::Payoff> VANILLA_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike);
    boost::shared_ptr<QuantLib::Payoff> ASSETORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike);
    boost::shared_ptr<QuantLib::Payoff> PERCENTAGESTRIKE_Payoff(
            const QuantLib::Option::Type& optionType,
            const double moneyness);
    /* *** Option::Type + 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> CASHORNOTHING_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double cashPayoff);
    boost::shared_ptr<QuantLib::Payoff> GAP_Payoff(
            const QuantLib::Option::Type& optionType,
            const double strike,
            const double secondStrike);
    /* *** 2 parameters *** */
    boost::shared_ptr<QuantLib::Payoff> SUPERFUND_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike);
   /* *** 3 parameters *** */
   boost::shared_ptr<QuantLib::Payoff> SUPERSHARE_Payoff(
            const QuantLib::Option::Type&,
            const double strike,
            const double secondStrike,
            const double cashPayoff);

    /* *** PricingEngines - without timesteps *** */
    boost::shared_ptr<QuantLib::PricingEngine> AB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> AC_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> ACGAPA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> ADA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> ADE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> ADGAPA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> AE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> AP_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> BAWA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> BSA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> I_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> PE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    boost::shared_ptr<QuantLib::PricingEngine> SE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    // FIXME these seem not to work following pricing engines redesign?
    //boost::shared_ptr<QuantLib::PricingEngine> FE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    //boost::shared_ptr<QuantLib::PricingEngine> FPE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    //boost::shared_ptr<QuantLib::PricingEngine> QE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);
    //boost::shared_ptr<QuantLib::PricingEngine> QFE_Engine(
    //    const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process);

    /* *** PricingEngines - with timesteps *** */
    boost::shared_ptr<QuantLib::PricingEngine> AEQPB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> CRR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDA_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDB_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> FDE_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> JOSHI_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> JR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> LR_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> TIAN_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);
    boost::shared_ptr<QuantLib::PricingEngine> TRI_Engine(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process, const long& timeSteps);

    /* *** 1D Interpolation *** */
    boost::shared_ptr<QuantLib::Interpolation> BACKWARDFLAT_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> FORWARDFLAT_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> LINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> LOGLINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> NATURALCUBICSPLINE_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> MONOTONICNATURALCUBICSPLINE_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> NATURALLOGCUBIC_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> MONOTONICNATURALLOGCUBIC_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> CONSTRAINEDCUBICSPLINE_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> CONSTRAINEDLOGCUBIC_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);
    boost::shared_ptr<QuantLib::Interpolation> ABCD_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin);

    /* *** Interpolation2D *** */
    boost::shared_ptr<QuantLib::Interpolation2D> BILINEAR_Interpolation(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin, ObjectHandler::dbl_itr& yEnd,
            const QuantLib::Matrix& zData);
    boost::shared_ptr<QuantLib::Interpolation2D> BICUBICSPLINE(
            ObjectHandler::dbl_itr& xBegin, ObjectHandler::dbl_itr& xEnd, ObjectHandler::dbl_itr& yBegin, ObjectHandler::dbl_itr& yEnd,
            const QuantLib::Matrix& zData);

    /* *** Pricers *** */
    /* *** IborCouponPricer *** */
    boost::shared_ptr<QuantLib::IborCouponPricer>
        IBOR_BY_BLACK_Pricer(
            const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>& capletVol);
    /* *** CmsCouponPricer **** */
    boost::shared_ptr<QuantLib::CmsCouponPricer>
        CONUNDRUM_BY_BLACK_Pricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);
    boost::shared_ptr<QuantLib::CmsCouponPricer>
        CONUNDRUM_BY_NUMERICAL_INTEGRATION_Pricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);

}

#endif
