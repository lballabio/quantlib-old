
/*
 Copyright (C) 2006 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/quantoforwardvanillaoption.hpp>
#include <qla/typefactory.hpp>
#include <qla/exercise.hpp>
#include <qla/termstructures.hpp>
#include <qla/volatilities.hpp>

#include <ql/DayCounters/all.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/PricingEngines/all.hpp>

namespace QuantLibAddin {

    QuantoForwardVanillaOption::QuantoForwardVanillaOption(
            const std::string &handleTermStructure,
            const std::string &handleBlackVol,
            const double &correlation,
            const double &moneyness,
            const long &resetDate,
            const std::string &handleBlackScholes,
            const std::string &optionTypeID,
            const std::string &payoffID,
            const double &strike,
            const std::string &handleExercise,
            const std::string &engineID,
            const long &timeSteps) {

        OH_GET_REFERENCE(termStructure, handleTermStructure, 
            YieldTermStructure, QuantLib::YieldTermStructure)
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructure);

        OH_GET_REFERENCE(blackVolTermStructure, handleBlackVol, 
            BlackVolTermStructure, QuantLib::BlackVolTermStructure)
        QuantLib::Handle<QuantLib::BlackVolTermStructure>
            blackVolTermStructureH(blackVolTermStructure);

        QuantLib::Handle<QuantLib::Quote> correlationH(
            boost::shared_ptr<QuantLib::Quote>(
            new QuantLib::SimpleQuote(correlation)));

        OH_GET_REFERENCE(blackScholesProcess, handleBlackScholes, 
            GeneralizedBlackScholesProcess, QuantLib::GeneralizedBlackScholesProcess)

        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            Create<boost::shared_ptr<QuantLib::StrikedTypePayoff> >()(optionTypeID, payoffID, strike);

        OH_GET_REFERENCE(exercise, handleExercise, Exercise,
            QuantLib::Exercise)

        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, timeSteps);

        mInstrument = boost::shared_ptr<QuantLib::QuantoForwardVanillaOption>(
            new QuantLib::QuantoForwardVanillaOption(
                termStructureH,
                blackVolTermStructureH,
                correlationH,
                moneyness,
                QuantLib::Date(resetDate),
                blackScholesProcess,
                payoff,
                exercise,
                pricingEngine));
    }

}

