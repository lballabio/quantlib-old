/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004 Ferdinando Ametrano
 Copyright (C) 2004, 2007 StatPro Italia srl
 Copyright (C) 2008 Paul Farrington

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

#include "../customutilities.hpp"
#include <ql/quantlib.hpp>
#include <iostream>

using namespace QuantLib;

int main()
{

    LARGE_TITLE("Test: Quanto Option");

    BOOST_MESSAGE("Testing analytic Bates engine against Black formula...");

    SavedSettings backup;

    Date settlementDate = Date::todaysDate();
    Settings::instance().evaluationDate() = settlementDate;

    DayCounter dayCounter = ActualActual();
    Date exerciseDate = settlementDate + 6*Months;

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                     new PlainVanillaPayoff(Option::Put, 30));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.1, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.04, dayCounter));
    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(32.0)));

    Real yearFraction = dayCounter.yearFraction(settlementDate, exerciseDate);
    Real forwardPrice = s0->value()*std::exp((0.1-0.04)*yearFraction);
    Real expected = blackFormula(payoff->optionType(), payoff->strike(),
        forwardPrice, std::sqrt(0.05*yearFraction)) *
                                            std::exp(-0.1*yearFraction);
    const Real v0 = 0.05;
    const Real kappa = 5.0;
    const Real theta = 0.05;
    const Real sigma = 1.0e-4;
    const Real rho = 0.0;
    const Real lambda = 0.0001;
    const Real nu = 0.0;
    const Real delta = 0.0001;

    VanillaOption option(payoff, exercise);

    boost::shared_ptr<BatesProcess> process(
        new BatesProcess(riskFreeTS, dividendTS, s0, v0,
                         kappa, theta, sigma, rho, lambda, nu, delta));

    boost::shared_ptr<PricingEngine> engine(new BatesEngine(
        boost::shared_ptr<BatesModel>(new BatesModel(process)), 64));

    option.setPricingEngine(engine);
    Real calculated = option.NPV();

    std::cout << "Option Price: " << calculated << std::endl;


    return 0;
}

