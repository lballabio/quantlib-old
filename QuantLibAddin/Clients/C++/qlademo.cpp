
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <Addins/C++/addincpp.hpp>

using namespace QuantLibAddinCpp;

int main() {

    try {

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L);
        ohSetConsole(1, 4L);
        ohLogMessage("Begin example program.", 4L);
        ohLogMessage(qlAddinVersion(), 4L);
        ohLogMessage(ohVersion(), 4L);

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long evaluationDate = 35930;        // 15 May 1998
        long settlementDate = 35932;        // 17 May 1998
        long exerciseDate = 36297;          // 17 May 1999

        qlSettingsSetEvaluationDate(evaluationDate);

        std::string idBlackConstantVol = qlBlackConstantVol(
            "my_blackconstantvol",
            settlementDate,
            volatility,
            "Actual/365 (Fixed)");

        std::string idGeneralizedBlackScholesProcess = qlGeneralizedBlackScholesProcess(
            "my_blackscholes",
            idBlackConstantVol,             // black constant vol object ID
            underlying,                     // underlying
            "Actual/365 (Fixed)",           // daycount convention
            settlementDate,                 // settlement date as long
            riskFreeRate,                   // risk free rate
            dividendYield);                 // dividend yield

        std::string idExercise = qlEuropeanExercise("my_exercise", exerciseDate);

        std::string idStrikedTypePayoff = qlStrikedTypePayoff(
            "my_payoff",
            "Vanilla",
            "Put",
            strike,                         // "thirdParameter" ?
            strike);

        std::string idPricingEngine = qlPricingEngine(
            "my_engine",
            "AE");                          // engine ID (Analytic European)

        std::string idVanillaOption = qlVanillaOption(
            "my_option",                        // object ID
            idGeneralizedBlackScholesProcess,   // stochastic process object ID
            idStrikedTypePayoff,                // option type
            idExercise,                         // payoff type
            idPricingEngine);                   // time steps

        std::ostringstream s;
        s << "option NPV() = " << qlInstrumentNPV(idVanillaOption);
        ohLogMessage(s.str(), 4L);

        ohLogObject(idVanillaOption);

        ohLogMessage("End example program.", 4L);

        return 0;
    } catch (const std::exception &e) {
        std::ostringstream s;
        s << "Error: " << e.what();
        ohLogMessage(s.str(), 1L);
        return 1;
    } catch (...) {
        ohLogMessage("unknown error", 1L);
        return 1;
    }

}

