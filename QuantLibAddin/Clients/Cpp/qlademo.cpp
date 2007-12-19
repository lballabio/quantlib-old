
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

#include <Addins/Cpp/addincpp.hpp>
#include <oh/ohdefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#endif

#define OH_NULL ObjectHandler::Variant()

using namespace QuantLibAddinCpp;

int main() {

    try {

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L, OH_NULL);
        ohSetConsole(1, 4L, OH_NULL);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion(OH_NULL));
        LOG_MESSAGE("ObjectHandler version = " << ohVersion(OH_NULL));

        std::string daycountConvention = "Actual/365 (Fixed)";
        std::string payoffType = "Vanilla";
        std::string optionType = "Put";
        std::string engineType = "AE";      // Analytic European
        std::string calendar = "TARGET";
        std::string xmlFileName = "option_demo.xml";
        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long evaluationDate = 35930;        // 15 May 1998
        long settlementDate = 35932;        // 17 May 1998
        long exerciseDate = 36297;          // 17 May 1999

        qlSettingsSetEvaluationDate(evaluationDate, OH_NULL);

        std::string idBlackConstantVol = qlBlackConstantVol(
            std::string("my_blackconstantvol"),
            settlementDate,
            calendar,
            volatility,
            daycountConvention,
            OH_NULL,
            OH_NULL,
            false);

        std::string idGeneralizedBlackScholesProcess = qlGeneralizedBlackScholesProcess(
            "my_blackscholes",
            idBlackConstantVol,
            underlying,
            daycountConvention,
            settlementDate,
            riskFreeRate,
            dividendYield,
            OH_NULL,
            OH_NULL,
            false);

        std::string idStrikedTypePayoff = qlStrikedTypePayoff(
            "my_payoff",
            payoffType,
            optionType,
            strike,
            strike,
            OH_NULL,
            OH_NULL,
            false);

        std::string idExercise = qlEuropeanExercise(
            "my_exercise",
            exerciseDate,
            OH_NULL,
            OH_NULL,
            false);

        std::string idPricingEngine = qlPricingEngine(
            "my_engine",
            engineType,
            idGeneralizedBlackScholesProcess,
            OH_NULL,
            OH_NULL,
            false);

        std::string idVanillaOption = qlVanillaOption(
            "my_option",
            idStrikedTypePayoff,
            idExercise,
            OH_NULL,
            OH_NULL,
            false);

        qlInstrumentSetPricingEngine(idVanillaOption, idPricingEngine, OH_NULL);

        LOG_MESSAGE("option PV = " << qlInstrumentNPV(idVanillaOption, OH_NULL));

        ohLogObject(idVanillaOption, OH_NULL);

        std::vector<std::string> idList;
        idList.push_back(idBlackConstantVol);
        idList.push_back(idGeneralizedBlackScholesProcess);
        idList.push_back(idStrikedTypePayoff);
        idList.push_back(idExercise);
        idList.push_back(idPricingEngine);
        idList.push_back(idVanillaOption);
        ohObjectSave(idList, xmlFileName, true, OH_NULL);

        LOG_MESSAGE("End example program.");

        return 0;
    } catch (const std::exception &e) {
        LOG_ERROR("Error: " << e.what());
        return 1;
    } catch (...) {
        LOG_ERROR("Unknown error");
        return 1;
    }

}

