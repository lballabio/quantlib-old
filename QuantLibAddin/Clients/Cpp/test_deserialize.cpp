
/*
 Copyright (C) 2007 Eric Ehlers

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

using namespace QuantLibAddinCpp;

int main() {

    try {

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L);
        ohSetConsole(1, 4L);
        ohLogMessage("Begin example program.", 4L);
        ohLogMessage(qlAddinVersion(), 4L);
        ohLogMessage(ohVersion(), 4L);

        std::string idBlackConstantVol = "my_blackconstantvol";
        std::string idGeneralizedBlackScholesProcess = "my_blackscholes";
        std::string idStrikedTypePayoff = "my_payoff";
        std::string idExercise = "my_exercise";
        std::string idPricingEngine = "my_engine";
        std::string idVanillaOption = "my_option";
        std::string xmlFileName = "option_demo.xml";
        long evaluationDate = 35930;        // 15 May 1998

        qlSettingsSetEvaluationDate(evaluationDate);

        std::vector<std::string> idList;
        idList.push_back(idBlackConstantVol);
        idList.push_back(idGeneralizedBlackScholesProcess);
        idList.push_back(idStrikedTypePayoff);
        idList.push_back(idExercise);
        idList.push_back(idPricingEngine);
        idList.push_back(idVanillaOption);
        ohObjectLoad(idList, xmlFileName);

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

