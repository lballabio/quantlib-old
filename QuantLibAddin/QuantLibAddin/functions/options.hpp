
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

// this file generated automatically by autogen.py on Tue Jan 25 14:57:59 2005
// editing this file manually is not recommended

#ifndef qla_options_hpp
#define qla_options_hpp

#include <ObjectHandler/objhandler.hpp>

namespace QuantLibAddin {

    const ObjHandler::Properties& QL_STOCHASTIC_PROCESS(
            const std::string &handleObject,
            const double &underlying,
            const std::string &dayCounterID,
            const long &settlementDate,
            const double &riskFreeRate,
            const double &dividendYield,
            const double &volatility);

    const ObjHandler::Properties& QL_OPTION_VANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_ASIAN_C(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &typeAverage,
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_BARRIER(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &typeBarrier,
            const double &barrier,
            const double &rebate,
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_SETENGINE(
            const std::string &handle,
            const std::string &engineName,
            const long &timeSteps);

}

#endif
