
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

// this file generated automatically by autogen.py
// editing this file manually is not recommended

#ifndef qla_options_hpp
#define qla_options_hpp

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    const ObjHandler::Properties& QL_OPTION_ASIAN_C(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &average,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_ASIAN_D(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &average,
            const double &runningAccumulator,
            const long &pastFixings,
            const std::vector< long  >&fixingDates,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_BARRIER(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &typeBarrier,
            const double &barrier,
            const double &rebate,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_BASKET(
            const std::string &handleObject,
            const std::vector< std::string  >&handleStochastic,
            const std::string &basket,
            const std::vector < std::vector < double  > >&correlations,
            const std::string &optionType,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_CLIQUET(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::vector< long  >&resetDates,
            const std::string &optionType,
            const double &strike,
            const long &exerciseDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_DIVIDENDVANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::vector< long  >&dividendDates,
            const std::vector< double  >&dividends,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_FORWARDVANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const double &moneyness,
            const long &resetDate,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

    const ObjHandler::Properties& QL_OPTION_SETENGINE(
            const std::string &handle,
            const std::string &engineName,
            const long &timeSteps);

    const ObjHandler::Properties& QL_STOCHASTIC_PROCESS(
            const std::string &handleObject,
            const double &underlying,
            const std::string &dayCounter,
            const long &settlementDate,
            const double &riskFreeRate,
            const double &dividendYield,
            const double &volatility);

    const ObjHandler::Properties& QL_OPTION_VANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps);

}

#endif
