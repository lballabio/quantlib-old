
/*
 Copyright (C) 2004 Eric Ehlers

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

#include <QuantLibAddin/qladdin.hpp>
extern "C" {
#include <QuantLibAddin/C/varies.h>
#include <QuantLibAddin/C/options.h>
#include <QuantLibAddin/C/defines.h>
}
#include <QuantLibAddin/C/varies.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

int QL_BLACKSCHOLES_C(
		const char *handleStochastic, 
		const double dividendYield,
		const double riskFreeRate,
		const double volatility,
		const double underlying,
		const long todaysDate,
		const long settlementDate,
		VariesList *result) {
	try {
		Properties properties = QL_BLACKSCHOLES(handleStochastic, dividendYield,
			riskFreeRate, volatility, underlying, todaysDate, settlementDate);
		propertiesToVaries(properties, result);
		return SUCCESS;
	} catch (const std::exception &e) {
		QL_LOGMESSAGE("QL_BLACKSCHOLES_C Error: " + std::string(e.what()));
		result = 0;
		return FAIL;
	}
}

int QL_OPTION_C(
		const char *handleOption, 
		const char *handleStochastic,
		const char *type,
		const double strike,
		const long timeSteps,
		const long exerciseDate,
		const long settlementDate,
		VariesList *result) {
	try {
		Properties properties = QL_OPTION(handleOption, handleStochastic, type, 
			strike, timeSteps, exerciseDate, settlementDate);
		propertiesToVaries(properties, result);
		return SUCCESS;
	} catch (const std::exception &e) {
		QL_LOGMESSAGE("QL_OPTION_C Error: " + std::string(e.what()));
		result = 0;
		return FAIL;
	}
}

int QL_OPTION_SETENGINE_C(
		const char *handleOption, 
		const char *engineName,
		const long timeSteps,
		VariesList *result) {
	try {
		Properties properties = QL_OPTION_SETENGINE(handleOption,
			engineName, timeSteps);
		propertiesToVaries(properties, result);
		return SUCCESS;
	} catch (const std::exception &e) {
		QL_LOGMESSAGE("QL_OPTION_SETENGINE_C Error: " + std::string(e.what()));
		result = 0;
		return FAIL;
	}
}
