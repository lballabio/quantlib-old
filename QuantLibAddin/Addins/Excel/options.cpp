
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

// this file generated automatically by autogen.py on Sun Dec 12 14:39:53 2004
// editing this file manually is not recommended

#include <QuantLibAddin/qladdin.hpp>
#include <Addins/Excel/utilities.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

LPXLOPER qlBlackScholes(
		double *dividendYield,
		double *riskFreeRate,
		double *volatility,
		double *underlying,
		long *todaysDate,
		long *settlementDate) {
	try {
		std::string handle = getCaller();
		Properties properties = QL_BLACKSCHOLES(
		std::string(handle),
		*dividendYield,
		*riskFreeRate,
		*volatility,
		*underlying,
		*todaysDate,
		*settlementDate);
		static XLOPER xRet;
		setValues(&xRet, properties, handle);
		return &xRet;
	} catch (const exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_BLACKSCHOLES: ") + e.what());
		return 0;
	}
}

LPXLOPER qlOption(
		char *handleStochastic,
		char *typeOption,
		double *strike,
		long *timeSteps,
		long *exerciseDate,
		long *settlementDate) {
	try {
		std::string handle = getCaller();
		Properties properties = QL_OPTION(
		std::string(handle),
		std::string(handleStochastic),
		std::string(typeOption),
		*strike,
		*timeSteps,
		*exerciseDate,
		*settlementDate);
		static XLOPER xRet;
		setValues(&xRet, properties, handle);
		return &xRet;
	} catch (const exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_OPTION: ") + e.what());
		return 0;
	}
}

LPXLOPER qlOptionSetEngine(
		char *handle,
		char *engineName,
		long *timeSteps) {
	try {
		Properties properties = QL_OPTION_SETENGINE(
		std::string(handle),
		std::string(engineName),
		*timeSteps);
		static XLOPER xRet;
		setValues(&xRet, properties, handle);
		return &xRet;
	} catch (const exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		return 0;
	}
}

