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

#include "utilities.hpp"
#include <string>
#include <sstream>
#include "QuantLibAddin/objectoption.hpp"
using std::ostringstream;
using std::string;
using namespace ObjHandler;
using namespace QuantLib;

LPXLOPER QL_BLACKSCHOLES(
		double *dividendYield,
		double *riskFreeRate,
		double *volatility,
		double *underlying,
		long int *todaysDateNum,
		long int *settlementDateNum) {
	try {
		std::string handleStochastic = getCaller();
		Date todaysDate(*todaysDateNum);
		Date settlementDate(*settlementDateNum);
		obj_ptr objectStochastic(
			new ObjectStochastic(*dividendYield, *riskFreeRate, *volatility, 
				*underlying, todaysDate, settlementDate));
		ObjectHandler::instance().storeObject(handleStochastic, objectStochastic);
		static XLOPER xRet;
		setValues(&xRet, objectStochastic, handleStochastic);
		return &xRet;
	} catch (const exception &e) {
		logMessage(std::string("ERROR: QL_BLACKSCHOLES: ") + e.what());
		return 0;
	}
}

LPXLOPER QL_OPTION(
		char *handleStochastic_char,
		char *type,
		double *strike,
		long int *timeSteps,
		long int *exerciseDateNum,
		long int *settlementDateNum) {
	try {
		std::string handleStochastic(handleStochastic_char);
		boost::shared_ptr<ObjectStochastic> objectStochastic = 
			boost::dynamic_pointer_cast<ObjectStochastic>
			(ObjectHandler::instance().retrieveObject(handleStochastic));
		if (!objectStochastic)
			QL_FAIL("error retrieving object " + handleStochastic);
		Date exerciseDate(*exerciseDateNum);
		Date settlementDate(*settlementDateNum);
		obj_ptr objectOption(
			new ObjectOption(objectStochastic, type, *strike, *timeSteps,
			exerciseDate, settlementDate));
		std::string handleOption = getCaller();
		ObjectHandler::instance().storeObject(handleOption, objectOption);
		static XLOPER xRet;
		setValues(&xRet, objectOption, handleOption);
		return &xRet;
	} catch(const exception &e) {
		logMessage(std::string("ERROR: QL_OPTION: ") + e.what());
		return 0;
	}
}

LPXLOPER QL_OPTION_SETENGINE(
		char *handleOption_char,
		char *engineName_char,
		long int *timeSteps) {
	try {
		std::string handleOption(handleOption_char);
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption>
			(ObjectHandler::instance().retrieveObject(handleOption));
		if (!objectOption)
			QL_FAIL("error retrieving object " + handleOption);
		std::string engineName(engineName_char);
		objectOption->setEngine(engineName, *timeSteps);
		static XLOPER xRet;
		setValues(&xRet, objectOption, handleOption);
		return &xRet;
	} catch(const exception &e) {
		logMessage(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		return 0;
	}
}
