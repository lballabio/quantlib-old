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

#include "qladdincpp.hpp"

using namespace ObjHandler;
using namespace QuantLib;

Properties QL_BLACKSCHOLES(
		const std::string &handleStochastic, 
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate) {
	obj_ptr objectStochastic(
		new ObjectStochastic(dividendYield, riskFreeRate, volatility, underlying,
			todaysDate, settlementDate));
	ObjectHandler::instance().storeObject(handleStochastic, objectStochastic);
	return objectStochastic->getProperties();
}

Properties QL_OPTION(
	const std::string &handleOption, 
	const std::string &handleStochastic,
	const std::string &type,
	const Real &strike,
	const Size &timeSteps,
	const Date &exerciseDate,
	const Date &settlementDate) {
	boost::shared_ptr<ObjectStochastic> objectStochastic = 
		boost::dynamic_pointer_cast<ObjectStochastic>
		(ObjectHandler::instance().retrieveObject(handleStochastic));
	if (!objectStochastic)
		QL_FAIL("QL_OPTION: error retrieving object " + handleStochastic);
	obj_ptr objectOption(
		new ObjectOption(objectStochastic, type, strike, timeSteps,
		exerciseDate, settlementDate));
    ObjectHandler::instance().storeObject(handleOption, objectOption);
	return objectOption->getProperties();
}

Properties QL_OPTION_SETENGINE(
	const std::string &handleOption, 
	const std::string &engineName,
	const Size &timeSteps) {
	boost::shared_ptr<ObjectOption> objectOption = 
		boost::dynamic_pointer_cast<ObjectOption>
		(ObjectHandler::instance().retrieveObject(handleOption));
	if (!objectOption)
		QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handleOption);
	objectOption->setEngine(engineName, timeSteps);
	return objectOption->getProperties();
}
