
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

#include <QuantLibAddin/functions/options.hpp>
#include <QuantLibAddin/objects/objectoption.hpp>

using namespace QuantLib;
using namespace ObjHandler;

namespace QuantLibAddin {

	const Properties& QL_BLACKSCHOLES(
			const std::string &handleStochastic, 
			const double &dividendYield,
			const double &riskFreeRate,
			const double &volatility,
			const double &underlying,
			const long &todaysDate,
			const long &settlementDate) {
		obj_ptr objectStochastic(
			new ObjectStochastic(Spread(dividendYield), Rate(riskFreeRate), 
				Volatility(volatility), Real(underlying),
				Date(todaysDate), Date(settlementDate)));
		ObjectHandler::instance().storeObject(handleStochastic, objectStochastic);
		return objectStochastic->getProperties();
	}

	const Properties& QL_OPTION(
		const std::string &handleOption, 
		const std::string &handleStochastic,
		const std::string &type,
		const double &strike,
		const long &timeSteps,
		const long &exerciseDate,
		const long &settlementDate) {
		boost::shared_ptr<ObjectStochastic> objectStochastic = 
			boost::dynamic_pointer_cast<ObjectStochastic>
			(ObjectHandler::instance().retrieveObject(handleStochastic));
		if (!objectStochastic)
			QL_FAIL("QL_OPTION: error retrieving object " + handleStochastic);
		obj_ptr objectOption(
			new ObjectOption(objectStochastic, type, Real(strike), Size(timeSteps),
			Date(exerciseDate), Date(settlementDate)));
		ObjectHandler::instance().storeObject(handleOption, objectOption);
		return objectOption->getProperties();
	}

	const Properties& QL_OPTION_SETENGINE(
		const std::string &handleOption, 
		const std::string &engineName,
		const long &timeSteps) {
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption>
			(ObjectHandler::instance().retrieveObject(handleOption));
		if (!objectOption)
			QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handleOption);
		objectOption->setEngine(engineName, timeSteps);
		return objectOption->getProperties();
	}

}

