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

#include <Addins/Calc/qladdin.hpp>
#include <QuantLibAddin/objectoption.hpp>
#include <Addins/Calc/utilities.hpp>

using namespace QuantLib;
using namespace ObjHandler;

SEQSEQ( ANY ) SAL_CALL QLAddin::qlBlackScholes( 
			const STRING& handle,
			double dividendYield,
			double riskFreeRate,
			double volatility,
			double underlying,
			sal_Int32 todaysDateNum,
			sal_Int32 settlementDateNum) THROWDEF_RTE_IAE {
	try {
		std::string handle2 = OUStringToString(handle);
		Date todaysDate(todaysDateNum);
		Date settlementDate(settlementDateNum);
		obj_ptr objectStochastic(
			new ObjectStochastic(dividendYield, riskFreeRate, volatility, 
				underlying, todaysDate, settlementDate));
		ObjectHandler::instance().storeObject(handle2, objectStochastic);
		return getArray(objectStochastic, handle);
	} catch (const std::exception &e) {
		logMessage(std::string("ERROR: QL_BLACKSCHOLES: ") + e.what());
		THROW_RTE;
	}
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOption( 
			const STRING& handle,
			const STRING& handleStochastic,
			const STRING& typeOption,
			double strike,
			sal_Int32 timeSteps,
			sal_Int32 exerciseDateNum,
			sal_Int32 settlementDateNum) THROWDEF_RTE_IAE {
	try {
		std::string handle2 = OUStringToString(handle);
		std::string handleStochastic2 = OUStringToString(handleStochastic);
		std::string type2 = OUStringToString(typeOption);
		boost::shared_ptr<ObjectStochastic> objectStochastic = 
			boost::dynamic_pointer_cast<ObjectStochastic>
			(ObjectHandler::instance().retrieveObject(handleStochastic2));
		if (!objectStochastic)
			QL_FAIL("error retrieving object " + handleStochastic2);
		Date exerciseDate(exerciseDateNum);
		Date settlementDate(settlementDateNum);
		obj_ptr objectOption(
			new ObjectOption(objectStochastic, type2, strike, timeSteps,
			exerciseDate, settlementDate));
		ObjectHandler::instance().storeObject(handle2, objectOption);
		return getArray(objectOption, handle);
	} catch (const std::exception &e) {
		logMessage(std::string("ERROR: QL_OPTION: ") + e.what());
		THROW_RTE;
	}
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionSetEngine( 
			const STRING& handle,
			sal_Int32 engineID,
			sal_Int32 timeSteps) THROWDEF_RTE_IAE {
	try {
		std::string handle2 = OUStringToString(handle);
		std::string engineName;
		if (engineID == 1)
			engineName = BINOMIAL_JARROW_RUDD;
		else if (engineID == 2)
			engineName = BINOMIAL_COX_ROSS_RUBINSTEIN;
		else if (engineID == 3)
			engineName = ADDITIVE_EQUIPROBABILITIES;
		else
			QL_FAIL("invalid engine ID");
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption>
			(ObjectHandler::instance().retrieveObject(handle2));
		if (!objectOption)
			QL_FAIL("error retrieving object " + handle2);
		objectOption->setEngine(engineName, timeSteps);
		return getArray(objectOption, handle);
	} catch (const std::exception &e) {
		logMessage(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		THROW_RTE;
	}
}

