
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
#include <Addins/Calc/qladdin.hpp>
#include <Addins/Calc/utilities.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

SEQSEQ( ANY ) SAL_CALL QLAddin::qlBlackScholes(
		const STRING & handle,
		double dividendYield,
		double riskFreeRate,
		double volatility,
		double underlying,
		sal_Int32 todaysDate,
		sal_Int32 settlementDate) THROWDEF_RTE_IAE {
	try {
		Properties properties = QL_BLACKSCHOLES(
			OUStringToString(handle),
			dividendYield,
			riskFreeRate,
			volatility,
			underlying,
			todaysDate,
			settlementDate);
		return getArray(properties, handle);
	} catch (const std::exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_BLACKSCHOLES: ") + e.what());
		THROW_RTE;
	}
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOption(
		const STRING & handle,
		const STRING & handleStochastic,
		const STRING & typeOption,
		double strike,
		sal_Int32 timeSteps,
		sal_Int32 exerciseDate,
		sal_Int32 settlementDate) THROWDEF_RTE_IAE {
	try {
		Properties properties = QL_OPTION(
			OUStringToString(handle),
			OUStringToString(handleStochastic),
			OUStringToString(typeOption),
			strike,
			timeSteps,
			exerciseDate,
			settlementDate);
		return getArray(properties, handle);
	} catch (const std::exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_OPTION: ") + e.what());
		THROW_RTE;
	}
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionSetEngine(
		const STRING & handle,
		const STRING & engineName,
		sal_Int32 timeSteps) THROWDEF_RTE_IAE {
	try {
		Properties properties = QL_OPTION_SETENGINE(
			OUStringToString(handle),
			OUStringToString(engineName),
			timeSteps);
		return getArray(properties, handle);
	} catch (const std::exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		THROW_RTE;
	}
}

