#include "qladdin.hpp"
#include "ObjectClassLibrary/objectoption.hpp"
#include "utilities.hpp"

extern ObjectHandler objectHandler;

SEQSEQ( ANY ) SAL_CALL QLAddin::qlBlackScholes( 
			const STRING& handle,
			double dividendYield,
			double riskFreeRate,
			double volatility,
			double underlying,
			sal_Int32 todaysDateNum,
			sal_Int32 settlementDateNum) THROWDEF_RTE_IAE {
	try {
		string handle2 = OUStringToString(handle);
		Date todaysDate(todaysDateNum);
		Date settlementDate(settlementDateNum);
		obj_ptr objectStochastic(
			new ObjectStochastic(dividendYield, riskFreeRate, volatility, 
				underlying, todaysDate, settlementDate));
		objectHandler.storeObject(handle2, objectStochastic);
		return getArray(objectStochastic, handle);
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_BLACKSCHOLES: ") + e.what());
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
		string handle2 = OUStringToString(handle);
		string handleStochastic2 = OUStringToString(handleStochastic);
		string type2 = OUStringToString(typeOption);
		boost::shared_ptr<ObjectStochastic> objectStochastic = 
			boost::dynamic_pointer_cast<ObjectStochastic>
			(objectHandler.retrieveObject(handleStochastic2));
		if (!objectStochastic)
			QL_FAIL("error retrieving object " + handleStochastic2);
		Date exerciseDate(exerciseDateNum);
		Date settlementDate(settlementDateNum);
		obj_ptr objectOption(
			new ObjectOption(objectStochastic, type2, strike, timeSteps,
			exerciseDate, settlementDate));
		objectHandler.storeObject(handle2, objectOption);
		return getArray(objectOption, handle);
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_OPTION: ") + e.what());
		THROW_RTE;
	}
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionSetEngine( 
			const STRING& handle,
			sal_Int32 engineID,
			sal_Int32 timeSteps) THROWDEF_RTE_IAE {
	try {
		string handle2 = OUStringToString(handle);
		string engineName;
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
			(objectHandler.retrieveObject(handle2));
		if (!objectOption)
			QL_FAIL("error retrieving object " + handle2);
		objectOption->setEngine(engineName, timeSteps);
		return getArray(objectOption, handle);
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		THROW_RTE;
	}
}

