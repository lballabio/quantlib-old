#include "utilities.hpp"
#include <string>
#include <sstream>
#include "QuantLibAddin/ObjectOption.hpp"
using std::ostringstream;
using std::string;
using namespace ObjHandler;
using namespace QuantLib;

extern ObjectHandler objectHandler;

LPXLOPER QL_BLACKSCHOLES(
		double *dividendYield,
		double *riskFreeRate,
		double *volatility,
		double *underlying,
		long int *todaysDateNum,
		long int *settlementDateNum) {
	try {
		string handleStochastic = getCaller();
		Date todaysDate(*todaysDateNum);
		Date settlementDate(*settlementDateNum);
		obj_ptr objectStochastic(
			new ObjectStochastic(*dividendYield, *riskFreeRate, *volatility, 
				*underlying, todaysDate, settlementDate));
		objectHandler.storeObject(handleStochastic, objectStochastic);
		static XLOPER xRet;
		setValues(&xRet, objectStochastic, handleStochastic);
		return &xRet;
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_BLACKSCHOLES: ") + e.what());
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
		string handleStochastic(handleStochastic_char);
		boost::shared_ptr<ObjectStochastic> objectStochastic = 
			boost::dynamic_pointer_cast<ObjectStochastic>
			(objectHandler.retrieveObject(handleStochastic));
		if (!objectStochastic)
			QL_FAIL("error retrieving object " + handleStochastic);
		Date exerciseDate(*exerciseDateNum);
		Date settlementDate(*settlementDateNum);
		obj_ptr objectOption(
			new ObjectOption(objectStochastic, type, *strike, *timeSteps,
			exerciseDate, settlementDate));
		string handleOption = getCaller();
		objectHandler.storeObject(handleOption, objectOption);
		static XLOPER xRet;
		setValues(&xRet, objectOption, handleOption);
		return &xRet;
	} catch(const exception &e) {
		logMessage(string("ERROR: QL_OPTION: ") + e.what());
		return 0;
	}
}

LPXLOPER QL_OPTION_SETENGINE(
		char *handleOption_char,
		char *engineName_char,
		long int *timeSteps) {
	try {
		string handleOption(handleOption_char);
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption>
			(objectHandler.retrieveObject(handleOption));
		if (!objectOption)
			QL_FAIL("error retrieving object " + handleOption);
		string engineName(engineName_char);
		objectOption->setEngine(engineName, *timeSteps);
		static XLOPER xRet;
		setValues(&xRet, objectOption, handleOption);
		return &xRet;
	} catch(const exception &e) {
		logMessage(string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
		return 0;
	}
}
