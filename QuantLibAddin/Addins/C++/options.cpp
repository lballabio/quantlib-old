#include "qladdin.hpp"

extern ObjectHandler objectHandler;

void QL_BLACKSCHOLES(
		const string &handleStochastic, 
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate) {
	obj_ptr objectStochastic(
		new ObjectStochastic(dividendYield, riskFreeRate, volatility, underlying,
			todaysDate, settlementDate));
    objectHandler.storeObject(handleStochastic, objectStochastic);
}

void QL_OPTION(
	const string &handleOption, 
	const string &handleStochastic,
	const string &type,
	const Real &strike,
	const Size &timeSteps,
	const Date &exerciseDate,
	const Date &settlementDate) {
	boost::shared_ptr<ObjectStochastic> objectStochastic = 
		boost::dynamic_pointer_cast<ObjectStochastic>
		(objectHandler.retrieveObject(handleStochastic));
	if (!objectStochastic)
		QL_FAIL("QL_OPTION: error retrieving object " + handleStochastic);
	obj_ptr objectOption(
		new ObjectOption(objectStochastic, type, strike, timeSteps,
		exerciseDate, settlementDate));
    objectHandler.storeObject(handleOption, objectOption);
}

void QL_OPTION_SETENGINE(
	const string &handleOption, 
	const string &engineName,
	const Size &timeSteps) {
	boost::shared_ptr<ObjectOption> objectOption = 
		boost::dynamic_pointer_cast<ObjectOption>
		(objectHandler.retrieveObject(handleOption));
	if (!objectOption)
		QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handleOption);
	objectOption->setEngine(engineName, timeSteps);
}
