#include "qladdincpp.hpp"
using namespace ObjHandler;
using namespace QuantLib;

extern ObjectHandler objectHandler;

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
    objectHandler.storeObject(handleStochastic, objectStochastic);
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
		(objectHandler.retrieveObject(handleStochastic));
	if (!objectStochastic)
		QL_FAIL("QL_OPTION: error retrieving object " + handleStochastic);
	obj_ptr objectOption(
		new ObjectOption(objectStochastic, type, strike, timeSteps,
		exerciseDate, settlementDate));
    objectHandler.storeObject(handleOption, objectOption);
	return objectOption->getProperties();
}

Properties QL_OPTION_SETENGINE(
	const std::string &handleOption, 
	const std::string &engineName,
	const Size &timeSteps) {
	boost::shared_ptr<ObjectOption> objectOption = 
		boost::dynamic_pointer_cast<ObjectOption>
		(objectHandler.retrieveObject(handleOption));
	if (!objectOption)
		QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handleOption);
	objectOption->setEngine(engineName, timeSteps);
	return objectOption->getProperties();
}
