#ifndef qladdincpp_h
#define qladdincpp_h

#include <QuantLibAddin/qladdindefines.hpp>
#include <string>

// options

ObjHandler::Properties QL_BLACKSCHOLES(
	const std::string &handleStochastic, 
	const QuantLib::Spread &dividendYield,
	const QuantLib::Rate &riskFreeRate,
	const QuantLib::Volatility &volatility,
	const QuantLib::Real &underlying,
	const QuantLib::Date &todaysDate,
	const QuantLib::Date &settlementDate);

ObjHandler::Properties QL_OPTION(
	const std::string &handleOption, 
	const std::string &handleStochastic,
	const std::string &type,
	const QuantLib::Real &strike,
	const QuantLib::Size &timeSteps,
	const QuantLib::Date &exerciseDate,
	const QuantLib::Date &settlementDate);

ObjHandler::Properties QL_OPTION_SETENGINE(
	const std::string &handleOption, 
	const std::string &engineName,
	const QuantLib::Size &timeSteps);

// utilities

void QL_LOGFILE(
	const string &logFileName);

void QL_LOGMESSAGE(
	const string &msg);

#endif
