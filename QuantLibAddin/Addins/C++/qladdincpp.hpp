#ifndef qladdincpp_h
#define qladdincpp_h

#include <QuantLibAddin/qladdindefines.hpp>
#include <string>
using std::string;

// options

Properties QL_BLACKSCHOLES(
	const string &handleStochastic, 
	const Spread &dividendYield,
	const Rate &riskFreeRate,
	const Volatility &volatility,
	const Real &underlying,
	const Date &todaysDate,
	const Date &settlementDate);

Properties QL_OPTION(
	const string &handleOption, 
	const string &handleStochastic,
	const string &type,
	const Real &strike,
	const Size &timeSteps,
	const Date &exerciseDate,
	const Date &settlementDate);

Properties QL_OPTION_SETENGINE(
	const string &handleOption, 
	const string &engineName,
	const Size &timeSteps);

// utilities

void QL_LOGFILE(
	const string &logFileName);

void QL_LOGMESSAGE(
	const string &msg);

#endif
