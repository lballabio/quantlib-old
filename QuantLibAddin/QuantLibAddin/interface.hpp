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

#ifndef interface_h
#define interface_h

#include <ObjectHandler/propertyvector.hpp>
using ObjHandler::Properties;
using std::string;

// options

Properties QL_BLACKSCHOLES(
	const string &handleStochastic, 
	const double &dividendYield,
	const double &riskFreeRate,
	const double &volatility,
	const double &underlying,
	const long &todaysDate,
	const long &settlementDate);

Properties QL_OPTION(
	const string &handleOption, 
	const string &handleStochastic,
	const string &type,
	const double &strike,
	const long &timeSteps,
	const long &exerciseDate,
	const long &settlementDate);

Properties QL_OPTION_SETENGINE(
	const string &handleOption, 
	const string &engineName,
	const long &timeSteps);

// utilities

string QL_LOGFILE(
	const string &logFileName);

void QL_LOGMESSAGE(
	const string &msg);

string QL_ANY2STRING(
	const ObjHandler::any_ptr &a);

Properties QL_QUERY(
	const string &handle);

#endif
