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

extern "C" {
#include "interface.h"
}
#include "interface.hpp"
#include <ObjectHandler/propertyvector.hpp>

using namespace ObjHandler;

// utilities - to be moved to separate file

void propertiesToVaries(const Properties &properties, VariesList *variesList) {
	variesList->count = properties.size();
	variesList->varies = new Varies[properties.size()];  // FIXME this memory gets leaked
    for (int i = 0; i < properties.size(); i++) {
		ObjectProperty property = properties[i];
		sprintf(variesList->varies[i].Label, property.name().c_str());
        any_ptr a = property();
		if (a->type() == typeid(int)) {
			variesList->varies[i].type = INT;
		    variesList->varies[i].AsInt = boost::any_cast<int>(*a);
		} else if (a->type() == typeid(long)) {
			variesList->varies[i].type = LONG;
		    variesList->varies[i].AsLong = boost::any_cast<long>(*a);
		} else if (a->type() == typeid(double)) {
			variesList->varies[i].type = DOUBLE;
		    variesList->varies[i].AsDouble = boost::any_cast<double>(*a);
		} else if (a->type() == typeid(std::string)) {
			variesList->varies[i].type = CHARP;
		    sprintf(variesList->varies[i].AsCharP, (boost::any_cast<std::string>(*a)).c_str());
		} else {
			delete [] variesList->varies;
		    throw Exception("propertiesToVaries: unrecognized type");
		}
    }
}

char c[100];	// FIXME
const char *variesToString(const Varies *v) {
	if (v->type == INT)
		sprintf(c, "%d", v->AsInt);
	else if (v->type == LONG)
		sprintf(c, "%d", v->AsLong);
	else if (v->type == DOUBLE)
		sprintf(c, "%f", v->AsDouble);
	else if (v->type == CHARP)
		sprintf(c, "%s", v->AsCharP);
	else
		throw Exception("variesToString: unrecognized type");
	return c;
}

// options

int QL_BLACKSCHOLES_C(
		const char *handleStochastic, 
		const double dividendYield,
		const double riskFreeRate,
		const double volatility,
		const double underlying,
		const long todaysDate,
		const long settlementDate,
		VariesList *result) {
	try {
		Properties properties = QL_BLACKSCHOLES(handleStochastic, dividendYield,
			riskFreeRate, volatility, underlying, todaysDate, settlementDate);
		propertiesToVaries(properties, result);
		return SUCCESS;
	} catch (const std::exception &e) {
		QL_LOGMESSAGE("QL_BLACKSCHOLES_C Error: " + string(e.what()));
		result = 0;
		return FAIL;
	}
}

int QL_OPTION_C(
		const char *handleOption, 
		const char *handleStochastic,
		const char *type,
		const double strike,
		const long timeSteps,
		const long exerciseDate,
		const long settlementDate,
		VariesList *result) {
	try {
		Properties properties = QL_OPTION(handleOption, handleStochastic, type, 
			strike, timeSteps, exerciseDate, settlementDate);
		propertiesToVaries(properties, result);
		return SUCCESS;
	} catch (const std::exception &e) {
		QL_LOGMESSAGE("QL_OPTION_C Error: " + string(e.what()));
		result = 0;
		return FAIL;
	}
}

// utilities

const char *QL_LOGFILE_C(const char *logFileName) {
	string ret = QL_LOGFILE(logFileName);
	return ret.c_str();
}

void QL_LOGMESSAGE_C(const char *msg) {
	QL_LOGMESSAGE(msg);
}
