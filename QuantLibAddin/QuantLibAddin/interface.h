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

#define SUCCESS 0
#define FAIL 1

// utilities - to be moved to separate file

// from http://www.kuro5hin.org/story/2002/5/1/142321/9513

typedef enum { INT, LONG, DOUBLE, CHARP } Type;

typedef struct {
	union {
		int AsInt;
		long AsLong;
		double AsDouble;
//		char* AsCharP;
		char AsCharP[100];	// FIXME to be allocated on stack
	};
	Type type;
	char Label[100];		// FIXME to be allocated on stack
} Varies;

typedef struct {
	int count;
	Varies *varies;
} VariesList;

const char *variesToString(const Varies *v);

// options

int QL_BLACKSCHOLES_C(
	const char *handleStochastic, 
	const double dividendYield,
	const double riskFreeRate,
	const double volatility,
	const double underlying,
	const long todaysDate,
	const long settlementDate,
	VariesList *result);

int QL_OPTION_C(
	const char *handleOption, 
	const char *handleStochastic,
	const char *type,
	const double strike,
	const long timeSteps,
	const long exerciseDate,
	const long settlementDate,
	VariesList *result);

//Properties QL_OPTION_SETENGINE(
//	const string &handleOption, 
//	const string &engineName,
//	const long &timeSteps);

// utilities

const char *QL_LOGFILE_C(
	const char *logFileName);

void QL_LOGMESSAGE_C(
	const char *msg);

//string QL_ANY2STRING(
//	const ObjHandler::any_ptr &a);

//Properties QL_QUERY(
//	const string &handle);

#endif

