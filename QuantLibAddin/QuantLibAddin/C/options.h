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

#ifndef options_h
#define options_h

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

int QL_OPTION_SETENGINE_C(
	const char *handleOption, 
	const char *engineName,
	const long timeSteps,
	VariesList *result);

#endif

