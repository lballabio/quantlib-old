
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

// this file generated automatically by autogen.py on Sun Dec 12 14:39:53 2004
// editing this file manually is not recommended

#define NUM_FUNCS 5
#define NUM_ATTS 6

static LPSTR func[NUM_FUNCS][NUM_ATTS] = {
	// functions
	{ " qlQuery",           " RC",       " QL_QUERY", 
		" handleObject", " 1", " QuantLib"},
	{ " qlLogfile",         " RC",       " QL_LOGFILE", 
		" logFileName", " 1", " QuantLib"},
	// options
	{ " qlBlackScholes",    " REEEENN#", " QL_BLACKSCHOLES", 
		" dividendYield,riskFreeRate,volatility,underlying,todaysDate,settlementDate", " 1", " QuantLib"},
	{ " qlOption",          " RCCENNN#", " QL_OPTION", 
		" handleStochastic,typeOption,strike,timeSteps,exerciseDate,settlementDate", " 1", " QuantLib"},
	{ " qlOptionSetEngine", " RCCN",     " QL_OPTION_SETENGINE", 
		" handle,engineName,timeSteps", " 1", " QuantLib"},
};
