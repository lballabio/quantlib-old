
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

// this file generated automatically by autogen.py on Wed Dec 15 22:13:59 2004
// editing this file manually is not recommended

#define NUM_FUNCS 5
#define NUM_ATTS 16

static LPSTR func[NUM_FUNCS][NUM_ATTS] = {
	// functions
	{	" qlQuery",                   // function name (code)
		" RC",                        // parameter types
		" QL_QUERY",                  // function name (Excel)
		// parameter names:
		" handleObject",
		" 1",                         // macro type
		" QuantLib",                  // category
		" ",                          // shortcut text
		" ",                          // help topic
		// function help:
		" retrieve the properties of a given object",
		" handle of object to be queried",// param 0
		// unused params:
		" ", " ", " ", " ", " ", " "
	},

	{	" qlLogfile",                 // function name (code)
		" RC",                        // parameter types
		" QL_LOGFILE",                // function name (Excel)
		// parameter names:
		" logFileName",
		" 1",                         // macro type
		" QuantLib",                  // category
		" ",                          // shortcut text
		" ",                          // help topic
		// function help:
		" begin logging to named file",
		" path and name of log file", // param 0
		// unused params:
		" ", " ", " ", " ", " ", " "
	},

	// options
	{	" qlBlackScholes",            // function name (code)
		" REEEENN#",                  // parameter types
		" QL_BLACKSCHOLES",           // function name (Excel)
		// parameter names:
		" dividendYield,riskFreeRate,volatility,underlying,todaysDate,settlementDate",
		" 1",                         // macro type
		" QuantLib",                  // category
		" ",                          // shortcut text
		" ",                          // help topic
		// function help:
		" construct and return a handle to a Black Scholes object",
		" dividend yield",            // param 0
		" risk free rate",            // param 1
		" volatility",                // param 2
		" underlying",                // param 3
		" today's date",              // param 4
		" settlement date",           // param 5
		// unused params:
		" "
	},

	{	" qlOption",                  // function name (code)
		" RCCENNN#",                  // parameter types
		" QL_OPTION",                 // function name (Excel)
		// parameter names:
		" handleStochastic,typeOption,strike,timeSteps,exerciseDate,settlementDate",
		" 1",                         // macro type
		" QuantLib",                  // category
		" ",                          // shortcut text
		" ",                          // help topic
		// function help:
		" construct and return a handle to an Option object",
		" handle of the Black Scholes object",// param 0
		" option type",               // param 1
		" strike",                    // param 2
		" time steps",                // param 3
		" exercise date",             // param 4
		" settlement date",           // param 5
		// unused params:
		" "
	},

	{	" qlOptionSetEngine",         // function name (code)
		" RCCN",                      // parameter types
		" QL_OPTION_SETENGINE",       // function name (Excel)
		// parameter names:
		" handle,engineName,timeSteps",
		" 1",                         // macro type
		" QuantLib",                  // category
		" ",                          // shortcut text
		" ",                          // help topic
		// function help:
		" change the pricing engine of an existing Option object",
		" handle of the Option object",// param 0
		" engine name",               // param 1
		" time steps",                // param 2
		// unused params:
		" ", " ", " ", " "
	},

};
