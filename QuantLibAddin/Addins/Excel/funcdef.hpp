
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

// this file generated automatically by autogen.py on Thu Jan 20 00:11:04 2005
// editing this file manually is not recommended

#define NUM_FUNCS 7
#define NUM_ATTS 24

static LPSTR func[NUM_FUNCS][NUM_ATTS] = {
    // functions
    {   " qlQuery",                   // function name (code)
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
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
    },

    {   " qlLogfile",                 // function name (code)
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
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
    },

    // options
    {   " qlStochasticProcess",       // function name (code)
        " RECNEEE#",                  // parameter types
        " QL_STOCHASTIC_PROCESS",     // function name (Excel)
        // parameter names:
        " underlying,dayCounterID,settlementDate,riskFreeRate,dividendYield,volatility",
        " 1",                         // macro type
        " QuantLib",                  // category
        " ",                          // shortcut text
        " ",                          // help topic
        // function help:
        " construct and return a handle to a Stochastic Process object",
        " underlying",                // param 0
        " ID of day counter",         // param 1
        " settlement date",           // param 2
        " risk free rate",            // param 3
        " dividend yield",            // param 4
        " volatility",                // param 5
        // unused params:
        " ", " ", " ", " ", " ", " ", " ", " ", " "
    },

    {   " qlOptionVanilla",           // function name (code)
        " RCCCECNNCN#",               // parameter types
        " QL_OPTION_VANILLA",         // function name (Excel)
        // parameter names:
        " handleStochastic,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps",
        " 1",                         // macro type
        " QuantLib",                  // category
        " ",                          // shortcut text
        " ",                          // help topic
        // function help:
        " construct and return a handle to a Vanilla Option object",
        " handle of the Stochastic Process object",// param 0
        " option type",               // param 1
        " payoff type",               // param 2
        " strike",                    // param 3
        " exercise type",             // param 4
        " exercise date",             // param 5
        " settlement date",           // param 6
        " engine type",               // param 7
        " time steps",                // param 8
        // unused params:
        " ", " ", " ", " ", " ", " "
    },

    {   " qlOptionAsianC",            // function name (code)
        " RCCCCECNNCN#",              // parameter types
        " QL_OPTION_ASIAN_C",         // function name (Excel)
        // parameter names:
        " handleStochastic,typeAverage,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps",
        " 1",                         // macro type
        " QuantLib",                  // category
        " ",                          // shortcut text
        " ",                          // help topic
        // function help:
        " construct and return a handle to a ContinuousAveragingAsianOption object",
        " handle of the Stochastic Process object",// param 0
        " average type",              // param 1
        " option type",               // param 2
        " payoff type",               // param 3
        " strike",                    // param 4
        " exercise type",             // param 5
        " exercise date",             // param 6
        " settlement date",           // param 7
        " engine type",               // param 8
        " time steps",                // param 9
        // unused params:
        " ", " ", " ", " ", " "
    },

    {   " qlOptionBarrier",           // function name (code)
        " RCCEECCECNNCN#",            // parameter types
        " QL_OPTION_BARRIER",         // function name (Excel)
        // parameter names:
        " handleStochastic,typeBarrier,barrier,rebate,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps",
        " 1",                         // macro type
        " QuantLib",                  // category
        " ",                          // shortcut text
        " ",                          // help topic
        // function help:
        " construct and return a handle to a Barrier Option object",
        " handle of the Stochastic Process object",// param 0
        " barrier type",              // param 1
        " barrier",                   // param 2
        " rebate",                    // param 3
        " option type",               // param 4
        " payoff type",               // param 5
        " strike",                    // param 6
        " exercise type",             // param 7
        " exercise date",             // param 8
        " settlement date",           // param 9
        " engine type",               // param 10
        " time steps",                // param 11
        // unused params:
        " ", " ", " "
    },

    {   " qlOptionSetEngine",         // function name (code)
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
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
    },

};
