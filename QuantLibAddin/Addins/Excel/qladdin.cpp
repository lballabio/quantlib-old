
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

// this file generated automatically by autogen.py on Sat Jan 29 13:00:36 2005
// editing this file manually is not recommended

#include <windows.h>
#include <Addins/Excel/xlcall.h>
#include <Addins/Excel/framewrk.hpp>

extern "C" __declspec(dllexport) int xlAutoOpen() {
    static XLOPER xDll;
    Excel(xlGetName, &xDll, 0);

    // functions

    Excel(xlfRegister, 0, 12, &xDll,
        TempStr(" qlQuery"),
        TempStr(" RC"),
        TempStr(" QL_QUERY"),
        TempStr(" handleObject"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" retrieve the properties of a given object"),
        TempStr(" qlQuery"),
        TempStr(" handle of object to be queried"));

    Excel(xlfRegister, 0, 12, &xDll,
        TempStr(" qlLogfile"),
        TempStr(" RC"),
        TempStr(" QL_LOGFILE"),
        TempStr(" logFileName"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" begin logging to named file"),
        TempStr(" qlLogfile"),
        TempStr(" path and name of log file"));

    // options

    Excel(xlfRegister, 0, 18, &xDll,
        TempStr(" qlStochasticProcess"),
        TempStr(" RCECNEEE#"),
        TempStr(" QL_STOCHASTIC_PROCESS"),
        TempStr(" handle,underlying,dayCounterID,settlementDate,riskFreeRate,dividendYield,volatility"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" construct and return a handle to a Stochastic Process object"),
        TempStr(" qlStochasticProcess"),
        TempStr(" handle of new object"),
        TempStr(" underlying"),
        TempStr(" ID of day counter"),
        TempStr(" settlement date"),
        TempStr(" risk free rate"),
        TempStr(" dividend yield"),
        TempStr(" volatility"));

    Excel(xlfRegister, 0, 21, &xDll,
        TempStr(" qlOptionVanilla"),
        TempStr(" RCCCCECNNCN#"),
        TempStr(" QL_OPTION_VANILLA"),
        TempStr(" handle,handleStochastic,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" construct and return a handle to a Vanilla Option object"),
        TempStr(" qlOptionVanilla"),
        TempStr(" handle of new object"),
        TempStr(" handle of the Stochastic Process object"),
        TempStr(" option type"),
        TempStr(" payoff type"),
        TempStr(" strike"),
        TempStr(" exercise type"),
        TempStr(" exercise date"),
        TempStr(" settlement date"),
        TempStr(" engine type"),
        TempStr(" time steps"));

    Excel(xlfRegister, 0, 22, &xDll,
        TempStr(" qlOptionAsianC"),
        TempStr(" RCCCCCECNNCN#"),
        TempStr(" QL_OPTION_ASIAN_C"),
        TempStr(" handle,handleStochastic,typeAverage,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" construct and return a handle to a ContinuousAveragingAsianOption object"),
        TempStr(" qlOptionAsianC"),
        TempStr(" handle of new object"),
        TempStr(" handle of the Stochastic Process object"),
        TempStr(" average type"),
        TempStr(" option type"),
        TempStr(" payoff type"),
        TempStr(" strike"),
        TempStr(" exercise type"),
        TempStr(" exercise date"),
        TempStr(" settlement date"),
        TempStr(" engine type"),
        TempStr(" time steps"));

    Excel(xlfRegister, 0, 24, &xDll,
        TempStr(" qlOptionBarrier"),
        TempStr(" RCCCEECCECNNCN#"),
        TempStr(" QL_OPTION_BARRIER"),
        TempStr(" handle,handleStochastic,typeBarrier,barrier,rebate,typeOption,typePayoff,strike,typeExercise,exerciseDate,settlementDate,typeEngine,timeSteps"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" construct and return a handle to a Barrier Option object"),
        TempStr(" qlOptionBarrier"),
        TempStr(" handle of new object"),
        TempStr(" handle of the Stochastic Process object"),
        TempStr(" barrier type"),
        TempStr(" barrier"),
        TempStr(" rebate"),
        TempStr(" option type"),
        TempStr(" payoff type"),
        TempStr(" strike"),
        TempStr(" exercise type"),
        TempStr(" exercise date"),
        TempStr(" settlement date"),
        TempStr(" engine type"),
        TempStr(" time steps"));

    Excel(xlfRegister, 0, 14, &xDll,
        TempStr(" qlOptionSetEngine"),
        TempStr(" RCCN"),
        TempStr(" QL_OPTION_SETENGINE"),
        TempStr(" handle,engineName,timeSteps"),
        TempStr(" 1"),
        TempStr(" QuantLib"),
        TempStr(" "),
        TempStr(" "),
        TempStr(" change the pricing engine of an existing Option object"),
        TempStr(" qlOptionSetEngine"),
        TempStr(" handle of the Option object"),
        TempStr(" engine name"),
        TempStr(" time steps"));

    Excel(xlFree, 0, 1, &xDll);
    return 1;
}

extern "C" __declspec(dllexport) void xlAutoFree(LPXLOPER px) {
    if (px->xltype == xltypeMulti && px->val.array.lparray) {
        unsigned short size = px->val.array.rows * px->val.array.columns;
        for (unsigned short i = 0; i < size; i++)
            if (px->val.array.lparray[i].xltype == xltypeStr
            &&  px->val.array.lparray[i].val.str)
                delete [] px->val.array.lparray[i].val.str;
        delete [] px->val.array.lparray;
    }
}
