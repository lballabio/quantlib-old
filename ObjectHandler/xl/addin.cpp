
/*
 Copyright (C) 2005 Eric Ehlers

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

#include <windows.h>
#include <xl/xlcall.h>
#include <xl/framewrk.hpp>

DLLEXPORT void xlAutoFree(LPXLOPER px) {
    if (px->xltype == xltypeMulti && px->val.array.lparray) {
        unsigned short size = px->val.array.rows * px->val.array.columns;
        for (unsigned short i = 0; i < size; i++)
            if (px->val.array.lparray[i].xltype == xltypeStr
            &&  px->val.array.lparray[i].val.str)
                delete [] px->val.array.lparray[i].val.str;
        delete [] px->val.array.lparray;
    }
}

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    Excel(xlGetName, &xDll, 0);

    Excel(xlfRegister, 0, 10, &xDll,
        TempStrNoSize("\x09""ohVersion"),       // function code name
        TempStrNoSize("\x01""C"),               // parameter codes
        TempStrNoSize("\x0A""OH_VERSION"),      // function display name
        TempStrNoSize("\x00"""),                // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x2B""returns the version number of ObjectHandler"));// function description

    Excel(xlfRegister, 0, 11, &xDll,
        TempStrNoSize("\x0A""fieldNames"),      // function code name
        TempStrNoSize("\x02""RC"),              // parameter codes
        TempStrNoSize("\x0E""OH_FIELD_NAMES"),  // function display name
        TempStrNoSize("\x06""handle"),          // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x1A""display object field names"), // function description
        TempStrNoSize("\x1E""handle of object to be queried"));// description param 0

    Excel(xlfRegister, 0, 12, &xDll,
        TempStrNoSize("\x0A""fieldValue"),      // function code name
        TempStrNoSize("\x03""RCC"),             // parameter codes
        TempStrNoSize("\x0E""OH_FIELD_VALUE"),  // function display name
        TempStrNoSize("\x10""handle,fieldName"),// comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x16""display value of field"),// function description
        TempStrNoSize("\x1E""handle of object to be queried"),// description param 0
        TempStrNoSize("\x0D""name of field"));// description param 0

    Excel(xlfRegister, 0, 11, &xDll,
        TempStrNoSize("\x0C""deleteObject"),    // function code name
        TempStrNoSize("\x02""LC"),              // parameter codes
        TempStrNoSize("\x10""OH_DELETE_OBJECT"),// function display name
        TempStrNoSize("\x0C""handleObject"),    // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x1D""delete object from repository"),// function description
        TempStrNoSize("\x1E""handle of object to be deleted"));// description param 0

    Excel(xlfRegister, 0, 10, &xDll,
        TempStrNoSize("\x10""deleteAllObjects"),// function code name
        TempStrNoSize("\x01""L"),               // parameter codes
        TempStrNoSize("\x15""OH_DELETE_ALL_OBJECTS"),// function display name
        TempStrNoSize("\x00"""),                // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x22""delete all objects from repository"));// function description

    Excel(xlfRegister, 0, 12, &xDll,
        TempStrNoSize("\x0A""setLogFile"),      // function code name
        TempStrNoSize("\x03""CCN"),             // parameter codes
        TempStrNoSize("\x0E""OH_SET_LOGFILE"),  // function display name
        TempStrNoSize("\x14""logFileName,logLevel"),// comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x1B""begin logging to named file"),// function description
        TempStrNoSize("\x19""path and name of log file"),// description param 0
        TempStrNoSize("\x1A""threshold for log messages"));// description param 1

    Excel(xlfRegister, 0, 12, &xDll,
        TempStrNoSize("\x0A""logMessage"),      // function code name
        TempStrNoSize("\x03""CCN"),             // parameter codes
        TempStrNoSize("\x0E""OH_LOG_MESSAGE"),  // function display name
        TempStrNoSize("\x13""logMessage,logLevel"),// comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x0D""log a message"),   // function description
        TempStrNoSize("\x14""message to be logged"),// description param 0
        TempStrNoSize("\x1A""threshold for log messages"));// description param 1

    Excel(xlfRegister, 0, 11, &xDll,
        TempStrNoSize("\x0B""setLogLevel"),     // function code name
        TempStrNoSize("\x02""NN"),              // parameter codes
        TempStrNoSize("\x0F""OH_SET_LOGLEVEL"),// function display name
        TempStrNoSize("\x08""logLevel"),        // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x1E""set threshold for log messages"),// function description
        TempStrNoSize("\x1A""threshold for log messages"));// description param 0

    Excel(xlfRegister, 0, 11, &xDll,
        TempStrNoSize("\x09""logObject"),       // function code name
        TempStrNoSize("\x02""LC"),              // parameter codes
        TempStrNoSize("\x0D""OH_LOG_OBJECT"),   // function display name
        TempStrNoSize("\x0C""handleObject"),    // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x24""write object description to log file"),// function description
        TempStrNoSize("\x1D""handle of object to be logged"));// description param 0

    Excel(xlfRegister, 0, 10, &xDll,
        TempStrNoSize("\x0D""logAllObjects"),   // function code name
        TempStrNoSize("\x01""L"),               // parameter codes
        TempStrNoSize("\x12""OH_LOG_ALL_OBJECTS"),// function display name
        TempStrNoSize("\x00"""),                // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x0D""ObjectHandler"),   // function category
        TempStrNoSize("\x00"""),                // shortcut text (command macros only)
        TempStrNoSize("\x00"""),                // path to help file
        TempStrNoSize("\x29""write all object descriptions to log file"));// function description

    Excel(xlFree, 0, 1, &xDll);
    return 1;
}

DLLEXPORT int xlAutoClose() {
    Excel(xlUDF, 0, 1, TempStrNoSize("\x15""OH_DELETE_ALL_OBJECTS"));
    return 1;
}

