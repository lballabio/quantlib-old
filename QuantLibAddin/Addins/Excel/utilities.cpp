
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

#include <qla/qladdin.hpp>
#include <Addins/Excel/xlutils.hpp>
#include <sstream>

using namespace ObjHandler;
using namespace QuantLibAddin;

DLLEXPORT LPXLOPER qlVer() {
    static XLOPER xRet;
    std::string ret = QL_VER();
    stringToXLOPER(xRet, ret.c_str());
    return &xRet;
}

DLLEXPORT LPXLOPER qlOhVer() {
    static XLOPER xRet;
    std::string ret = QL_OH_VER();
    stringToXLOPER(xRet, ret.c_str());
    return &xRet;
}

/*
DLLEXPORT LPXLOPER qlQuery(char *handleObject) {
    try {
        Properties properties = QL_QUERY(std::string(handleObject));
        static XLOPER xRet;
        xRet.xltype = xltypeMulti;
        xRet.xltype |= xlbitDLLFree;
        xRet.val.array.rows = properties.size();
        xRet.val.array.columns = 2;
        xRet.val.array.lparray = new XLOPER[2 * properties.size()];
        if (!xRet.val.array.lparray)
            throw Exception("error on call to new");
        for (unsigned int i = 0; i < properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            stringToXLOPER(xRet.val.array.lparray[i * 2], property.name().c_str());
            anyToXLOPER(a, xRet.val.array.lparray[i * 2 + 1]);
        }
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_QUERY: ") + e.what());
        return 0;
    }
}
*/

DLLEXPORT LPXLOPER qlFieldNames(char *handleObject) {
    static XLOPER xRet;
    xRet.val.array.lparray = 0;
    try {
        Properties properties = QL_QUERY(std::string(handleObject));
        xRet.xltype = xltypeMulti;
        xRet.xltype |= xlbitDLLFree;
        xRet.val.array.rows = properties.size();
        xRet.val.array.columns = 1;
        xRet.val.array.lparray = new XLOPER[properties.size()];
        if (!xRet.val.array.lparray)
            throw Exception("error on call to new");
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            stringToXLOPER(xRet.val.array.lparray[i], property.name().c_str());
        }
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_FIELDNAMES: ") + e.what());
        if (xRet.val.array.lparray)
            delete [] xRet.val.array.lparray;
        return 0;
    }
}

DLLEXPORT LPXLOPER qlValue(char *handleObject, char *fieldName) {
    try {
        Properties properties = QL_QUERY(std::string(handleObject));
        static XLOPER xRet;
        std::string fieldNameUpper = QuantLib::StringFormatter::toUppercase(fieldName);
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            if (property.name().compare(fieldNameUpper) == 0) {
                anyToXLOPER(a, xRet, true);
                return &xRet;
            }
        }
        throw Exception(std::string("no field with name ") + fieldNameUpper);
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_VALUE: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlLogfile(char *logFileName, long *logLevel) {
    static XLOPER xRet;
    int lvl = *logLevel ? *logLevel : 4;
    std::string ret = QL_LOGFILE(std::string(logFileName), lvl);
    stringToXLOPER(xRet, ret.c_str());
    return &xRet;
}

DLLEXPORT LPXLOPER qlLogMessage(char *logMessage, long *logLevel) {
    static XLOPER xRet;
    int lvl = *logLevel ? *logLevel : 4;
    std::string ret = QL_LOGMESSAGE(std::string(logMessage), lvl);
    stringToXLOPER(xRet, ret.c_str());
    return &xRet;
}

DLLEXPORT LPXLOPER qlLogLevel(long *logLevel) {
    static XLOPER xRet;
    QL_LOGLEVEL(*logLevel);
    xRet.xltype = xltypeInt;
    xRet.val.w = *logLevel;
    return &xRet;
}
