
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

DLLEXPORT char* qlVer() {
    try {
        static char ret[XL_MAX_STR_LEN];
        std::string ver = QL_VER();
        int len = __min(XL_MAX_STR_LEN - 1, ver.length());
        strncpy(ret, ver.c_str(), len);
        ret[len] = 0;
        return ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_VER: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT char* qlOhVer() {
    try {
        static char ret[XL_MAX_STR_LEN];
        std::string ver = QL_OH_VER();
        int len = __min(XL_MAX_STR_LEN - 1, ver.length());
        strncpy(ret, ver.c_str(), len);
        ret[len] = 0;
        return ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OH_VER: ") + e.what(), 2);
        return 0;
    }
}

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
        QL_LOGMESSAGE(std::string("ERROR: QL_FIELDNAMES: ") + e.what(), 2);
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
        QL_LOGMESSAGE(std::string("ERROR: QL_VALUE: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT char* qlLogfile(char *logFileName, long *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        int lvl = *logLevel ? *logLevel : 4;
        std::string log = QL_LOGFILE(std::string(logFileName), lvl);
        int len = __min(XL_MAX_STR_LEN - 1, log.length());
        strncpy(ret, log.c_str(), len);
        ret[len] = 0;
        return ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT char* qlLogMessage(char *logMessage, long *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        int lvl = *logLevel ? *logLevel : 4;
        std::string msg = QL_LOGMESSAGE(std::string(logMessage), lvl);
        int len = __min(XL_MAX_STR_LEN - 1, msg.length());
        strncpy(ret, msg.c_str(), len);
        ret[len] = 0;
        return ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT long* qlLogLevel(long *logLevel) {
    try {
        static long ret;
        QL_LOGLEVEL(*logLevel);
        ret = *logLevel;
        return &ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT short int* qlLogObject(char *handleObject) {
    try {
        static short int ret = FALSE;
        QL_LOG_OBJECT(handleObject);
        return &ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_LOG_OBJECT: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* qlLogAllObjects() {
    try {
        static short int ret = FALSE;
        QL_LOG_ALL_OBJECTS();
        return &ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_LOG_ALL_OBJECTS: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* qlObjectDelete(char *handleObject) {
    try {
        static short int ret = FALSE;
        QL_OBJECT_DELETE(handleObject);
        return &ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OBJECT_DELETE: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* qlObjectDeleteAll() {
    try {
        static short int ret = FALSE;
        QL_OBJECT_DELETE_ALL();
        return &ret;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OBJECT_DELETE_ALL: ") + e.what(), 2);
        return 0;
    }
}

