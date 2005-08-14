
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

DLLEXPORT char* qlVersion() {
    try {
        static char ret[XL_MAX_STR_LEN];
        std::string ver = QuantLibAddin::qlVersion();
        int len = __min(XL_MAX_STR_LEN - 1, ver.length());
        strncpy(ret, ver.c_str(), len);
        ret[len] = 0;
        return ret;
	} catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_VERSION: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* qlDependsOn(
        LPXLOPER dummy0,
        LPXLOPER dummy1,
        LPXLOPER dummy2,
        LPXLOPER dummy3,
        LPXLOPER dummy4,
        LPXLOPER dummy5,
        LPXLOPER dummy6,
        LPXLOPER dummy7,
        LPXLOPER dummy8,
        LPXLOPER dummy9) {
    try {
        static short int ret = FALSE;
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_DEPENDS_ON: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT LPXLOPER qlListRegisteredEnums() {
    try {
        std::vector < std::string > returnValue;
        returnValue = getRegisteredEnums();
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_REGISTERED_ENUMS: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT LPXLOPER qlListEnum(
        char *enumId) {
    try {
        std::vector < std::string > returnValue;
        returnValue = getEnumMembers(
            enumId);
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_ENUM: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT LPXLOPER qlListRegisteredTypes() {
    try {
        std::vector < std::string > returnValue;
        returnValue = getRegisteredComplexTypes();
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_REGISTERED_TYPES: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT LPXLOPER qlListType(
        char *typeId) {
    try {
        std::vector < std::string > returnValue;
        returnValue = getComplexTypeMembers(
            typeId);
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_TYPE: ") + e.what(), 2);
        return 0;
    }
}