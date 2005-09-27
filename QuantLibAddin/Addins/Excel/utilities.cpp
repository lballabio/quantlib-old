
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
		stringToChar(ret, QuantLibAddin::qlVersion());
        return ret;
	} catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_VERSION: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* qlDependsOn(
        OPER *dummy0,
        OPER *dummy1,
        OPER *dummy2,
        OPER *dummy3,
        OPER *dummy4,
        OPER *dummy5,
        OPER *dummy6,
        OPER *dummy7,
        OPER *dummy8,
        OPER *dummy9) {
    try {
        static short int ret = FALSE;
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_DEPENDS_ON: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListRegisteredEnums() {
    try {
        std::vector < std::string > returnValue = 
            getRegisteredEnums();
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_REGISTERED_ENUMS: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListEnum(
        char *enumId) {
    try {
        std::vector < std::string > returnValue = 
            getEnumMembers(enumId);
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_ENUM: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListRegisteredTypes() {
    try {
        std::vector < std::string > returnValue = 
            getRegisteredComplexTypes();
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_REGISTERED_TYPES: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListType(
        char *typeId) {
    try {
        std::vector < std::string > returnValue = 
            getComplexTypeMembers(typeId);
        static XLOPER xRet;
        vectorStringToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: QL_LIST_TYPE: ") + e.what(), 2);
        return 0;
    }
}