
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
#include <xlsdk/xlsdk.hpp>
#include <sstream>
#include <ohxl/conversions.hpp>

using namespace QuantLibAddin;

DLLEXPORT char* qlVersion() {
    try {
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, QuantLibAddin::qlVersion());
        return ret;
	} catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: qlVersion: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListRegisteredEnums() {
    try {
        std::vector < std::string > returnValue = 
            getRegisteredEnums();
        static XLOPER xRet;
        ObjHandler::vectorToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: qlListRegisteredEnums: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListEnum(
        char *enumId) {
    try {
        std::vector < std::string > returnValue = 
            getEnumMembers(enumId);
        static XLOPER xRet;
        ObjHandler::vectorToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: qlListEnum: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListRegisteredTypes() {
    try {
        std::vector < std::string > returnValue = 
            getRegisteredComplexTypes();
        static XLOPER xRet;
        ObjHandler::vectorToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: qlListRegisteredTypes: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT XLOPER* qlListType(
        char *typeId) {
    try {
        std::vector < std::string > returnValue = 
            getComplexTypeMembers(typeId);
        static XLOPER xRet;
        ObjHandler::vectorToXloper(xRet, returnValue);
        return &xRet;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: qlListType: ") + e.what(), 2);
        return 0;
    }
}

