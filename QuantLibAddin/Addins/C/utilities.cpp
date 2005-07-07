
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
#include <cstdarg>

extern "C" {
#include <Addins/C/utilities.h>
#include <Addins/C/varies.h>
#include <Addins/C/defines.h>
}
#include <Addins/C/varies.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

const char *QL_VERSION() {
    static std::string ret = QuantLibAddin::QL_VERSION();
    return ret.c_str();
}

const char *QL_OH_VERSION() {
    static std::string ret = QuantLibAddin::QL_OH_VERSION();
    return ret.c_str();
}

const char *QL_LOGFILE(const char *logFileName) {
    OH_LOGFILE(logFileName);
    static std::string ret(logFileName);
    return ret.c_str();
}

void QL_CONSOLE(const long console) {
    OH_CONSOLE(console);
}

void QL_LOG_MESSAGE(
        const char *fmt,
        ...) {
    char buf[1000]; // FIXME
    va_list list;
    va_start(list, fmt);
    vsprintf(buf, fmt, list);
    va_end(list);
    OH_LOG_MESSAGE(buf);
}

int QL_LIST_REGISTERED_ENUMS(
        char **result) {
    try {
        std::vector < std::string > returnValue = getRegisteredEnums();
        Conversion< std::string >::convertArray(returnValue, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("QL_LIST_REGISTERED_ENUMS Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_LIST_ENUM(
        char *enumId,
        char **result) {
    try {
        std::vector < std::string > returnValue = getEnumMembers(
            std::string(enumId));
        Conversion< std::string >::convertArray(returnValue, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("QL_LIST_ENUM Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_LIST_REGISTERED_TYPES(
        char **result) {
    try {
        std::vector < std::string > returnValue = getRegisteredComplexTypes();
        Conversion< std::string >::convertArray(returnValue, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("QL_LIST_REGISTERED_TYPES Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_LIST_TYPE(
        char *enumId,
        char **result) {
    try {
        std::vector < std::string > returnValue = getComplexTypeMembers(
            std::string(enumId));
        Conversion< std::string >::convertArray(returnValue, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("QL_LIST_TYPE Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}