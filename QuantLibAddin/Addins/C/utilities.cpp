
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
}

using namespace ObjHandler;
using namespace QuantLibAddin;

const char *QL_VER() {
    static std::string ret = QuantLibAddin::QL_VER();
    return ret.c_str();
}

const char *QL_OH_VER() {
    static std::string ret = QuantLibAddin::QL_OH_VER();
    return ret.c_str();
}

const char *QL_LOGFILE(const char *logFileName) {
    OH_LOGFILE(logFileName);
    static std::string ret(logFileName);
    return ret.c_str();
}

void QL_CONSOLE(const int console) {
    OH_CONSOLE(console);
}

void QL_LOGMESSAGE(
        const char *fmt,
        ...) {
    char buf[1000]; // FIXME
    va_list list;
    va_start(list, fmt);
    vsprintf(buf, fmt, list);
    va_end(list);
    OH_LOGMESSAGE(buf);
}

