
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
extern "C" {
#include <Addins/C/varies.h>
}
#include <Addins/C/varies.hpp>
#include <oh/objecthandler.hpp> 
#include <ql/patterns/singleton.hpp>

void initialize() {
// instantiate the objecthandler singleton
static ObjectHandler::Repository oh;
}

// code stub for unsupported sessions functionality

#ifdef QL_ENABLE_SESSIONS

QuantLib::Integer QuantLib::sessionId() {
    return 0;
}

#endif

void variesToScalar(long &ret, const Varies &value, const long &defaultValue) {
    ret = defaultValue;
}

void variesToScalar(double &ret, const Varies &value, const double &defaultValue) {
}

void variesToScalar(bool &ret, const Varies &value, const bool &defaultValue) {
}

void variesToScalar(std::string &ret, const Varies &value, const std::string &defaultValue) {
}

