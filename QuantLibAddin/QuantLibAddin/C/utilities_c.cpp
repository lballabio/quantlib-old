/*
 Copyright (C) 2004 Eric Ehlers

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

#include <ObjectHandler/propertyvector.hpp>
extern "C" {
#include <QuantLibAddin/C/utilities.h>
}
#include <QuantLibAddin/qladdin.hpp>

using namespace QuantLibAddin;

const char *QL_LOGFILE_C(const char *logFileName) {
	std::string ret = QL_LOGFILE(logFileName);
	return ret.c_str();
}

void QL_LOGMESSAGE_C(const char *msg) {
	QL_LOGMESSAGE(msg);
}
