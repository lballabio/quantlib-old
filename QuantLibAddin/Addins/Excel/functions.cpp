
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

#include <QuantLibAddin/qladdin.hpp>
#include <Addins/Excel/utilities.hpp>
#include <sstream>

using namespace ObjHandler;
using namespace QuantLibAddin;

LPXLOPER qlQuery(char *handleObject) {
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
			setXLOPERString(xRet.val.array.lparray[i * 2], property.name().c_str());
			anyToXLOPER(a, xRet.val.array.lparray[i * 2 + 1]);
		}
		return &xRet;
	} catch (const exception &e) {
		QL_LOGMESSAGE(std::string("ERROR: QL_FIELDNAMES: ") + e.what());
		return 0;
	}
}

LPXLOPER qlLogfile(char *logFileName) {
	static XLOPER xRet;
	std::string ret = QL_LOGFILE(std::string(logFileName));
	setXLOPERString(xRet, ret.c_str());
	return &xRet;
}
