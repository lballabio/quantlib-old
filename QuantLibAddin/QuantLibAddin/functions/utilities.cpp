
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

#include <QuantLibAddin/functions/utilities.hpp>

using namespace ObjHandler;

namespace QuantLibAddin {

	std::string QL_LOGFILE(
			const std::string &logFileName) {
		if (setLogFile(logFileName))
			return logFileName;
		else
			return "logging disabled";
	}

	void QL_LOGMESSAGE(
			const std::string &msg) {
		logMessage(msg);
	}

	std::string QL_ANY2STRING(
			const any_ptr &a) {
		return AnyToString(a);
	}

	Properties QL_QUERY(
			const std::string &handle) {
		boost::shared_ptr<Object> object =
				ObjectHandler::instance().retrieveObject(handle);
		if (!object)
				throw Exception("error retrieving object " + handle);
		return object->getProperties();
	}

}

