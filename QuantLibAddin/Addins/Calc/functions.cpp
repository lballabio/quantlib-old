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

#include <Addins/Calc/qladdin.hpp>
#include <QuantLibAddin/objectoption.hpp>
#include <Addins/Calc/utilities.hpp>

using namespace ObjHandler;

SEQSEQ(ANY) SAL_CALL QLAddin::qlQuery(
			const STRING& handleObject) THROWDEF_RTE_IAE {
	try {
		std::string handleObject2 = OUStringToString(handleObject);
		boost::shared_ptr<Object> object = 
			ObjectHandler::instance().retrieveObject(handleObject2);
		if (!object)
			QL_FAIL("error retrieving object " + handleObject2);
		Properties properties = object->getProperties();
		SEQSEQ( ANY ) rows(properties.size());
		for (int i = 0; i < properties.size(); i++) {
			SEQ( ANY ) row(2);
			ObjectProperty property = properties[i];
			any_ptr a = property();
			row[0] = stringToANY(property.name());
			row[1] = anyToANY(a);
			rows[i] = row;
		}
		return rows;
	} catch (const std::exception &e) {
		logMessage(std::string("ERROR: QL_FIELDNAMES: ") + e.what());
		THROW_RTE;
	}
}

STRING SAL_CALL QLAddin::qlLogfile(
			const STRING& logFileName) THROWDEF_RTE_IAE {
	try {
		std::string logFileName2 = OUStringToString(logFileName);
		if (setLogFile(std::string(logFileName2)))
			return logFileName;
		else
			return STRFROMASCII("logging disabled");
	} catch (const std::exception &e) {
		logMessage(std::string("ERROR: QL_LOGFILE: ") + e.what());
		THROW_RTE;
	}
}

