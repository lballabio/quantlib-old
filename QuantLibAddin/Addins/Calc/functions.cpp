#include "qladdin.hpp"
#include "ObjectClassLibrary/objectoption.hpp"
#include "utilities.hpp"

extern ObjectHandler objectHandler;

SEQSEQ( STRING ) SAL_CALL QLAddin::qlFieldNames(
			const STRING& handleObject) THROWDEF_RTE_IAE {
	try {
		string handleObject2 = OUStringToString(handleObject);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject2));
		if (!object)
			QL_FAIL("error retrieving object " + handleObject2);
		vector < string > fieldNames = object->getFieldNames();
		SEQSEQ(STRING) rows(fieldNames.size());
		for (int i = 0; i < fieldNames.size(); i++) {
			SEQ(STRING) row(1);
			row[0] = STRFROMASCII(fieldNames[i].c_str());
			rows[i] = row;
		}
		return rows;
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_FIELDNAMES: ") + e.what());
		THROW_RTE;
	}
}

ANY SAL_CALL QLAddin::qlValue(
			const STRING& handleObject,
			const STRING& fieldName) THROWDEF_RTE_IAE {
	try {
		string handleObject2 = OUStringToString(handleObject);
		string fieldName2 = OUStringToString(fieldName);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject2));
		if (!object)
			QL_FAIL("error retrieving object " + handleObject2);
		any_ptr a = object->getValue(fieldName2);
		return anyToANY(a);
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_VALUE: ") + e.what());
		THROW_RTE;
	}
}

STRING SAL_CALL QLAddin::qlLogfile(
			const STRING& logFileName) THROWDEF_RTE_IAE {
	try {
		string logFileName2 = OUStringToString(logFileName);
		if (setLogFile(string(logFileName2)))
			return logFileName;
		else
			return STRFROMASCII("logging disabled");
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_LOGFILE: ") + e.what());
		THROW_RTE;
	}
}

