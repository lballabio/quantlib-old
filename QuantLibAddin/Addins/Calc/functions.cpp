#include "qladdin.hpp"
#include "QuantLibAddin/objectoption.hpp"
#include "utilities.hpp"

extern ObjectHandler objectHandler;

SEQSEQ(ANY) SAL_CALL QLAddin::qlQuery(
			const STRING& handleObject) THROWDEF_RTE_IAE {
	try {
		string handleObject2 = OUStringToString(handleObject);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject2));
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
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_FIELDNAMES: ") + e.what());
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

