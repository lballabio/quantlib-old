#include "utilities.hpp"
#include <sstream>
#include <ql/quantlib.hpp>	// QL_FAIL

using std::ostringstream;

extern ObjectHandler objectHandler;

LPXLOPER QL_FIELDNAMES(char *handleObject_char) {
	try {
		string handleObject(handleObject_char);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject));
		if (!object)
			QL_FAIL("error retrieving object " + handleObject);
		vector < string > fieldNames = object->getFieldNames();
		static XLOPER xRet;
		xRet.xltype = xltypeMulti;
		xRet.val.array.rows = fieldNames.size();
		xRet.val.array.columns = 1;
		// FIXME - memory allocated below gets leaked - need to set xlbitXLFree ?
		xRet.val.array.lparray = new XLOPER[fieldNames.size()];
		if (!xRet.val.array.lparray)
			QL_FAIL("error on call to new");
		for (unsigned int i = 0; i < fieldNames.size(); i++)
			setXLOPERString(xRet.val.array.lparray[i], fieldNames[i].c_str());
		return &xRet;
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_FIELDNAMES: ") + e.what());
		return 0;
	}
}

LPXLOPER QL_VALUE(
		char *handleObject_char,
		char *fieldName_char) {
	try {
		string handleObject(handleObject_char);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject));
		if (!object)
			throw(string("error retrieving object " + handleObject).c_str());
		string fieldName(fieldName_char);
		any_ptr any = object->getValue(fieldName);
		static XLOPER xRet;
		anyToXLOPER(any, xRet);
		return &xRet;
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_VALUE: ") + e.what());
		return 0;
	}
}

LPXLOPER QL_LOGFILE(char *logFileName) {
	static XLOPER xRet;
	try {
		if (setLogFile(string(logFileName)))
			setXLOPERString(xRet, logFileName);
		else
			setXLOPERString(xRet, "logging disabled");
	} catch (const exception &e) {
		ostringstream msg;
		msg << "error opening logfile: " << e.what();
		setXLOPERString(xRet, msg.str().c_str());
	}
	return &xRet;
}
