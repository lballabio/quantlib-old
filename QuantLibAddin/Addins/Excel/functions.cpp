#include "utilities.hpp"
#include <sstream>
#include <ql/quantlib.hpp>	// QL_FAIL

using std::ostringstream;

extern ObjectHandler objectHandler;

LPXLOPER QL_QUERY(char *handleObject_char) {
	try {
		string handleObject(handleObject_char);
		boost::shared_ptr<Object> object = 
			(objectHandler.retrieveObject(handleObject));
		if (!object)
			QL_FAIL("error retrieving object " + handleObject);

		Properties properties = object->getProperties();
		static XLOPER xRet;
		xRet.xltype = xltypeMulti;
		xRet.val.array.rows = properties.size();
		xRet.val.array.columns = 2;
		// FIXME - memory allocated below gets leaked - need to set xlbitXLFree ?
		xRet.val.array.lparray = new XLOPER[2 * properties.size()];
		if (!xRet.val.array.lparray)
			QL_FAIL("error on call to new");
		for (unsigned int i = 0; i < properties.size(); i++) {
			ObjectProperty property = properties[i];
			any_ptr a = property();
			setXLOPERString(xRet.val.array.lparray[i * 2], property.name().c_str());
			anyToXLOPER(a, xRet.val.array.lparray[i * 2 + 1]);
		}
		return &xRet;
	} catch (const exception &e) {
		logMessage(string("ERROR: QL_FIELDNAMES: ") + e.what());
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
