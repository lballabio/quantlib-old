#include "utilities.hpp"
#include "framewrk.hpp"
#include <exception>
using std::string;
using namespace ObjHandler;

void anyToXLOPER(const any_ptr &any, XLOPER &xOp) {
	if (any->type() == typeid(int)) {
		xOp.xltype = xltypeInt;
		xOp.val.w = boost::any_cast<int>(*any);
	} else if (any->type() == typeid(double)) {
		xOp.xltype = xltypeNum;
		xOp.val.num = boost::any_cast<double>(*any);
	} else if (any->type() == typeid(string)) {
		string s = boost::any_cast<string>(*any);
		setXLOPERString(xOp, s.c_str());
	} else
		xOp.xltype = xltypeErr;
}

void setValues(LPXLOPER xArray, obj_ptr object, const string &handle) {
	Properties properties = object->getProperties();
	xArray->xltype = xltypeMulti;
	xArray->val.array.rows = 1;
	xArray->val.array.columns = properties.size() + 1;
	// FIXME - memory allocated below gets leaked - need to set xlbitXLFree ?
	xArray->val.array.lparray = new XLOPER[properties.size() + 1]; 
	if (!xArray->val.array.lparray)
		throw("setValues: error on call to new");
	setXLOPERString(xArray->val.array.lparray[0], handle.c_str());
	for (unsigned int i = 0; i < properties.size(); i++) {
		ObjectProperty property = properties[i];
		any_ptr a = property();
		anyToXLOPER(a, xArray->val.array.lparray[i + 1]);
	}
}

string XLOPERtoString(LPXLOPER xOp) {
	XLOPER xStr;
	if (xlretSuccess != Excel4(xlCoerce, &xStr, 2, xOp, TempInt(xltypeStr))) 
		throw exception("XLOPERtoString: error on call to xlCoerce");
	string s;
	s.assign(xStr.val.str + 1, xStr.val.str[0]);
	Excel(xlFree, 0, 1, &xStr);
	return s;
}

string getCaller() {
	XLOPER xCaller, xRef;
	if (xlretSuccess != Excel(xlfCaller, &xCaller, 0))
		throw exception("getCaller: error on call to xlfCaller");
	if (xlretSuccess != Excel(xlfGetCell, &xRef, 2, TempInt(1), &xCaller))
		throw exception("getCaller: error on call to xlfGetCell");
	return "QL#" + XLOPERtoString(&xRef);
}

void setXLOPERString(XLOPER &xStr, const char *s) {
	xStr.xltype = xltypeStr;
	int len = __min(255, strlen(s));	// XLOPER string max length is 255
	xStr.val.str = new char[ len + 1 ]; // caller needs to delete
	if (!xStr.val.str) 
		throw exception("error calling new in function setXLOPERString");
	strncpy(xStr.val.str + 1, s, len);
	xStr.val.str[0] = len;
}
