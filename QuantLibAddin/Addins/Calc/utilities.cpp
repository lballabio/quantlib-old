#include "qladdin.hpp"
#include "QuantLibAddin/objectoption.hpp"
#include "utilities.hpp"
using namespace ObjHandler;

// convert boost::any to Calc Any
ANY anyToANY(const any_ptr &a) {
	if (a->type() == typeid(int)) {
		int i1 = boost::any_cast<int>(*a);
		sal_Int32 i2 = static_cast< sal_Int32 >(i1);
		return CSS::uno::makeAny(i2);
	} else if (a->type() == typeid(double)) {
		double d = boost::any_cast<double>(*a);
		return CSS::uno::makeAny(d);
	} else if (a->type() == typeid(string)) {
		string s1 = boost::any_cast<string>(*a);
		STRING s2 = STRFROMASCII( s1.c_str() );
		return CSS::uno::makeAny(s2);
	} else
		QL_FAIL("anyToANY: unable to interpret value");
}

ANY stringToANY(const string &s) {
	STRING s2 = STRFROMASCII( s.c_str() );
	return CSS::uno::makeAny(s2);
}

SEQSEQ( ANY ) getArray(obj_ptr object, STRING handle) {
    SEQSEQ( ANY ) rows(1);
	Properties properties = object->getProperties();
	SEQ( ANY ) row(properties.size() + 1);
	row[0] = CSS::uno::makeAny(handle);
    for (int i = 0; i < properties.size(); i++) {
		ObjectProperty property = properties[i];
		any_ptr a = property();
        row[i + 1] = anyToANY(a);
    }
	rows[0] = row;
    return rows;
}

string OUStringToString(const STRING& s1) {
	::rtl::OString s2;
	if (s1.convertToString(&s2, 
	RTL_TEXTENCODING_ASCII_US, OUSTRING_TO_OSTRING_CVTFLAGS))
		return s2.getStr();
	else
		QL_FAIL("OUStringToString: unable to convert string");
}
