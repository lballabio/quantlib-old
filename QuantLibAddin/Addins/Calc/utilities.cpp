#include "qladdin.hpp"
#include "ObjectClassLibrary/objectoption.hpp"
#include "utilities.hpp"

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

SEQSEQ( ANY ) getArray(obj_ptr object, STRING handle) {
	vector < string > fieldNames = object->getFieldNames();
    SEQSEQ( ANY ) rows(1);
	SEQ( ANY ) row(fieldNames.size() + 1);
	row[0] = CSS::uno::makeAny(handle);
    for (int i = 0; i < fieldNames.size(); i++) {
		string fieldName = fieldNames[i];
		any_ptr a = object->getValue(fieldName);
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
