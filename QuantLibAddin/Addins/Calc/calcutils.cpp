
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#include <qla/qladdin.hpp>
#include <Addins/Calc/qladdin.hpp>
#include <Addins/Calc/calcutils.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

// convert boost::any to Calc Any
ANY anyToANY(const any_ptr &a) {
    if (a->type() == typeid(int)) {
        int i1 = boost::any_cast<int>(*a);
        sal_Int32 i2 = static_cast< sal_Int32 >(i1);
        return CSS::uno::makeAny(i2);
    } else if (a->type() == typeid(double)) {
        double d = boost::any_cast<double>(*a);
        return CSS::uno::makeAny(d);
    } else if (a->type() == typeid(std::string)) {
        std::string s1 = boost::any_cast<std::string>(*a);
        STRING s2 = STRFROMASCII( s1.c_str() );
        return CSS::uno::makeAny(s2);
    } else if (a->type() == typeid(std::vector<long>)) {
        return CSS::uno::makeAny(STRFROMASCII("<VECTOR>"));
    } else if (a->type() == typeid(std::vector<double>)) {
        return CSS::uno::makeAny(STRFROMASCII("<VECTOR>"));
    } else
        throw Exception("anyToANY: unable to interpret value");
}

ANY stringToANY(const std::string &s) {
    STRING s2 = STRFROMASCII( s.c_str() );
    return CSS::uno::makeAny(s2);
}

SEQSEQ( ANY ) getArray(Properties properties, STRING handle) {
    SEQSEQ( ANY ) rows(2);
    SEQ( ANY ) row0(properties.size() + 1);
    SEQ( ANY ) row1(properties.size() + 1);
    row0[0] = CSS::uno::makeAny(STRFROMASCII("HANDLE"));
    row1[0] = CSS::uno::makeAny(handle);
    for (unsigned int i=0; i<properties.size(); i++) {
        ObjectProperty property = properties[i];
        row0[i + 1] = CSS::uno::makeAny(STRFROMASCII(property.name().c_str()));
        any_ptr a = property();
        row1[i + 1] = anyToANY(a);
    }
    rows[0] = row0;
    rows[1] = row1;
    return rows;
}

std::string OUStringToString(const STRING& s1) {
    ::rtl::OString s2;
    if (s1.convertToString(&s2, RTL_TEXTENCODING_ASCII_US, 
        OUSTRING_TO_OSTRING_CVTFLAGS))
        return s2.getStr();
    else
        throw Exception("OUStringToString: unable to convert string");
}

