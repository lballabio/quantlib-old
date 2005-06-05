
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

ANY boostAnyToCalcAny(const boost::any &a) {
    if (a.type() == typeid(int)) {
        int i1 = boost::any_cast<int>(a);
        sal_Int32 i2 = static_cast< sal_Int32 >(i1);
        return CSS::uno::makeAny(i2);
    } else if (a.type() == typeid(long)) {
        long l = boost::any_cast< long >(a);
//        sal_Int32 l2 = static_cast< sal_Int32 >(l);
        return CSS::uno::makeAny(l);
    } else if (a.type() == typeid(double)) {
        double d = boost::any_cast<double>(a);
        return CSS::uno::makeAny(d);
    } else if (a.type() == typeid(bool)) {
        bool b = boost::any_cast< bool >(a);
        sal_Int32 b2 = static_cast< sal_Int32 >(b);
        return CSS::uno::makeAny(b2);
    } else if (a.type() == typeid(std::string)) {
        std::string s = boost::any_cast<std::string>(a);
        return stringToANY(s);
    } else if (a.type() == typeid(boost::any)) {
//        boost::any a2 = boost::any_cast< boost::any >(a);
//        return boostAnyToCalcAny(a2);
        return CSS::uno::makeAny(STRFROMASCII("boost::any"));
    } else if (a.type() == typeid(std::vector< int >)
           ||  a.type() == typeid(std::vector< long >)
           ||  a.type() == typeid(std::vector< double >)
           ||  a.type() == typeid(std::vector< bool >)
           ||  a.type() == typeid(std::vector< std::string >)
           ||  a.type() == typeid(std::vector< boost::any >)) {
        return CSS::uno::makeAny(STRFROMASCII("<VECTOR>"));
    } else if (a.type() == typeid(std::vector< std::vector< int > >)
           ||  a.type() == typeid(std::vector< std::vector< long > >)
           ||  a.type() == typeid(std::vector< std::vector< double > >)
           ||  a.type() == typeid(std::vector< std::vector< bool > >)
           ||  a.type() == typeid(std::vector< std::vector< std::string > >)
           ||  a.type() == typeid(std::vector< std::vector< boost::any > >)) {
        return CSS::uno::makeAny(STRFROMASCII("<MATRIX>"));
    } else
        return CSS::uno::makeAny(STRFROMASCII("unknown type"));
//        throw Exception("boostAnyToCalcAny: unable to interpret value");
}

boost::any calcAnyToBoostAny(const ANY &a) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long l;
        a >>= l;
        return boost::any(l);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double d;
        a >>= d;
        return boost::any(d);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
        STRING s;
        a >>= s;
        return boost::any(OUStringToStlString(s));
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << OUStringToStlString(t);
        return (boost::any(msg.str()));
    }
}

ANY stringToANY(const std::string &s) {
    STRING s2 = STRFROMASCII( s.c_str() );
    return CSS::uno::makeAny(s2);
}

SEQSEQ( ANY ) propertyVectorToSeqSeq(Properties properties, const STRING &handle) {
    SEQSEQ( ANY ) rows(1);
    SEQ( ANY ) row(properties.size() + 1);
    row[0] = CSS::uno::makeAny(handle);
    for (unsigned int i=0; i<properties.size(); i++) {
        ObjectProperty property = properties[i];
        any_ptr a = property();
        row[i + 1] = boostAnyToCalcAny(*a);
    }
    rows[0] = row;
    return rows;
}

std::string OUStringToStlString(const STRING& s1) {
    ::rtl::OString s2;
    if (s1.convertToString(&s2, RTL_TEXTENCODING_ASCII_US, 
        OUSTRING_TO_OSTRING_CVTFLAGS))
        return s2.getStr();
    else
        throw Exception("OUStringToStlString: unable to convert string");
}

STRING stlStringToOuString(const std::string &s) {
    return STRFROMANSI(s.c_str());
}

std::string calcAnyToStlString(const ANY& s) {
    STRING str;
    s >>= str;
    return OUStringToStlString(str);
}

std::vector < long >SeqSeqToVectorLong(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < long >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < double >SeqSeqToVectorDouble(const SEQSEQ( double )& ss) {
    std::vector < double >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < bool >SeqSeqToVectorBool(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < bool >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < std::string >SeqSeqToVectorString(const SEQSEQ( ANY )& ss) {
    std::vector < std::string >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(calcAnyToStlString(ss[i][j]));
    return v;
}

std::vector < boost::any >SeqSeqToVectorAny(const SEQSEQ( ANY )& ss) {
    std::vector < boost::any >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(calcAnyToBoostAny(ss[i][j]));
    return v;
}

std::vector < std::vector < long > >SeqSeqToMatrixLong(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < std::vector < long > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < long >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < double > >SeqSeqToMatrixDouble(const SEQSEQ( double )& ss) {
    std::vector < std::vector < double > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < double >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < bool > >SeqSeqToMatrixBool(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < std::vector < bool > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < bool >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < std::string > >SeqSeqToMatrixString(const SEQSEQ( ANY )& ss) {
    std::vector < std::vector < std::string > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < std::string >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(calcAnyToStlString(ss[i][j]));
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < boost::any > >SeqSeqToMatrixAny(const SEQSEQ( ANY )& ss) {
    std::vector < std::vector < boost::any > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < boost::any >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(calcAnyToBoostAny(ss[i][j]));
        vv.push_back(v);
    }
    return vv;
}

SEQSEQ( sal_Int32 ) VectorLongToSeqSeq(const std::vector < long > &v) {
    SEQSEQ( sal_Int32 ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( sal_Int32 ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( double ) VectorDoubleToSeqSeq(const std::vector < double > &v) {
    SEQSEQ( double ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( double ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( sal_Int32 ) VectorBoolToSeqSeq(const std::vector < bool > &v) {
    SEQSEQ( sal_Int32 ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( sal_Int32 ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( STRING ) VectorStringToSeqSeq(const std::vector < std::string > &v) {
    SEQSEQ( STRING ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( STRING ) s(1);
        s[0] = stlStringToOuString(v[i]);
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( ANY ) VectorAnyToSeqSeq(const std::vector < boost::any > &v) {
    SEQSEQ( ANY ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( ANY ) s(1);
        s[0] = boostAnyToCalcAny(v[i]);
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( sal_Int32 ) MatrixLongToSeqSeq(const std::vector < std::vector < long > >&vv) {
    SEQSEQ( sal_Int32 ) ss(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < long > v = vv[i];
        SEQ( sal_Int32 ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            s[j] = v[j];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( double ) MatrixDoubleToSeqSeq(const std::vector < std::vector < double > >&vv) {
    SEQSEQ( double ) ss(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < double > v = vv[i];
        SEQ( double ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            s[j] = v[j];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( sal_Int32 ) MatrixBoolToSeqSeq(const std::vector < std::vector < bool > >&vv) {
    SEQSEQ( sal_Int32 ) ss(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < bool > v = vv[i];
        SEQ( sal_Int32 ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            s[j] = v[j];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( STRING ) MatrixStringToSeqSeq(const std::vector < std::vector < std::string > >&vv) {
    SEQSEQ( STRING ) ss(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < std::string > v = vv[i];
        SEQ( STRING ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            s[j] = stlStringToOuString(v[j]);
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( ANY ) MatrixAnyToSeqSeq(const std::vector < std::vector < boost::any > >&vv) {
    SEQSEQ( ANY ) ss(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < boost::any > v = vv[i];
        SEQ( ANY ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            s[j] = boostAnyToCalcAny(v[j]);
        ss[i] = s;
    }
    return ss;
}

