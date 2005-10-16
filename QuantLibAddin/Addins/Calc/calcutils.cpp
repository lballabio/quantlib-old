
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

#include <oh/objhandler.hpp>
#include <Addins/Calc/qldefs.hpp>
#include <Addins/Calc/calcutils.hpp>
#include <sstream>

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
        return stlStringToCalcAny(s);
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
//        throw ObjHandler::Exception("boostAnyToCalcAny: unable to interpret value");
}

SEQSEQ( ANY ) propertyVectorToSeqSeq(ObjHandler::Properties properties, const STRING &handle) {
    SEQSEQ( ANY ) rows(1);
    SEQ( ANY ) row(properties.size() + 1);
    row[0] = CSS::uno::makeAny(handle);
    for (unsigned int i=0; i<properties.size(); i++) {
        ObjHandler::ObjectProperty property = properties[i];
        ObjHandler::any_ptr a = property();
        row[i + 1] = boostAnyToCalcAny(*a);
    }
    rows[0] = row;
    return rows;
}

std::string ouStringToStlString(const STRING& s1) {
    ::rtl::OString s2;
    if (s1.convertToString(&s2, RTL_TEXTENCODING_ASCII_US, 
        OUSTRING_TO_OSTRING_CVTFLAGS))
        return s2.getStr();
    else
        throw ObjHandler::Exception("ouStringToStlString: unable to convert string");
}

STRING stlStringToOuString(const std::string &s) {
    return STRFROMANSI(s.c_str());
}

ANY stlStringToCalcAny(const std::string &s) {
    STRING s2 = STRFROMASCII( s.c_str() );
    return CSS::uno::makeAny(s2);
}

std::vector < long > seqSeqToVectorLong(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < long >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < double > seqSeqToVectorDouble(const SEQSEQ( double )& ss) {
    std::vector < double >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < bool > seqSeqToVectorBool(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < bool >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
    return v;
}

std::vector < std::string > seqSeqToVectorString(const SEQSEQ( ANY )& ss) {
    std::vector < std::string >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(anyToScalarString(ss[i][j]));
    return v;
}

std::vector < boost::any > seqSeqToVectorAny(const SEQSEQ( ANY )& ss) {
    std::vector < boost::any >v;
    for (int i=0; i<ss.getLength(); i++)
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(anyToScalarAny(ss[i][j]));
    return v;
}

std::vector < std::vector < long > > seqSeqToMatrixLong(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < std::vector < long > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < long >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < double > > seqSeqToMatrixDouble(const SEQSEQ( double )& ss) {
    std::vector < std::vector < double > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < double >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < bool > > seqSeqToMatrixBool(const SEQSEQ( sal_Int32 )& ss) {
    std::vector < std::vector < bool > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < bool >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(ss[i][j]);
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < std::string > > seqSeqToMatrixString(const SEQSEQ( ANY )& ss) {
    std::vector < std::vector < std::string > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < std::string >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(anyToScalarString(ss[i][j]));
        vv.push_back(v);
    }
    return vv;
}

std::vector < std::vector < boost::any > > seqSeqToMatrixAny(const SEQSEQ( ANY )& ss) {
    std::vector < std::vector < boost::any > >vv;
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < boost::any >v;
        for (int j=0; j<ss[i].getLength(); j++)
            v.push_back(anyToScalarAny(ss[i][j]));
        vv.push_back(v);
    }
    return vv;
}

SEQSEQ( sal_Int32 ) vectorLongToSeqSeq(const std::vector < long > &v) {
    SEQSEQ( sal_Int32 ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( sal_Int32 ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( double ) vectorDoubleToSeqSeq(const std::vector < double > &v) {
    SEQSEQ( double ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( double ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( sal_Int32 ) vectorBoolToSeqSeq(const std::vector < bool > &v) {
    SEQSEQ( sal_Int32 ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( sal_Int32 ) s(1);
        s[0] = v[i];
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( STRING ) vectorStringToSeqSeq(const std::vector < std::string > &v) {
    SEQSEQ( STRING ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( STRING ) s(1);
        s[0] = stlStringToOuString(v[i]);
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( ANY ) vectorAnyToSeqSeq(const std::vector < boost::any > &v) {
    SEQSEQ( ANY ) ss(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( ANY ) s(1);
        s[0] = boostAnyToCalcAny(v[i]);
        ss[i] = s;
    }
    return ss;
}

SEQSEQ( sal_Int32 ) matrixLongToSeqSeq(const std::vector < std::vector < long > >&vv) {
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

SEQSEQ( double ) matrixDoubleToSeqSeq(const std::vector < std::vector < double > >&vv) {
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

SEQSEQ( sal_Int32 ) matrixBoolToSeqSeq(const std::vector < std::vector < bool > >&vv) {
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

SEQSEQ( STRING ) matrixStringToSeqSeq(const std::vector < std::vector < std::string > >&vv) {
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

SEQSEQ( ANY ) matrixAnyToSeqSeq(const std::vector < std::vector < boost::any > >&vv) {
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

long anyToScalarLong(const ANY &a, const long &defaultValue) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double ret;
        a >>= ret;
        return static_cast < long > (ret);
    } else
        return defaultValue;
}

double anyToScalarDouble(const ANY &a, const double &defaultValue) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double ret;
        a >>= ret;
        return ret;
    } else
        return defaultValue;
}

bool anyToScalarBool(const ANY &a, const bool &defaultValue) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double ret;
        a >>= ret;
        return (ret != 0);
    } else
        return defaultValue;
}

std::string anyToScalarString(const ANY &a, const std::string &defaultValue) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID")))
        return defaultValue;
    else {
        STRING ret;
        a >>= ret;
        return ouStringToStlString(ret);
    }
}

boost::any anyToScalarAny(const ANY &a) {
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID"))) {
        return boost::any();
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
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
        return boost::any(ouStringToStlString(s));
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        return boost::any(std::string("<MATRIX>"));
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(t);
        return (boost::any(msg.str()));
    }
}

std::vector < long > anyToVectorLong(const ANY &a) {
    std::vector < long > ret;
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double d;
        a >>= d;
        ret.push_back(static_cast < long > (d));
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        a >>= ss;
        for (int i=0; i<ss.getLength(); i++)
            for (int j=0; j<ss[i].getLength(); j++)
                ret.push_back(anyToScalarLong(ss[i][j]));
    }
    return ret;
}

std::vector < double > anyToVectorDouble(const ANY &a) {
    std::vector < double > ret;
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double d;
        a >>= d;
        ret.push_back(d);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        a >>= ss;
        for (int i=0; i<ss.getLength(); i++)
            for (int j=0; j<ss[i].getLength(); j++)
                ret.push_back(anyToScalarDouble(ss[i][j]));
    }
    return ret;
}

std::vector < bool > anyToVectorBool(const ANY &a) {
    std::vector < bool > ret;
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double d;
        a >>= d;
        ret.push_back(d != 0);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        a >>= ss;
        for (int i=0; i<ss.getLength(); i++)
            for (int j=0; j<ss[i].getLength(); j++)
                ret.push_back(anyToScalarBool(ss[i][j]));
    }
    return ret;
}

std::vector < std::string > anyToVectorString(const ANY& a) {
    std::vector < std::string > ret;
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
        STRING s;
        a >>= s;
        ret.push_back(ouStringToStlString(s));
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        a >>= ss;
        for (int i=0; i<ss.getLength(); i++)
            for (int j=0; j<ss[i].getLength(); j++)
                ret.push_back(anyToScalarString(ss[i][j]));
    }
    return ret;
}

std::vector < boost::any > anyToVectorAny(const ANY &a) {
    std::vector < boost::any > ret;
    STRING t = a.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        a >>= ss;
        for (int i=0; i<ss.getLength(); i++)
            for (int j=0; j<ss[i].getLength(); j++)
                ret.push_back(anyToScalarAny(ss[i][j]));
    } else
        ret.push_back(anyToScalarAny(a));
    return ret;
}

std::vector < std::vector < long > > anyToMatrixLong(const ANY &a) {
    std::vector < std::vector < long > > ret;
    return ret;
}

std::vector < std::vector < double > > anyToMatrixDouble(const ANY &a) {
    std::vector < std::vector < double > > ret;
    return ret;
}

std::vector < std::vector < bool > > anyToMatrixBool(const ANY &a) {
    std::vector < std::vector < bool > > ret;
    return ret;
}

std::vector < std::vector < std::string > > anyToMatrixString(const ANY& s) {
    std::vector < std::vector < std::string > > ret;
    return ret;
}

std::vector < std::vector < boost::any > > anyToMatrixAny(const ANY &a) {
    std::vector < std::vector < boost::any > > ret;
    return ret;
}

SEQSEQ(ANY) boostAnyToSeqSeq(const ObjHandler::any_ptr &a) {
    if (a->type() == typeid(long)) {
        long l = boost::any_cast< long >(*a);
        sal_Int32 l2 = static_cast< sal_Int32 >(l);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(l2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(double)) {
        double d = boost::any_cast< double >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(d);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(bool)) {
        bool b = boost::any_cast< bool >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        sal_Int32 b2 = static_cast< sal_Int32 >(b);
        s[0] = CSS::uno::makeAny(b2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(std::string)) {
        std::string str = boost::any_cast<std::string>(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = CSS::uno::makeAny(STRFROMASCII(str.c_str()));
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid( boost::any )) {
        boost::any a2 = boost::any_cast< boost::any >(*a);
        SEQSEQ( ANY ) ss(1);
        SEQ( ANY ) s(1);
        s[0] = boostAnyToCalcAny(a2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(std::vector< long >)) {
        std::vector< long > v= boost::any_cast< std::vector< long > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< double >)) {
        std::vector< double > v= boost::any_cast< std::vector< double > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< bool >)) {
        std::vector< bool > v= boost::any_cast< std::vector< bool > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) s(1);
            sal_Int32 b = static_cast< sal_Int32 >(v[i]);
            s[0] = CSS::uno::makeAny(b);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector<std::string>)) {
        std::vector<std::string> v= boost::any_cast< std::vector<std::string> >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(STRFROMASCII(v[i].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< boost::any >)) {
        std::vector< boost::any > v= boost::any_cast< std::vector< boost::any > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) s(1);
            s[0] = boostAnyToCalcAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< long > >)) {
        std::vector< std::vector< long > > vv= boost::any_cast< std::vector< std::vector< long > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector< long > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); j++)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< double > >)) {
        std::vector< std::vector< double > > vv= boost::any_cast< std::vector< std::vector< double > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector< double > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); j++)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< bool > >)) {
        std::vector< std::vector< bool > > vv= boost::any_cast< std::vector< std::vector< bool > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector< bool > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); j++) {
                sal_Int32 b = static_cast< sal_Int32 >(v[j]);
                s[j] = CSS::uno::makeAny(b);
            }
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< std::string > >)) {
        std::vector< std::vector< std::string > > vv= boost::any_cast< std::vector< std::vector< std::string > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector< std::string > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); j++)
                s[j] = CSS::uno::makeAny(STRFROMASCII(v[j].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< boost::any > >)) {
        std::vector< std::vector< boost::any > > vv= boost::any_cast< std::vector< std::vector< boost::any > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector< boost::any > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); j++)
                s[j] = boostAnyToCalcAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else
        throw ObjHandler::Exception("boostAnyToSeqSeq: unable to interpret value");
}


