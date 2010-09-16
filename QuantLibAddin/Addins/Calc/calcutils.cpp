
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/objecthandler.hpp>
#include <qldefs.hpp>
#include <calcutils.hpp>
#include <sstream>

std::string ouStringToStlString(const STRING& s1) {
    ::rtl::OString s2;
    if (s1.convertToString(&s2, RTL_TEXTENCODING_ASCII_US, 
        OUSTRING_TO_OSTRING_CVTFLAGS))
        return s2.getStr();
    else
        throw ObjectHandler::Exception("ouStringToStlString: unable to convert string");
}

ANY stlStringToCalcAny(const std::string &s) {
    STRING s2 = STRFROMASCII( s.c_str() );
    return CSS::uno::makeAny(s2);
}

/*
SEQSEQ(ANY) boostAnyToSeqSeq(const ObjectHandler::any_ptr &a) {
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
        scalarToCalc(s[0], a2);
        ss[0] = s;
        return ss;
    } else if (a->type() == typeid(std::vector< long >)) {
        std::vector< long > v= boost::any_cast< std::vector< long > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< double >)) {
        std::vector< double > v= boost::any_cast< std::vector< double > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< bool >)) {
        std::vector< bool > v= boost::any_cast< std::vector< bool > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            sal_Int32 b = static_cast< sal_Int32 >(v[i]);
            s[0] = CSS::uno::makeAny(b);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector<std::string>)) {
        std::vector<std::string> v= boost::any_cast< std::vector<std::string> >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            s[0] = CSS::uno::makeAny(STRFROMASCII(v[i].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< boost::any >)) {
        std::vector< boost::any > v= boost::any_cast< std::vector< boost::any > >(*a);
        SEQSEQ( ANY ) ss(v.size());
        for (unsigned int i=0; i<v.size(); ++i) {
            SEQ( ANY ) s(1);
            scalarToCalc(s[0], v[i]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< long > >)) {
        std::vector< std::vector< long > > vv= boost::any_cast< std::vector< std::vector< long > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< long > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< double > >)) {
        std::vector< std::vector< double > > vv= boost::any_cast< std::vector< std::vector< double > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< double > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(v[j]);
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< bool > >)) {
        std::vector< std::vector< bool > > vv= boost::any_cast< std::vector< std::vector< bool > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< bool > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j) {
                sal_Int32 b = static_cast< sal_Int32 >(v[j]);
                s[j] = CSS::uno::makeAny(b);
            }
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< std::string > >)) {
        std::vector< std::vector< std::string > > vv= boost::any_cast< std::vector< std::vector< std::string > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< std::string > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                s[j] = CSS::uno::makeAny(STRFROMASCII(v[j].c_str()));
            ss[i] = s;
        }
        return ss;
    } else if (a->type() == typeid(std::vector< std::vector< boost::any > >)) {
        std::vector< std::vector< boost::any > > vv= boost::any_cast< std::vector< std::vector< boost::any > > >(*a);
        SEQSEQ( ANY ) ss(vv.size());
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector< boost::any > v = vv[i];
            SEQ( ANY ) s(v.size());
            for (unsigned int j=0; j<v.size(); ++j)
                scalarToCalc(s[j], v[j]);
            ss[i] = s;
        }
        return ss;
    } else
        throw ObjectHandler::Exception("boostAnyToSeqSeq: unable to interpret value");
}
*/

// conversions from native C++ datatypes to Calc datatypes

void scalarToCalc(sal_Int32 &ret, const bool &value) {
    ret = value;
}

void scalarToCalc(STRING &ret, const std::string &value) {
    ret = STRFROMANSI(value.c_str());
}

void scalarToCalc(ANY &ret, const boost::any &value) {
    if (value.type() == typeid(int)) {
        int temp1 = boost::any_cast<int>(value);
        sal_Int32 temp2 = static_cast< sal_Int32 >(temp1);
        ret = CSS::uno::makeAny(temp2);
    } else if (value.type() == typeid(long)) {
        long temp = boost::any_cast< long >(value);
        ret = CSS::uno::makeAny(temp);
    } else if (value.type() == typeid(double)) {
        double temp = boost::any_cast<double>(value);
        ret = CSS::uno::makeAny(temp);
    } else if (value.type() == typeid(bool)) {
        bool temp = boost::any_cast< bool >(value);
        sal_Int32 b2 = static_cast< sal_Int32 >(temp);
        ret = CSS::uno::makeAny(b2);
    } else if (value.type() == typeid(std::string)) {
        std::string temp = boost::any_cast<std::string>(value);
        ret = stlStringToCalcAny(temp);
    } else if (value.type() == typeid(boost::any)) {
//        boost::any a2 = boost::any_cast< boost::any >(value);
//        return boostAnyToCalcAny(a2);
//        ret = CSS::uno::makeAny(STRFROMASCII("unknown type"));
        throw ObjectHandler::Exception("scalarToCalc: unable to interpret value");
    } else if (value.type() == typeid(std::vector< int >)
           ||  value.type() == typeid(std::vector< long >)
           ||  value.type() == typeid(std::vector< double >)
           ||  value.type() == typeid(std::vector< bool >)
           ||  value.type() == typeid(std::vector< std::string >)
           ||  value.type() == typeid(std::vector< boost::any >)) {
        ret = CSS::uno::makeAny(STRFROMASCII("<VECTOR>"));
    } else if (value.type() == typeid(std::vector< std::vector< int > >)
           ||  value.type() == typeid(std::vector< std::vector< long > >)
           ||  value.type() == typeid(std::vector< std::vector< double > >)
           ||  value.type() == typeid(std::vector< std::vector< bool > >)
           ||  value.type() == typeid(std::vector< std::vector< std::string > >)
           ||  value.type() == typeid(std::vector< std::vector< boost::any > >)) {
        ret = CSS::uno::makeAny(STRFROMASCII("<MATRIX>"));
    } else
//        ret = CSS::uno::makeAny(STRFROMASCII("unknown type"));
        throw ObjectHandler::Exception("scalarToCalc: unable to interpret value");
}

// conversions from Calc datatypes to native C++ datatypes

void calcToScalar(bool &ret, const sal_Int32 &value) {
    ret = value != 0;
}

void calcToScalar(QuantLib::Natural &ret, sal_Int32 &value) {
  long temp;
  value >>= temp;
  ret = temp;
}

// void calcToScalar(QuantLib::Integer &ret, sal_Int32 &value) {
//   long temp;
//   value >>= temp;
//   ret = temp;
// }

void calcToScalar(int &ret, sal_Int32 &value) {
  long temp;
  value >>= temp;
  ret = temp;
}

void calcToScalar(QuantLib::Size &ret, sal_Int32 &value) {
  long temp;
  value >>= temp;
  ret = temp;
}

void calcToScalar(long &ret, const ANY &value, const long &defaultValue) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < long > (temp);
    } else
        ret = defaultValue;
}

void calcToScalar(QuantLib::Natural &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Natural > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}


void calcToScalar(QuantLib::Integer &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Integer > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}

void calcToScalar(QuantLib::Size &ret, const ANY &value) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = static_cast < QuantLib::Size > (temp);
    } else if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(typeName);
        throw ObjectHandler::Exception(msg.str());
    }
}

void calcToScalar(double &ret, const ANY &value, const double &defaultValue) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = temp;
    } else
        ret = defaultValue;
}

void calcToScalar(bool &ret, const ANY &value, const bool &defaultValue) {
    STRING typeName = value.getValueTypeName();
    if (typeName.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = (temp != 0);
    } else
        ret = defaultValue;
}

void calcToScalar(std::string &ret, const ANY &value, const std::string &defaultValue) {
    STRING t = value.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID")))
        ret = defaultValue;
    else {
        STRING temp;
        value >>= temp;
        ret = ouStringToStlString(temp);
    }
}

void calcToScalar(boost::any &ret, const ANY &value) {
    STRING t = value.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("VOID"))) {
        ret = boost::any();
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("LONG"))) {
        long temp;
        value >>= temp;
        ret = temp;
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("DOUBLE"))) {
        double temp;
        value >>= temp;
        ret = temp;
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("STRING"))) {
        STRING temp;
        value >>= temp;
        ret = ouStringToStlString(temp);
    } else if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        ret = std::string("<MATRIX>");
    } else {
        std::ostringstream msg;
        msg << "unrecognized type: " << ouStringToStlString(t);
        ret = msg.str();
    }
}

