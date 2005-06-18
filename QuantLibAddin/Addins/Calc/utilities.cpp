
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

STRING SAL_CALL QLAddin::qlVersion() THROWDEF_RTE_IAE {
    try {
        std::string ret =  QL_VERSION();
        return STRFROMANSI(ret.c_str());
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_VERSION: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlOhVersion() THROWDEF_RTE_IAE {
    try {
        std::string ret =  QL_OH_VERSION();
        return STRFROMANSI(ret.c_str());
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_OH_VERSION: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(ANY) SAL_CALL QLAddin::qlFieldNames(
        const STRING& handleObject) THROWDEF_RTE_IAE {
    try {
        Properties properties = OH_QUERY_OBJECT(OUStringToStlString(handleObject));
        SEQSEQ( ANY ) rows(properties.size());
        for (unsigned int i=0; i<properties.size(); i++) {
            SEQ( ANY ) row(1);
            ObjectProperty property = properties[i];
            any_ptr a = property();
            row[0] = stringToANY(property.name());
            rows[i] = row;
        }
        return rows;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_FIELD_NAMES: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(ANY) boostAnyToSeqSeq(const any_ptr &a) {
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
        throw Exception("boostAnyToSeqSeq: unable to interpret value");
}

SEQSEQ(ANY) SAL_CALL QLAddin::qlValue(
        const STRING& handleObject,
        const STRING& fieldName) THROWDEF_RTE_IAE {
    try {
        Properties properties = OH_QUERY_OBJECT(OUStringToStlString(handleObject));
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            STRING propertyName = STRFROMANSI(property.name().c_str());
            if (fieldName.equalsIgnoreAsciiCase(propertyName))
                return boostAnyToSeqSeq(a);
        }
        throw Exception(std::string("no field with name ") + OUStringToStlString(fieldName));
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_VALUE: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlLogfile(
        const STRING& logFileName,
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        int lvl = logLevel ? logLevel : 4;
        OH_LOGFILE(OUStringToStlString(logFileName), lvl);
        return logFileName;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_LOGFILE: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlLogMessage(
        const STRING& logMessage,
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        int lvl = logLevel ? logLevel : 4;
        OH_LOG_MESSAGE(OUStringToStlString(logMessage), lvl);
        return logMessage;
    } catch (...) {
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlLogLevel(
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        OH_LOG_LEVEL(logLevel);
        return logLevel;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_LOGLEVEL: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlLogObject(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        OH_LOG_OBJECT(OUStringToStlString(handleObject));
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_LOG_OBJECT: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlLogAllObjects() THROWDEF_RTE_IAE {
    try {
        OH_LOG_ALL_OBJECTS();
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_LOG_ALL_OBJECTS: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlDeleteObject(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        OH_DELETE_OBJECT(OUStringToStlString(handleObject));
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_DELETE_OBJECT: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlDeleteAllObjects() THROWDEF_RTE_IAE {
    try {
        OH_DELETE_ALL_OBJECTS();
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_DELETE_ALL_OBJECTS: ") + e.what(), 2);
        THROW_RTE;
    }
}


sal_Int32 SAL_CALL QLAddin::qlDependsOn(
        const ANY &dummy0,
        const ANY &dummy1,
        const ANY &dummy2,
        const ANY &dummy3,
        const ANY &dummy4,
        const ANY &dummy5,
        const ANY &dummy6,
        const ANY &dummy7,
        const ANY &dummy8,
        const ANY &dummy9) THROWDEF_RTE_IAE {
    try {
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE(std::string("ERROR: QL_DEPENDS_ON: ") + e.what(), 2);
        THROW_RTE;
    }
}
