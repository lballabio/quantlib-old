
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

STRING SAL_CALL QLAddin::qlVer() THROWDEF_RTE_IAE {
    try {
        std::string ret =  QL_VER();
        return STRFROMANSI(ret.c_str());
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_VER: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlOhVer() THROWDEF_RTE_IAE {
    try {
        std::string ret =  QL_OH_VER();
        return STRFROMANSI(ret.c_str());
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_OH_VER: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(ANY) SAL_CALL QLAddin::qlFieldNames(
        const STRING& handleObject) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_QUERY(OUStringToString(handleObject));
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
        OH_LOGMESSAGE(std::string("ERROR: QL_FIELDNAMES: ") + e.what(), 2);
        THROW_RTE;
    }
}

// convert boost::any to Calc SEQSEQ(ANY)
SEQSEQ(ANY) anyToSEQANY(const any_ptr &a) {
    if (a->type() == typeid(int)) {
        int i1 = boost::any_cast<int>(*a);
        sal_Int32 i2 = static_cast< sal_Int32 >(i1);
        SEQSEQ( ANY ) rows(1);
        SEQ( ANY ) row(1);
        row[0] = CSS::uno::makeAny(i2);
        rows[0] = row;
        return rows;
    } else if (a->type() == typeid(double)) {
        double d = boost::any_cast<double>(*a);
        SEQSEQ( ANY ) rows(1);
        SEQ( ANY ) row(1);
        row[0] = CSS::uno::makeAny(d);
        rows[0] = row;
        return rows;
    } else if (a->type() == typeid(std::string)) {
        std::string s1 = boost::any_cast<std::string>(*a);
        STRING s2 = STRFROMASCII( s1.c_str() );
        SEQSEQ( ANY ) rows(1);
        SEQ( ANY ) row(1);
        row[0] = CSS::uno::makeAny(s2);
        rows[0] = row;
        return rows;
    } else if (a->type() == typeid(std::vector<long>)) {
        std::vector<long> v= boost::any_cast< std::vector<long> >(*a);
        SEQSEQ( ANY ) rows(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) row(1);
            row[0] = CSS::uno::makeAny(v[i]);
            rows[i] = row;
        }
        return rows;
    } else if (a->type() == typeid(std::vector<double>)) {
        std::vector<double> v= boost::any_cast< std::vector<double> >(*a);
        SEQSEQ( ANY ) rows(v.size());
        for (unsigned int i=0; i<v.size(); i++) {
            SEQ( ANY ) row(1);
            row[0] = CSS::uno::makeAny(v[i]);
            rows[i] = row;
        }
        return rows;
    } else
        throw Exception("anyToSEQANY: unable to interpret value");
}

SEQSEQ(ANY) SAL_CALL QLAddin::qlValue(
        const STRING& handleObject,
        const STRING& fieldName) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_QUERY(OUStringToString(handleObject));
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            STRING propertyName = STRFROMANSI(property.name().c_str());
            if (fieldName.equalsIgnoreAsciiCase(propertyName))
                return anyToSEQANY(a);
        }
        throw Exception(std::string("no field with name ") + OUStringToString(fieldName));
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_VALUE: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlLogfile(
        const STRING& logFileName,
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        int lvl = logLevel ? logLevel : 4;
        OH_LOGFILE(OUStringToString(logFileName), lvl);
        return logFileName;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_LOGFILE: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlLogMessage(
        const STRING& logMessage,
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        int lvl = logLevel ? logLevel : 4;
        OH_LOGMESSAGE(OUStringToString(logMessage), lvl);
        return logMessage;
    } catch (...) {
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::qlLogLevel(
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        OH_LOGLEVEL(logLevel);
        return logLevel;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_LOGLEVEL: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Bool SAL_CALL QLAddin::qlLogObject(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        OH_LOG_OBJECT(OUStringToString(handleObject));
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_LOG_OBJECT: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Bool SAL_CALL QLAddin::qlLogAllObjects() THROWDEF_RTE_IAE {
    try {
        OH_LOG_ALL_OBJECTS();
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_LOG_ALL_OBJECTS: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Bool SAL_CALL QLAddin::qlObjectDelete(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        OH_OBJECT_DELETE(OUStringToString(handleObject));
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_OBJECT_DELETE: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Bool SAL_CALL QLAddin::qlObjectDeleteAll() THROWDEF_RTE_IAE {
    try {
        OH_OBJECT_DELETE_ALL();
        return sal_False;
    } catch (const std::exception &e) {
        OH_LOGMESSAGE(std::string("ERROR: QL_OBJECT_DELETE_ALL: ") + e.what(), 2);
        THROW_RTE;
    }
}

