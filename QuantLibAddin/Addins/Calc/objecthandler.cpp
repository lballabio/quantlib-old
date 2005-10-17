
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

sal_Int32 SAL_CALL QLAddin::ohDependsOn(
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
        return sal_True;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohDependsOn: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohDeleteAllObjects() THROWDEF_RTE_IAE {
    try {
        ObjHandler::ObjectHandler::instance().deleteAllObjects();
        return sal_True;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohDeleteAllObjects: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohDeleteObject(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        ObjHandler::ObjectHandler::instance().deleteObject(ouStringToStlString(handleObject));
        return sal_True;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohDeleteObject: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(ANY) SAL_CALL QLAddin::ohFieldNames(
        const STRING& handleObject) THROWDEF_RTE_IAE {
    try {
        ObjHandler::obj_ptr objectPointer = ObjHandler::retrieveObject(ouStringToStlString(handleObject));
        if (!objectPointer)
            throw ObjHandler::Exception(std::string("no object with handle ") + ouStringToStlString(handleObject));
        ObjHandler::Properties properties = objectPointer->getProperties();
        SEQSEQ( ANY ) rows(properties.size());
        for (unsigned int i=0; i<properties.size(); i++) {
            SEQ( ANY ) row(1);
            ObjHandler::ObjectProperty property = properties[i];
            ObjHandler::any_ptr a = property();
            row[0] = stlStringToCalcAny(property.name());
            rows[i] = row;
        }
        return rows;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohFieldNames: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohLogAllObjects() THROWDEF_RTE_IAE {
    try {
        ObjHandler::logAllObjects();
        return sal_True;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohLogAllObjects: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::ohLogMessage(
        const STRING &logMessage,
        const ANY &logLevel) THROWDEF_RTE_IAE {
    try {
        long logLevelScalar = anyToScalarLong(logLevel, 4);
        ObjHandler::logMessage(ouStringToStlString(logMessage), logLevelScalar);
        return logMessage;
    } catch (...) {
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohLogObject(
        const STRING & handleObject) THROWDEF_RTE_IAE {
    try {
        ObjHandler::logObject(ouStringToStlString(handleObject));
        return sal_True;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohLogObject: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::ohSetLogFile(
        const STRING &logFileName,
        const ANY &logLevel) THROWDEF_RTE_IAE {
    try {
        long logLevelScalar = anyToScalarLong(logLevel, 4);
        ObjHandler::setLogFile(ouStringToStlString(logFileName), logLevelScalar);
        return logFileName;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohSetLogfile: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohSetLogLevel(
        sal_Int32 logLevel) THROWDEF_RTE_IAE {
    try {
        ObjHandler::setLogLevel(logLevel);
        return logLevel;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohSetLogLevel: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::ohVersion() THROWDEF_RTE_IAE {
    try {
        return STRFROMANSI(OBJHANDLER_VERSION);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohVersion: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(ANY) SAL_CALL QLAddin::ohFieldValue(
        const STRING& handleObject,
        const STRING& fieldName,
        const ANY &trigger) THROWDEF_RTE_IAE {
    try {
        ObjHandler::obj_ptr objectPointer = ObjHandler::retrieveObject(ouStringToStlString(handleObject));
        if (!objectPointer)
            throw ObjHandler::Exception(std::string("no object with handle ") + ouStringToStlString(handleObject));
        ObjHandler::Properties properties = objectPointer->getProperties();
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjHandler::ObjectProperty property = properties[i];
            ObjHandler::any_ptr a = property();
            STRING propertyName = STRFROMANSI(property.name().c_str());
            if (fieldName.equalsIgnoreAsciiCase(propertyName))
                return boostAnyToSeqSeq(a);
        }
        throw ObjHandler::Exception(std::string("no field with name ") + ouStringToStlString(fieldName));
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohFieldValue: ") + e.what(), 2);
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL QLAddin::ohObjectCount() THROWDEF_RTE_IAE {
    try {
        return ObjHandler::ObjectHandler::instance().objectCount();
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohObjectCount: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ(STRING) SAL_CALL QLAddin::ohHandleList() THROWDEF_RTE_IAE {
    try {
        std::vector < std::string > handleList =
            ObjHandler::ObjectHandler::instance().handleList();
        return vectorStringToSeqSeq(handleList);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohHandleList: ") + e.what(), 2);
        THROW_RTE;
    }
}

