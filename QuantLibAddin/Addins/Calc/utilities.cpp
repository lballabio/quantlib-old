
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

SEQSEQ( STRING ) SAL_CALL QLAddin::qlListEnum(
        const STRING &enumId) THROWDEF_RTE_IAE {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getEnumMembers(
            ouStringToStlString(enumId));
        return vectorStringToSeqSeq(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: QL_LIST_ENUM: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ( STRING ) SAL_CALL QLAddin::qlListRegisteredEnums() THROWDEF_RTE_IAE {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getRegisteredEnums();
        return vectorStringToSeqSeq(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: QL_LIST_REGISTERED_ENUMS: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ( STRING ) SAL_CALL QLAddin::qlListRegisteredTypes() THROWDEF_RTE_IAE {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getRegisteredComplexTypes();
        return vectorStringToSeqSeq(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: QL_LIST_REGISTERED_TYPES: ") + e.what(), 2);
        THROW_RTE;
    }
}

SEQSEQ( STRING ) SAL_CALL QLAddin::qlListType(
        const STRING &enumId) THROWDEF_RTE_IAE {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getComplexTypeMembers(
            ouStringToStlString(enumId));
        return vectorStringToSeqSeq(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: QL_LIST_TYPE: ") + e.what(), 2);
        THROW_RTE;
    }
}

STRING SAL_CALL QLAddin::qlVersion() THROWDEF_RTE_IAE {
    try {
        return STRFROMANSI(QuantLibAddin::qlVersion().c_str());
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: QL_VERSION: ") + e.what(), 2);
        THROW_RTE;
    }
}

