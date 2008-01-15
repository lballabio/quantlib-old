
/*
 Copyright (C) 2006, 2007, 2008 Eric Ehlers

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

#include <qlo/qladdindefines.hpp>
#include <qlo/Enumerations/Factories/all.hpp>
#include <qlo/Conversions/all.hpp>
#include <oh/objecthandler.hpp>

#include <Addins/Calc/qladdin.hpp>
#include <Addins/Calc/calcutils.hpp>
#include <Addins/Calc/conversions.hpp>

/*
sal_Int32 SAL_CALL QLAddin::ohTrigger(
        const ANY &dummy0,
        const ANY &dummy1,
        const ANY &dummy2,
        const ANY &dummy3,
        const ANY &dummy4,
        const ANY &dummy5,
        const ANY &dummy6,
        const ANY &dummy7,
        const ANY &dummy8,
        const ANY &dummy9,
        const ANY &trigger) THROWDEF_RTE_IAE {
    long returnValue = 1;
    return returnValue;
}

SEQSEQ(STRING) SAL_CALL QLAddin::ohPropertyNames(
        const STRING &objectID,
        const ANY &trigger) THROWDEF_RTE_IAE {
    try {

        // convert input datatypes to C++ datatypes

        std::string objectIDCpp = ouStringToStlString(objectID);

        // convert object IDs into library objects

        OH_GET_OBJECT(objectIDObj, objectIDCpp, ObjectHandler::Object)

        // invoke the member function

        std::vector<std::string> returnValue = objectIDObj->propertyNames();

        // convert and return the return value

        SEQSEQ(STRING) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        ObjectHandler::logMessage(std::string("ERROR: ohPropertyNames: ") + e.what(), 2);
        THROW_RTE;
    }
}

ANY SAL_CALL QLAddin::ohPropertyValue(
        const STRING &objectID,
        const STRING &fieldName,
        const ANY &trigger) THROWDEF_RTE_IAE {
    try {

        // convert input datatypes to C++ datatypes

        std::string objectIDCpp = ouStringToStlString(objectID);

        std::string fieldNameCpp = ouStringToStlString(fieldName);

        // convert object IDs into library objects

        OH_GET_OBJECT(objectIDObj, objectIDCpp, ObjectHandler::Object)

        // invoke the member function

        boost::any returnValue = objectIDObj->propertyValue(
            fieldNameCpp);

        // convert and return the return value

        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        ObjectHandler::logMessage(std::string("ERROR: ohPropertyValue: ") + e.what(), 2);
        THROW_RTE;
    }
}
*/

