#include <qlo/qladdindefines.hpp>
#include <qlo/typefactory.hpp>
#include <qlo/conversions.hpp>
#include <oh/objecthandler.hpp>

#include <Addins/Calc/qladdin.hpp>
#include <Addins/Calc/calcutils.hpp>
#include <Addins/Calc/conversions.hpp>

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

        OH_GET_OBJECT(objectIDObj, objectIDCpp, ObjHandler::Object)

        // invoke the member function

        std::vector<std::string> returnValue = objectIDObj->propertyNames();

        // convert and return the return value


/* no VO - not a constructor*/
        SEQSEQ(STRING) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohPropertyNames: ") + e.what(), 2);
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

        OH_GET_OBJECT(objectIDObj, objectIDCpp, ObjHandler::Object)

        // invoke the member function

        boost::any returnValue = objectIDObj->propertyValue(
            fieldNameCpp);

        // convert and return the return value


/* no VO - not a constructor*/
        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("ERROR: ohPropertyValue: ") + e.what(), 2);
        THROW_RTE;
    }
}
