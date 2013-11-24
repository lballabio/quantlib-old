
#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/register/register_all.hpp>
#include <ohxl/functions/export.hpp>
#include <ohxl/utilities/xlutilities.hpp>
#include <ohxl/objectwrapperxl.hpp>
#include "ValueObjects/vo.hpp"
#include "AddinObjects/obj.hpp"

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <oh/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <sstream>

DLLEXPORT int xlAutoOpen() {

    // Instantiate the ObjectHandler Repository
    static ObjectHandler::RepositoryXL repositoryXL;

    static XLOPER xDll;

    try {

        Excel(xlGetName, &xDll, 0);

        ObjectHandler::Configuration::instance().init();

        registerOhFunctions(xDll);

        Excel(xlfRegister, 0, 7, &xDll,
            // function code name
            TempStrNoSize("\x05""close"),
            // parameter codes
            TempStrNoSize("\x03""XX#"),
            // function display name
            TempStrNoSize("\x05""close"),
            // comma-delimited list of parameters
            TempStrNoSize("\x03""x,y"),
            // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x01""1"),
            // function category
            TempStrNoSize("\x07""Example"));

        Excel(xlfRegister, 0, 7, &xDll,
            // function code name
            TempStrNoSize("\x0b""SimpleQuote"),
            // parameter codes
            TempStrNoSize("\x04""CCX#"),
            // function display name
            TempStrNoSize("\x0b""SimpleQuote"),
            // comma-delimited list of parameters
            TempStrNoSize("\x0e""ObjectId,value"),
            // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x01""1"),
            // function category
            TempStrNoSize("\x07""Example"));

        Excel(xlfRegister, 0, 7, &xDll,
            // function code name
            TempStrNoSize("\x10""SimpleQuotevalue"),
            // parameter codes
            TempStrNoSize("\x03""XC#"),
            // function display name
            TempStrNoSize("\x10""SimpleQuotevalue"),
            // comma-delimited list of parameters
            TempStrNoSize("\x08""ObjectId"),
            // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x01""1"),
            // function category
            TempStrNoSize("\x07""Example"));

        Excel(xlFree, 0, 1, &xDll);
        return 1;

    } catch (const std::exception &e) {

        std::ostringstream err;
        err << "Error loading AddinXlHw: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;

    } catch (...) {

        Excel(xlFree, 0, 1, &xDll);
        return 0;

    }
}

DLLEXPORT int xlAutoClose() {

    static XLOPER xDll;

    try {

        Excel(xlGetName, &xDll, 0);

        unregisterOhFunctions(xDll);

        Excel(xlFree, 0, 1, &xDll);
        return 1;

    } catch (const std::exception &e) {

        std::ostringstream err;
        err << "Error unloading AddinXlHw: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;

    } catch (...) {

        Excel(xlFree, 0, 1, &xDll);
        return 0;

    }

}

DLLEXPORT void xlAutoFree(XLOPER *px) {

    freeOper(px);

}

DLLEXPORT char *close() {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("close"));

        std::string returnValue = 
            QuantLibAddin::close();

        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(returnValue, ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT char *SimpleQuote(char *objectID, double *value) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("SimpleQuote"));

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSimpleQuote(objectID, false));

        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SimpleQuote(valueObject, *x, false));

        std::string returnValue =
            ObjectHandler::RepositoryXL::instance().storeObject(objectID, object, true);

        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(returnValue, ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT double *SimpleQuotevalue(char *objectID) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("SimpleQuotevalue"));

        OH_GET_REFERENCE(x, objectID, QuantLibAddin::SimpleQuote, QuantLib::SimpleQuote);

        static double ret;
        ret = x->value(*);
        return &ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}
