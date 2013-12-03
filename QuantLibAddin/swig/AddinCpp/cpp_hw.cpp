
#include "cpp_hw.hpp"
#include "ValueObjects/vo_hw.hpp"
#include "AddinObjects/obj_hw.hpp"
#include <boost/shared_ptr.hpp>
#include <oh/repository.hpp>

//#include "quantLibAddinTest.hpp"
//#include <oh/objecthandler.hpp>
#include <ql/math/comparison.hpp>
#include <ql/quotes/simplequote.hpp>

static ObjectHandler::Repository repository;

bool QuantLibAddin::qlClose(double x, double y) {
    return QuantLib::close(x, y);
}

std::string QuantLibAddin::qlSimpleQuote(const std::string &objectID, double Value) {
        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSimpleQuote(
                objectID, Value, false));
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SimpleQuote(
                valueObject, Value, false));
        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(
                objectID, object, false, valueObject);
        return returnValue;
}

double QuantLibAddin::qlSimpleQuoteValue(const std::string &objectID) {
    OH_GET_REFERENCE(x, objectID, QuantLibAddin::SimpleQuote, QuantLib::SimpleQuote);
    return x->value();
}

