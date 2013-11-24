
#ifndef obj_hpp
#define obj_hpp

#include <string>
#include <oh/libraryobject.hpp>
#include <oh/valueobject.hpp>
#include <boost/shared_ptr.hpp>

#include <ql/quotes/simplequote.hpp>
#include <ql/math/comparison.hpp>

namespace QuantLibAddin {

    bool close(double x, double y);

    class SimpleQuote :
        public ObjectHandler::LibraryObject<QuantLib::SimpleQuote> {
        public:
            SimpleQuote(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                double value,
                bool permanent)
            : ObjectHandler::LibraryObject<QuantLib::SimpleQuote>(properties, permanent) {
                libraryObject_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(value));
            }
    };

}

#endif

