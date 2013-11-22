
#include "quantLibAddinTest.hpp"
#include <oh/objecthandler.hpp>
#include <ql/math/comparison.hpp>
#include <ql/quotes/simplequote.hpp>

namespace QuantLibAddin {

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

    namespace ValueObjects {

        class qlSimpleQuote : public ObjectHandler::ValueObject {
            friend class boost::serialization::access;
        public:
            qlSimpleQuote() {}
            qlSimpleQuote(
                const std::string& ObjectId,
                double Value_,
                bool Permanent);

            const std::set<std::string>& getSystemPropertyNames() const;
            std::vector<std::string> getPropertyNamesVector() const;
            ObjectHandler::property_t getSystemProperty(const std::string&) const;
            void setSystemProperty(const std::string& name, const ObjectHandler::property_t& value);

        protected:
            static const char* mPropertyNames[];
            static std::set<std::string> mSystemPropertyNames;

            double Value_;
            bool Permanent_;

            template<class Archive>
            void serialize(Archive& ar, const unsigned int) {
            boost::serialization::void_cast_register<qlSimpleQuote, ObjectHandler::ValueObject>(this, this);
                ar  & boost::serialization::make_nvp("ObjectId", objectId_)
                    & boost::serialization::make_nvp("Value", Value_)
                    & boost::serialization::make_nvp("Permanent", Permanent_)
                    & boost::serialization::make_nvp("UserProperties", userProperties);
            }
        };

        const char* qlSimpleQuote::mPropertyNames[] = {
            "Value",
            "Permanent"
        };

        std::set<std::string> qlSimpleQuote::mSystemPropertyNames(
            mPropertyNames, mPropertyNames + sizeof(mPropertyNames) / sizeof(const char*));

        const std::set<std::string>& qlSimpleQuote::getSystemPropertyNames() const {
            return mSystemPropertyNames;
        }

        std::vector<std::string> qlSimpleQuote::getPropertyNamesVector() const {
            std::vector<std::string> ret(
                mPropertyNames, mPropertyNames + sizeof(mPropertyNames) / sizeof(const char*));
            for (std::map<std::string, ObjectHandler::property_t>::const_iterator i = userProperties.begin();
                i != userProperties.end(); ++i)
                ret.push_back(i->first);
            return ret;
        }

        ObjectHandler::property_t qlSimpleQuote::getSystemProperty(const std::string& name) const {
            std::string nameUpper = boost::algorithm::to_upper_copy(name);
            if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
                return objectId_;
            else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
                return className_;
            else if(strcmp(nameUpper.c_str(), "VALUE")==0)
                return Value_;
            else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
                return Permanent_;
            else
                OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
        }

        void qlSimpleQuote::setSystemProperty(const std::string& name, const ObjectHandler::property_t& value) {
            std::string nameUpper = boost::algorithm::to_upper_copy(name);
            if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
                objectId_ = boost::get<std::string>(value);
            else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
                className_ = boost::get<std::string>(value);
            else if(strcmp(nameUpper.c_str(), "VALUE")==0)
                className_ = boost::get<double>(value);
            else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
                Permanent_ = ObjectHandler::convert2<bool>(value);
            else
                OH_FAIL("Error: attempt to set non-existent Property: '" + name + "'");
        }

        qlSimpleQuote::qlSimpleQuote(
                const std::string& ObjectId,
                double Value,
                bool Permanent) :
            ObjectHandler::ValueObject(ObjectId, "qlSimpleQuote", Permanent),
            Value_(Value),
            Permanent_(Permanent) { }
    }
}

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

