
#include "vo_hw.hpp"
#include <boost/algorithm/string/case_conv.hpp>

namespace QuantLibAddin {

    namespace ValueObjects {

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

