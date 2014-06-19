
#ifndef vo_hw_hpp
#define vo_hw_hpp

#include <string>
#include <set>
#include <oh/valueobject.hpp>

namespace QuantLibAddin {

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

} }

#endif

