
/*!
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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

#ifndef example_customer_valueobject_hpp
#define example_customer_valueobject_hpp

#include <oh/valueobject.hpp>
#include <boost/serialization/void_cast.hpp>
#include <boost/serialization/nvp.hpp>
#include <vector>
#include <string>

namespace AccountExample {

    class CustomerValueObject : public ObjectHandler::ValueObject {
        friend class boost::serialization::access;

    public:

        CustomerValueObject() {}
        CustomerValueObject(
            const std::string &objectID,
            const std::string &name, 
            const long &age,
            bool permanent)
            : ObjectHandler::ValueObject(objectID, "Customer", permanent), name_(name), age_(age) {}

        const std::set<std::string>& getSystemPropertyNames() const;
        std::vector<std::string> getPropertyNamesVector() const;
        ObjectHandler::property_t getSystemProperty(const std::string& name) const;
        void setSystemProperty(const std::string& name, const ObjectHandler::property_t& value);

    private:

        static const char* mPropertyNames[];
        std::string name_;
        long age_;

        template<class Archive>
        void serialize(Archive& ar, const unsigned int) {
            boost::serialization::void_cast_register<CustomerValueObject, ValueObject>(this, this);
            ar & boost::serialization::make_nvp("ObjectId", objectId_)
               & boost::serialization::make_nvp("ClassName", className_)
               & boost::serialization::make_nvp("Name", name_)
               & boost::serialization::make_nvp("Age", age_)
               & boost::serialization::make_nvp("Permanent", permanent_);
        }

    };

}

#endif

