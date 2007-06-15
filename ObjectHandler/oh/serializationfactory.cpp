
/*
 Copyright (C) 2007 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <oh/config.hpp>
#endif

#include <oh/serializationfactory.hpp>

namespace ObjectHandler {

    SerializationFactory *SerializationFactory::instance_;

    SerializationFactory::SerializationFactory() {
        instance_ = this;
    }

    SerializationFactory::~SerializationFactory() {
        instance_ = 0;
    }

    SerializationFactory &SerializationFactory::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized SerializationFactory object");
        return *instance_;
    }

    SerializationFactory::CreatorMap &SerializationFactory::creatorMap_() const {
        static CreatorMap creatorMap;
        return creatorMap;
    }

    void SerializationFactory::registerCreator(const std::string &className, const Creator &creator) {
        creatorMap_()[className] = creator;
    }

     boost::shared_ptr<Object> SerializationFactory::makeObject(const char *path, const char *objectID) const {

        boost::shared_ptr<ValueObject> valueObject = loadObject(path, objectID);
        CreatorMap::const_iterator i = creatorMap_().find(valueObject->className());
        OH_REQUIRE(i != creatorMap_().end(), "No creator for class " << valueObject->className());
        Creator creator = i->second;
        return creator(valueObject);

     }

}

