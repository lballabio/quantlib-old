
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
#include <oh/range.hpp>

namespace ObjectHandler {

    boost::shared_ptr<Object> createRange(
        const boost::shared_ptr<ValueObject> &valueObject) {

        std::vector<std::vector<double> > values = boost::any_cast<std::vector<std::vector<double> > >(valueObject->getProperty("values"));
        boost::shared_ptr<Object> object(new Range(values));
        object->setProperties(valueObject);
        return object;
    }

    SerializationFactory *SerializationFactory::instance_;

    SerializationFactory::SerializationFactory() {
        instance_ = this;
        registerCreator("ohRange", createRange);
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

}

