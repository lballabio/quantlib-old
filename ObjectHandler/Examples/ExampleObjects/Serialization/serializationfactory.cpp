
/*
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
#include <oh/config.hpp>
#endif

#include <ExampleObjects/Serialization/serializationfactory.hpp>
#include <ExampleObjects/Serialization/creators.hpp>
#include <ExampleObjects/ValueObjects/accountvalueobject.hpp>
#include <ExampleObjects/ValueObjects/customervalueobject.hpp>
#include <oh/repository.hpp>
#include <oh/valueobjects/vo_range.hpp>

#include <set>
#include <fstream>

#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/shared_ptr.hpp>

namespace AccountExample {

	SerializationFactory::SerializationFactory() {
		registerCreator("Account", createAccount);
		registerCreator("Customer", createCustomer);
	}
/*
	SerializationFactory &SerializationFactory::instance() {
		if (instance_) {
			SerializationFactory *ret = dynamic_cast<SerializationFactory*>(instance_);
			if (ret) return *ret;
		}
		OH_FAIL("Attempt to reference uninitialized SerializationFactory object");
	}
*/

	void SerializationFactory::register_out(boost::archive::xml_oarchive &ar,
		std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >& valueObjects) {
		ar.register_type<ObjectHandler::ValueObjects::ohRange>();
		ar.register_type<AccountExample::AccountValueObject>();
		ar.register_type<AccountExample::CustomerValueObject>();
		ar << boost::serialization::make_nvp("object_list", valueObjects);
	}

	void SerializationFactory::register_in(boost::archive::xml_iarchive &ar,
		std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >& valueObjects) {
		ar.register_type<ObjectHandler::ValueObjects::ohRange>();
		ar.register_type<AccountExample::AccountValueObject>();
		ar.register_type<AccountExample::CustomerValueObject>();
		ar >> boost::serialization::make_nvp("object_list", valueObjects);
	}

}
