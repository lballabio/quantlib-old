
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

#include <fstream>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/variant.hpp>
//#include <boost/serialization/vector.hpp>
#include <examplefactory.hpp>
#include <ExampleObjects/ValueObjects/accountvalueobject.hpp>
#include <ExampleObjects/ValueObjects/customervalueobject.hpp>
#include <oh/repository.hpp>

namespace ExampleAddin {

    ExampleFactory &ExampleFactory::instance() {
        if (instance_) {
            ExampleFactory *ret = dynamic_cast<ExampleFactory*>(instance_);
            if (ret) return *ret;
        }
        OH_FAIL("Attempt to reference uninitialized ExampleFactory object");
    }

    void tpl_register_classes(boost::archive::xml_oarchive &ar) {
        ar.register_type<AccountExample::AccountValueObject>();
        ar.register_type<AccountExample::CustomerValueObject>();
    }

    void tpl_register_classes(boost::archive::xml_iarchive &ar) {
        ar.register_type<AccountExample::AccountValueObject>();
        ar.register_type<AccountExample::CustomerValueObject>();
    }

	void ExampleFactory::saveObject(const boost::shared_ptr<ObjectHandler::Object> &object, const char *path) const {
        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        tpl_register_classes(oa);
		oa << boost::serialization::make_nvp("oh_object", object->properties());
	}

    void ExampleFactory::saveObject(
		const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
		const char *path) const {

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        tpl_register_classes(oa);
		for (std::vector<boost::shared_ptr<ObjectHandler::Object> >::const_iterator i=objectList.begin();i!=objectList.end();++i){	
			// We need to supply a name for the object to be serialized.
			// The objectID isn't suitable because certain values of objectID are
			// invalid as XML tags e.g. values beginning with numeric characters.
			// For our purposes the tag is ignored, so just supply a dummy string.
			//std::string objectID = boost::any_cast<std::string>(valueObject->getProperty("objectID"));
			//oa << boost::serialization::make_nvp(objectID.c_str(), valueObject);
			oa << boost::serialization::make_nvp("oh_object", (*i)->properties());
		}
    }

	boost::shared_ptr<ObjectHandler::Object> ExampleFactory::loadObject(const std::string &objectID, const char *path) const {
        std::ifstream ifs(path);
        boost::archive::xml_iarchive ia(ifs);
        tpl_register_classes(ia);
		boost::shared_ptr<ObjectHandler::ValueObject> valueObject;
		ia >> boost::serialization::make_nvp(objectID.c_str(), valueObject);
		// This VO has picked up the ID of the old VO that was deserialized.
		// Override this value with the new ID supplied by the caller.
		valueObject->setID(objectID);
		CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
		OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
		Creator creator = j->second;
		boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
		ObjectHandler::Repository::instance().storeObject(objectID, object);
		return object;
	}

	std::vector<boost::shared_ptr<ObjectHandler::Object> > ExampleFactory::loadObject(
        const std::vector<std::string> &idList, const char *path) const {

		std::vector<boost::shared_ptr<ObjectHandler::Object> > returnValues;
        std::ifstream ifs(path);
        boost::archive::xml_iarchive ia(ifs);
        tpl_register_classes(ia);

		for(std::vector<std::string>::const_iterator i=idList.begin();i!=idList.end();++i){
			boost::shared_ptr<ObjectHandler::ValueObject> valueObject;
			ia >> boost::serialization::make_nvp(i->c_str(), valueObject);
			// This VO has picked up the ID of the old VO that was deserialized.
			// Override this value with the new ID supplied by the caller.
			valueObject->setID(*i);
			CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
			OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
			Creator creator = j->second;
			boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
			ObjectHandler::Repository::instance().storeObject(*i, object);
			returnValues.push_back(object);
		}
	    return returnValues;
	}
}

