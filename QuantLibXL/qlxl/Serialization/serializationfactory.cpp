
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

#include <fstream>
#include <set>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/vector.hpp>
#include <qlxl/Serialization/serializationfactory.hpp>
#include <qlxl/Serialization/serialization_register.hpp>
#include <oh/repository.hpp>
#include <boost/filesystem.hpp>

namespace QuantLibXL {

    SerializationFactory &SerializationFactory::instance() {
        if (instance_) {
            SerializationFactory *ret = dynamic_cast<SerializationFactory*>(instance_);
            if (ret) return *ret;
        }
        OH_FAIL("Attempt to reference uninitialized SerializationFactory object");
    }

    int SerializationFactory::saveObject(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
        const char *path,
        bool forceOverwrite) const {

        OH_REQUIRE(forceOverwrite || !boost::filesystem::exists(path),
            "Cannot overwrite output file : " << path);

        OH_REQUIRE(objectList.size(), "Object list is empty");

        std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
        std::set<std::string> seen;
        std::vector<boost::shared_ptr<ObjectHandler::Object> >::const_iterator i;
        for (i=objectList.begin(); i!=objectList.end(); ++i) {
            boost::shared_ptr<ObjectHandler::Object> object = *i;
            std::string objectID = boost::any_cast<std::string>(
                object->properties()->getProperty("objectID"));
            if (seen.find(objectID) == seen.end()) {
                valueObjects.push_back(object->properties());
                seen.insert(objectID);
            }
        }

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa);
        oa << boost::serialization::make_nvp("object_list", valueObjects);
        return valueObjects.size();
    }

    int SerializationFactory::processPath(
        const std::string &path,
        bool overwriteExisting) const {

        try {

            std::ifstream ifs(path.c_str());
            boost::archive::xml_iarchive ia(ifs);
            register_in(ia);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
            ia >> boost::serialization::make_nvp("object_list", valueObjects);
            OH_REQUIRE(valueObjects.size(), "Object list is empty");

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                boost::shared_ptr<ObjectHandler::ValueObject> valueObject = *i;
                // Code to overwrite the object ID
                //valueObject->setProperty("objectID", XXX);
                CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
                OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
                Creator creator = j->second;
                boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
                std::string objectID =
                    boost::any_cast<std::string>(valueObject->getProperty("objectID"));
                if (overwriteExisting)
                    ObjectHandler::Repository::instance().deleteObject(objectID);
                ObjectHandler::Repository::instance().storeObject(objectID, object);
            }

            return valueObjects.size();

        } catch (const std::exception &e) {
            OH_FAIL("Error deserializing file " << path << ": " << e.what());
        }
    }

    int SerializationFactory::loadObject(
        const char *path,
        bool overwriteExisting) const {

        OH_REQUIRE(boost::filesystem::exists(path), "Invalid path : " << path);

        if (boost::filesystem::is_directory(path)) {

            int returnValue = 0;
            for (boost::filesystem::directory_iterator itr(path); 
                itr!=boost::filesystem::directory_iterator(); ++itr) {

                if (boost::filesystem::is_regular(itr->path().string()))
                    returnValue += processPath(itr->path().string(), overwriteExisting);
            }
            return returnValue;

        } else {
            return processPath(path, overwriteExisting);
        }
    }

    void register_in(boost::archive::xml_iarchive& ia) {
        tpl_register_classes(ia);
    }

    void register_out(boost::archive::xml_oarchive& oa) {
        tpl_register_classes(oa);
    }

}

