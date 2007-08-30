
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
#include <Addins/Cpp/Serialization/serializationfactory.hpp>
#include <Addins/Cpp/Serialization/serialization_register.hpp>
#include <oh/repository.hpp>
#include <boost/filesystem.hpp>

namespace QuantLibAddinCpp {

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

        // Create a boost path object from the char*.
        boost::filesystem::path boostPath(path);

        // Ensure that the parent directory exists.
        OH_REQUIRE(boost::filesystem::exists(boostPath.branch_path()),
            "Invalid path : " << path);

        // If the file itself exists then ensure we can overwrite it.
        OH_REQUIRE(forceOverwrite || !boost::filesystem::exists(boostPath),
            "Cannot overwrite output file : " << path);

        OH_REQUIRE(objectList.size(), "Object list is empty");

        std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
        std::set<std::string> seen;
        std::vector<boost::shared_ptr<ObjectHandler::Object> >::const_iterator i;
        for (i=objectList.begin(); i!=objectList.end(); ++i) {
            boost::shared_ptr<ObjectHandler::Object> object = *i;
            std::string objectID = boost::any_cast<std::string>(
                object->properties()->getProperty("OBJECTID"));
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

    std::string SerializationFactory::processObject(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {

        // Code to overwrite the object ID
        //valueObject->setProperty("OBJECTID", XXX);
        CreatorMap::const_iterator i = creatorMap_().find(valueObject->className());
        OH_REQUIRE(i != creatorMap_().end(), "No creator for class " << valueObject->className());
        Creator creator = i->second;
        boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
        std::string objectID =
            boost::any_cast<std::string>(valueObject->getProperty("OBJECTID"));
        if (overwriteExisting)
            ObjectHandler::Repository::instance().deleteObject(objectID);
        ObjectHandler::Repository::instance().storeObject(objectID, object);
        return objectID;
    }

    void SerializationFactory::processPath(
        const std::string &path,
        bool overwriteExisting,
        std::vector<std::string> &processedIDs) const {

        try {

            std::ifstream ifs(path.c_str());
            boost::archive::xml_iarchive ia(ifs);
            register_in(ia);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
            ia >> boost::serialization::make_nvp("object_list", valueObjects);
            OH_REQUIRE(valueObjects.size(), "Object list is empty");

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            int count = 0;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                try {
                    processedIDs.push_back(processObject(*i, overwriteExisting));
                    count++;
                } catch (const std::exception &e) {
                    OH_FAIL("Error processing item " << count << ": " << e.what());
                }
            }

        } catch (const std::exception &e) {
            OH_FAIL("Error deserializing file " << path << ": " << e.what());
        }
    }
 
    bool hasXmlExtension(const boost::filesystem::path &path) {
        std::string extension = boost::filesystem::extension(path);
        return _stricmp(extension.c_str(), ".XML") == 0;
    }

    std::vector<std::string> SerializationFactory::loadObject(
        const char *path,
        bool overwriteExisting) const {

        boost::filesystem::path boostPath(path);
        OH_REQUIRE(boost::filesystem::exists(boostPath), "Invalid path : " << path);

        std::vector<std::string> returnValue;

        if (boost::filesystem::is_directory(boostPath)) {

            boost::filesystem::recursive_directory_iterator end_itr;
            for (boost::filesystem::recursive_directory_iterator itr(boostPath); itr != end_itr; ++itr) {
                if (boost::filesystem::is_regular(itr->status()) && hasXmlExtension(*itr))
                    processPath(itr->path().string(), overwriteExisting, returnValue);
            }

        } else {
            //OH_REQUIRE(hasXmlExtension(boostPath),
            //    "The file '" << boostPath << "' does not have extension '.xml'");
            processPath(boostPath.string(), overwriteExisting, returnValue);
        }

        return returnValue;
    }

    void register_in(boost::archive::xml_iarchive& ia) {
        tpl_register_classes(ia);
    }

    void register_out(boost::archive::xml_oarchive& oa) {
        tpl_register_classes(oa);
    }

}

