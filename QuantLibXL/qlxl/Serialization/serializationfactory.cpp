
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

#include <oh/ohdefines.hpp>
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
#include <boost/regex.hpp>

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
        const std::string &path,
        bool forceOverwrite) const {

        // Create a boost path object from the char*.
        boost::filesystem::path boostPath(path);

        // If a parent directory has been specified then ensure it exists.
        if (boostPath.has_branch_path()) {
            OH_REQUIRE(boost::filesystem::exists(boostPath.branch_path()),
                "Invalid path : " << path);
        }

        // If the file itself exists then ensure we can overwrite it.
        if (boost::filesystem::exists(boostPath)) {
            if (forceOverwrite) {
                try {
                    boost::filesystem::remove(boostPath);
                } catch (const boost::filesystem::basic_filesystem_error<boost::filesystem::path>&) {
                    OH_FAIL("Overwrite=TRUE but overwrite failed for existing file: " << path);
                }
            } else {
                OH_FAIL("Overwrite=FALSE and the specified output file exists: " << path);
            }
        }

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

        std::ofstream ofs(path.c_str());
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
        ObjectHandler::Repository::instance().storeObject(objectID, object, overwriteExisting);
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

    std::vector<std::string> SerializationFactory::loadObject(
        const std::string &directory,
        const std::string &pattern,
        bool recurse,
        bool overwriteExisting) const {

        boost::filesystem::path boostPath(directory);
        OH_REQUIRE(boost::filesystem::exists(boostPath) && boost::filesystem::is_directory(boostPath),
            "The specified directory is not valid : " << directory);

        std::vector<std::string> returnValue;
        bool fileFound = false;
        boost::regex r(pattern, boost::regex::perl | boost::regex::icase);

        if (recurse) {

            for (boost::filesystem::recursive_directory_iterator itr(boostPath);
                itr != boost::filesystem::recursive_directory_iterator(); ++itr) {
                if (regex_match(itr->path().leaf(), r) && boost::filesystem::is_regular(itr->status())) {
                    fileFound = true;
                    processPath(itr->path().string(), overwriteExisting, returnValue);
                }
            }

        } else {

            for (boost::filesystem::directory_iterator itr(boostPath);
                itr != boost::filesystem::directory_iterator(); ++itr) {
                if (regex_match(itr->path().leaf(), r) && boost::filesystem::is_regular(itr->status())) {
                    fileFound = true;
                    processPath(itr->path().string(), overwriteExisting, returnValue);
                }
            }

        }

        OH_REQUIRE(fileFound, "Found no files matching pattern '" << pattern << "' in directory '"
            << directory << "' with recursion = " << std::boolalpha << recurse);

        // processPath() will already have thrown if empty files were detected
        // so the following is a redundant sanity check.
        OH_REQUIRE(!returnValue.empty(), "No objects loaded from directory : " << directory);

        return returnValue;
    }

    std::string SerializationFactory::saveObjectString(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> > &objectList,
        bool forceOverwrite) {

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

        std::ostringstream os;
        boost::archive::xml_oarchive oa(os);
        register_out(oa);
        oa << boost::serialization::make_nvp("object_list", valueObjects);
        return os.str();
    }

    std::vector<std::string> SerializationFactory::loadObjectString(
        const std::string &xml,
        bool overwriteExisting) {

        std::vector<std::string> returnValue;

        try {
            std::istringstream xmlStream(xml);
            boost::archive::xml_iarchive ia(xmlStream);
            register_in(ia);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
            ia >> boost::serialization::make_nvp("object_list", valueObjects);
            OH_REQUIRE(valueObjects.size(), "Object list is empty");

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            int count = 0;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                try {
                    returnValue.push_back(processObject(*i, overwriteExisting));
                    count++;
                } catch (const std::exception &e) {
                    OH_FAIL("Error processing item " << count << ": " << e.what());
                }
            }

        } catch (const std::exception &e) {
            OH_FAIL("Error deserializing xml : " << e.what());
        }

        OH_REQUIRE(!returnValue.empty(), "No objects loaded from xml");

        return returnValue;
    }

    void register_in(boost::archive::xml_iarchive& ia) {
        tpl_register_classes(ia);
    }

    void register_out(boost::archive::xml_oarchive& oa) {
        tpl_register_classes(oa);
    }

}

