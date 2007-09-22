
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

#include <fstream>
#include <set>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/filesystem.hpp>
#include <examplefactory.hpp>
#include <ExampleObjects/ValueObjects/accountvalueobject.hpp>
#include <ExampleObjects/ValueObjects/customervalueobject.hpp>
#include <oh/repository.hpp>
#include <oh/ValueObjects/vo_range.hpp>
//#include <cstring>

namespace ExampleAddin {

    ExampleFactory &ExampleFactory::instance() {
        if (instance_) {
            ExampleFactory *ret = dynamic_cast<ExampleFactory*>(instance_);
            if (ret) return *ret;
        }
        OH_FAIL("Attempt to reference uninitialized ExampleFactory object");
    }

    void tpl_register_classes(boost::archive::xml_oarchive &ar) {
        ar.register_type<ObjectHandler::ValueObjects::ohRange>();
        ar.register_type<AccountExample::AccountValueObject>();
        ar.register_type<AccountExample::CustomerValueObject>();
    }

    void tpl_register_classes(boost::archive::xml_iarchive &ar) {
        ar.register_type<ObjectHandler::ValueObjects::ohRange>();
        ar.register_type<AccountExample::AccountValueObject>();
        ar.register_type<AccountExample::CustomerValueObject>();
    }

    int ExampleFactory::saveObject(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
        const char *path,
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

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa);
        oa << boost::serialization::make_nvp("object_list", valueObjects);
        return valueObjects.size();
    }

    std::string ExampleFactory::processObject(
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

    void ExampleFactory::processPath(
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
        return STRICMP(extension.c_str(), ".XML") == 0;
    }

    std::vector<std::string> ExampleFactory::loadObject(
        const char *path,
        bool overwriteExisting) const {

        boost::filesystem::path boostPath(path);
        OH_REQUIRE(boost::filesystem::exists(boostPath), "Invalid path : " << path);

        std::vector<std::string> returnValue;

        if (boost::filesystem::is_directory(boostPath)) {

            // Workaround for apparent bug in boost::filesystem version 1.34.1.
            // If the directory is empty, testing equality between recursive_directory_iterator
            // and the end iterator returns true when it should return false and subsequent
            // dereference of the iterator crashes the program.  So we first perform the test with
            // directory_iterator which correctly returns false for an empty directory.
            OH_REQUIRE(boost::filesystem::directory_iterator(boostPath) != boost::filesystem::directory_iterator(),
                "Directory is empty : " << path);

            bool fileFound = false;
            for (boost::filesystem::recursive_directory_iterator itr(boostPath);
                itr != boost::filesystem::recursive_directory_iterator(); ++itr) {
                if (boost::filesystem::is_regular(itr->status()) && hasXmlExtension(*itr)) {
                    fileFound = true;
                    processPath(itr->path().string(), overwriteExisting, returnValue);
                }
            }

            OH_REQUIRE(fileFound, "No XML files found in path : " << path);

        } else {
            //OH_REQUIRE(hasXmlExtension(boostPath),
            //    "The file '" << boostPath << "' does not have extension '.xml'");
            processPath(boostPath.string(), overwriteExisting, returnValue);
        }

        // processPath() will already have thrown if empty files were detected
        // so the following is a redundant sanity check.
        OH_REQUIRE(!returnValue.empty(), "No objects loaded from path : " << path);

        return returnValue;
    }

    std::string ExampleFactory::saveObjectString(
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

    std::vector<std::string> ExampleFactory::loadObjectString(
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

