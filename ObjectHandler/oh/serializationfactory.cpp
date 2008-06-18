/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2008 Nazcatech sprl Belgium

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
#include <oh/processor.hpp>
#include <oh/range.hpp>
#include <oh/group.hpp>
#include <oh/repository.hpp>

#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/shared_ptr.hpp>

#include <fstream>

namespace ObjectHandler {

    std::map<std::string, ProcessorPtr> ProcessorFactory::processorMap_;

    boost::shared_ptr<Object> createRange(
        const boost::shared_ptr<ValueObject> &valueObject) {

        bool permanent = 
            boost::get<bool>(valueObject->getProperty("PERMANENT"));
        std::vector<std::vector<double> > values = 
            ObjectHandler::matrix::convert2<double>(valueObject->getProperty("VALUES"), "VALUES");

        boost::shared_ptr<Object> object(new Range(valueObject, values, permanent));
        return object;
    }

    boost::shared_ptr<Object> createGroup(
        const boost::shared_ptr<ValueObject> &valueObject) {

        bool permanent = 
            boost::get<bool>(valueObject->getProperty("PERMANENT"));
        std::vector<std::string> list = 
            boost::get<std::vector<std::string> >(valueObject->getProperty("LIST"));

        boost::shared_ptr<Object> object(new Group(valueObject, list, permanent));
        return object;
    }

    SerializationFactory *SerializationFactory::instance_;

    SerializationFactory::SerializationFactory() {
        instance_ = this;
        registerCreator("ohRange", createRange);
        registerCreator("ohGroup", createGroup);

        ProcessorFactory::processorMap_["DefaultProcessor"] = ProcessorPtr(new DefaultProcessor());
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

    StrObjectPair SerializationFactory::createObject(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {

        // Code to overwrite the object ID
        //valueObject->setProperty("OBJECTID", XXX);
        CreatorMap::const_iterator i = creatorMap_().find(valueObject->className());
        OH_REQUIRE(i != creatorMap_().end(), "No creator for class " << valueObject->className());

        StrObjectPair object;
        Creator creator = i->second;
        object.second = creator(valueObject);

        object.first = boost::get<std::string>(valueObject->getProperty("OBJECTID"));
        ObjectHandler::Repository::instance().storeObject(object.first, object.second, overwriteExisting);

        return object;
    }

    boost:: shared_ptr<Object> SerializationFactory::recreateObject( 
        boost::shared_ptr<ObjectHandler::ValueObject> valueObject) const {

        CreatorMap::const_iterator i = creatorMap_().find(valueObject->className());
        OH_REQUIRE(i != creatorMap_().end(), "No creator for class " << valueObject->className());
        Creator creator = i->second;
        boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
        return object;
    }

    int SerializationFactory::saveObject(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
        const std::string &path,
        bool forceOverwrite)  {

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
            std::string objectID
                = boost::get<std::string>(object->properties()->getProperty("OBJECTID"));
            if (seen.find(objectID) == seen.end()) {
                valueObjects.push_back(object->properties());
                seen.insert(objectID);
            }
        }

        // Provisionally comment out this sort because
        // 1) It causes legs and schedules to appear in the wrong sequence
        // 2) In our environment we can control the sequence in which objects are saved
        // 3) I don't understand why this sort is required anyway?
        //std::stable_sort(valueObjects.begin(), valueObjects.end(), compareCategory);

        std::ofstream ofs(path.c_str());
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa, valueObjects);
        return valueObjects.size();
    }

    /*std::string SerializationFactory::processObject(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting)  {

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
    }*/

    void SerializationFactory::processPath(
        const std::string &path,
        bool overwriteExisting,
        std::vector<std::string> &processedIDs)  {

        try {

            std::ifstream ifs(path.c_str());
            boost::archive::xml_iarchive ia(ifs);
            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;

            register_in(ia, valueObjects);

            OH_REQUIRE(valueObjects.size(), "Object list is empty");

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            int count = 0;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                try {
                    processedIDs.push_back(ProcessorFactory::getProcessor(*i)->process(*this, *i, overwriteExisting));
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
        bool overwriteExisting)  {

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

        ProcessorFactory::postProcess();

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
            std::string objectID = boost::get<std::string>(
                   object->properties()->getProperty("OBJECTID"));
            if (seen.find(objectID) == seen.end()) {
                valueObjects.push_back(object->properties());
                seen.insert(objectID);
            }
        }

        //std::stable_sort(valueObjects.begin(), valueObjects.end(), compareCategory);

        std::ostringstream os;
        {
            boost::archive::xml_oarchive oa(os);
            register_out(oa, valueObjects);
        }
        return os.str();
    }

    std::vector<std::string> SerializationFactory::loadObjectString(
        const std::string &xml,
        bool overwriteExisting) {

        std::vector<std::string> returnValue;

        try {
            std::istringstream xmlStream(xml);
            boost::archive::xml_iarchive ia(xmlStream);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
            register_in(ia, valueObjects);

            OH_REQUIRE(valueObjects.size(), "Object list is empty");

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            int count = 0;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                try {
                    returnValue.push_back(ProcessorFactory::getProcessor(*i)->process(*this, *i, overwriteExisting));
                    count++;
                } catch (const std::exception &e) {
                    OH_FAIL("Error processing item " << count << ": " << e.what());
                }
            }
            ProcessorFactory::postProcess();

        } catch (const std::exception &e) {
            OH_FAIL("Error deserializing xml : " << e.what());
        }

        OH_REQUIRE(!returnValue.empty(), "No objects loaded from xml");

        return returnValue;
    }

}

