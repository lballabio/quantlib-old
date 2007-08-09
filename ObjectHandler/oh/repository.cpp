
/*
 Copyright (C) 2005, 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <oh/repository.hpp>
#include <oh/serializationfactory.hpp>
#include <oh/exception.hpp>
#include <boost/regex.hpp>
#include <ostream>
#include <sstream>

namespace ObjectHandler {

    Repository *Repository::instance_;

    // std::map cannot be exported across DLL boundaries
    // so instead we use a static variable.
    Repository::ObjectMap objectMap_;

    Repository::Repository() {
        instance_ = this;
    }

    Repository::~Repository() {
        instance_ = 0;
    }

    Repository &Repository::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized Repository object");
        return *instance_;
    }

    // TODO: implement Scott Meyers' "Effective STL" item 24
    std::string Repository::storeObject(
        const std::string &objectID,
        const boost::shared_ptr<Object> &object) {

        objectMap_[objectID] = object;
        return objectID;
    }

    void Repository::retrieveObject(boost::shared_ptr<Object> &ret,
                        const std::string &id) {
        ret = retrieveObjectImpl(id);
    }

    boost::shared_ptr<Object> Repository::retrieveObjectImpl(
        const std::string &objectID) const {

        ObjectMap::const_iterator result = objectMap_.find(objectID);
        OH_REQUIRE(result != objectMap_.end(), "ObjectHandler error: attempt to retrieve object "
            "with unknown ID '" << objectID << "'");
        return result->second;
    }

    void Repository::deleteObject(const std::string &objectID) {
        objectMap_.erase(objectID);
    }

    void Repository::deleteAllObjects(const bool &deletePermanent) {

        if (deletePermanent) {
            objectMap_.clear();
        } else {
            ObjectMap::iterator i = objectMap_.begin();
            while (i != objectMap_.end()) {
                if (i->second->permanent())
                    ++i;
                else
                    objectMap_.erase(i++);
            }
        }
    }

    void Repository::dump(std::ostream& out) {

        out << "dump of all objects in ObjectHandler:" <<
            std::endl << std::endl;
        for (ObjectMap::const_iterator i=objectMap_.begin();
             i!=objectMap_.end(); ++i) {
            boost::shared_ptr<Object> object = i->second;
            out << "Object with ID = " << i->first << ":" << std::endl << object;
        }
    }

    void Repository::dumpObject(const std::string &objectID, std::ostream &out) {

        ObjectMap::const_iterator result = objectMap_.find(objectID);
        if (result == objectMap_.end()) {
            out << "no object in repository with ID = " << objectID << std::endl;
        } else {
            out << "log dump of object with ID = " << objectID << std::endl << result->second;
        }
    }

    int Repository::objectCount() {
        return objectMap_.size();
    }

    const std::vector<std::string> Repository::listObjectIDs(
        const std::string &regex) {

        std::vector<std::string> objectIDs;
        if (regex.empty()) {
            objectIDs.reserve(objectMap_.size());
            for (ObjectMap::const_iterator i=objectMap_.begin();
                i!=objectMap_.end(); ++i)
                objectIDs.push_back(i->first);
        } else {
            boost::regex r(regex, boost::regex::perl | boost::regex::icase);
            for (ObjectMap::const_iterator i=objectMap_.begin();
                i!=objectMap_.end(); ++i) {
                std::string objectID = i->first;
                if (regex_match(objectID, r)) objectIDs.push_back(objectID);
            }
        }
        return objectIDs;
    }

	int Repository::saveObject(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> > &objectList,
        const std::string &path,
        bool forceOverwrite) {

		return ObjectHandler::SerializationFactory::instance().saveObject(
            objectList, path.c_str(), forceOverwrite);   
	}	

	int Repository::loadObject(
        const std::string &path,
        bool overwriteExisting) {

        return ObjectHandler::SerializationFactory::instance().loadObject(
            path.c_str(), overwriteExisting);			
    }


}
