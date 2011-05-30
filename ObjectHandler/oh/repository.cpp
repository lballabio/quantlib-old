/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006, 2007, 2008 Eric Ehlers
 Copyright (C) 2005, 2006, 2007 Ferdinando Ametrano
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

#include <oh/repository.hpp>
#include <oh/serializationfactory.hpp>
#include <oh/exception.hpp>
#include <oh/group.hpp>
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
        const boost::shared_ptr<Object> &object,
        bool overwrite) {

            OH_REQUIRE(overwrite || !objectExists(objectID), "Cannot store object with ID '"
                << objectID << "' because an object with that ID already exists");


            if(objectExists(objectID)){
                ObjectMap::const_iterator result = objectMap_.find(objectID);
                //result->second->notifyObservers();
                result->second->reset(object);

            }
            else{
                boost::shared_ptr<ObjectWrapper> objWrapper(
                    new ObjectWrapper(object));

                objectMap_[objectID] = objWrapper;
            }

            registerObserver(objectMap_[objectID]);

            return objectID;
    }

    void Repository::retrieveObject(boost::shared_ptr<Object> &ret,
        const std::string &id) {

            ret = retrieveObjectImpl(id);
    }

    boost::shared_ptr<Object> Repository::retrieveObjectImpl(
        const std::string &objectID) {

            ObjectMap::const_iterator result = objectMap_.find(formatID(objectID));
            OH_REQUIRE(result != objectMap_.end(), "ObjectHandler error: attempt to retrieve object "
                "with unknown ID '" << objectID << "'");
            if(result->second->dirty()){
                result->second->recreate();
            }
            return result->second->object();
    }

    const boost::shared_ptr<ObjectWrapper>& Repository::getObjectWrapper(const std::string &objectID) const{
        ObjectMap::const_iterator result = objectMap_.find(objectID);
        OH_REQUIRE(result != objectMap_.end(), "ObjectHandler error: attempt to retrieve object "
            "with unknown ID '" << objectID << "'");

        return result->second;
    }

    void Repository::registerObserver( 
        boost::shared_ptr<ObjectWrapper> objWrapper){

            const std::set<std::string>& relationObs = objWrapper->object()->properties()->getPrecedentObjects();
            std::set<std::string>::const_iterator iter = relationObs.begin();
            for(; iter != relationObs.end();  iter++){
                boost::shared_ptr<ObjectWrapper> objServable = getObjectWrapper(formatID(*iter));
                objWrapper->registerWith(objServable);

            }
    }

    void Repository::deleteObject(const std::string &objectID) {
        std::string realID = formatID(objectID);
        OH_REQUIRE(objectExists(realID), "Cannot delete Object with ID '" << realID
            << "' because no Object with that ID is present in the Repository");
        objectMap_.erase(realID);
    }

    void Repository::deleteObject(const std::vector<std::string> &objectIDs) {
        OH_REQUIRE(!objectIDs.empty(), "List of Object IDs for deletion is empty");
        for (std::vector<std::string>::const_iterator i = objectIDs.begin();
            i != objectIDs.end(); ++i)
            deleteObject(*i);
    }

    void Repository::deleteAllObjects(const bool &deletePermanent) {

        if (deletePermanent) {
            objectMap_.clear();
        } else {
            ObjectMap::iterator i = objectMap_.begin();
            while (i != objectMap_.end()) {
                if (i->second->object()->permanent())
                    ++i;
                else{
                    objectMap_.erase(i++);                    
                }
            }
        }
    }

    void Repository::dump(std::ostream& out) {

        out << "dump of all objects in ObjectHandler:" <<
            std::endl << std::endl;
        for (ObjectMap::const_iterator i=objectMap_.begin();
            i!=objectMap_.end(); ++i) {
                boost::shared_ptr<Object> object = i->second->object();
                out << "Object with ID = " << i->first << ":" << std::endl << object;
        }
    }

    void Repository::dumpObject(const std::string &objectID, std::ostream &out) {

        std::string realID = formatID(objectID);
        ObjectMap::const_iterator result = objectMap_.find(realID);
        if (result == objectMap_.end()) {
            out << "no object in repository with ID = " << realID << std::endl;
        } else {
            out << "log dump of object with ID = " << realID << std::endl << result->second;
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

    bool Repository::objectExists(const std::string &objectID) const {
        return objectMap_.find(objectID) != objectMap_.end();
    }

    std::vector<bool> Repository::objectExists(const std::vector<std::string> &objectList) {
        std::vector<bool> ret;

        for (std::vector<std::string>::const_iterator i = objectList.begin();
            i != objectList.end(); ++i) {

                ret.push_back(objectExists(formatID(*i)));
        }

        return ret;
    }

    std::vector<double> Repository::creationTime(const std::vector<std::string> &objectList)  {
        std::vector<double> ret;

        for (std::vector<std::string>::const_iterator i = objectList.begin();
            i != objectList.end(); ++i) {

                std::string realID = formatID(*i);
                if(objectExists(realID)){
                    ObjectMap::const_iterator result = objectMap_.find(realID);
                    ret.push_back(result->second->creationTime());
                } else {
                    OH_FAIL("Unable to retrieve object with ID "<<*i);
                }
        }
        return ret;
    }

    std::vector<double> Repository::updateTime(const std::vector<std::string> &objectList) {
        std::vector<double> ret;

        for (std::vector<std::string>::const_iterator i = objectList.begin();
            i != objectList.end(); ++i) {

                std::string realID = formatID(*i);
                if (objectExists(realID)){
                    ObjectMap::const_iterator result = objectMap_.find(realID);
                    ret.push_back( result->second->updateTime());
                } else {
                    OH_FAIL("Unable to retrieve object with ID "<<*i);
                }
        }
        return ret;
    }

    const std::vector<std::string> Repository::precedentIDs(const std::string &objectID) {
        std::string realID = formatID(objectID);
        if (objectExists(realID)){
			ObjectMap::const_iterator result = objectMap_.find(realID);

			boost::shared_ptr<Object> object = result->second->object();
			boost::shared_ptr<Group> group = boost::dynamic_pointer_cast<Group>(object);

			if(group)
				return precedentIDs(group);

			const std::set<std::string>& relationObs = object->properties()->getPrecedentObjects();
			std::vector<std::string> vecRelationObs;
			std::set<std::string>::const_iterator it = relationObs.begin();
			for(; it != relationObs.end(); ++it){
				vecRelationObs.push_back(formatID(*it));
			}
			return vecRelationObs;
        } else {
            OH_FAIL( "Unable to retrieve object with ID "<<objectID);
        }
    }

	const std::vector<std::string> Repository::precedentIDs(const boost::shared_ptr<Group>& group) {
		std::vector<std::string> ret;
		std::vector<std::string>::const_iterator i = group->list().begin();
		for(; i != group->list().end(); ++i)
			if(objectExists(*i))
				ret.push_back(*i);

		return ret;
	}

    std::vector<bool> Repository::isPermanent(const std::vector<std::string> &objectList){
        std::vector<bool> ret;

        for (std::vector<std::string>::const_iterator i = objectList.begin();
            i != objectList.end(); ++i) {

                std::string realID = formatID(*i);
                if (objectExists(realID)){
                    ObjectMap::const_iterator result = objectMap_.find(realID);

                    ret.push_back(result->second->object()->permanent());

                } else {
                    OH_FAIL("Unable to retrieve object with ID "<<*i);
                }
        }
        return ret;
    }

    const std::vector<std::string> Repository::className(const std::vector<std::string> &objectList){
        std::vector<std::string> ret;

        for (std::vector<std::string>::const_iterator i = objectList.begin();
            i != objectList.end(); ++i) {
                std::string realID = formatID(*i);
                if (objectExists(realID)){

                    ObjectMap::const_iterator result = objectMap_.find(realID);

                    ret.push_back(result->second->object()->properties()->className());

                } else {
                    OH_FAIL("Unable to retrieve object with ID "<<*i);
                }
        }
        return ret;
    }

    std::string Repository::formatID(const std::string &objectID){
        return objectID;
    }

}

