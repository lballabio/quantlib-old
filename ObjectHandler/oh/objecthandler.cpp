
/*
 Copyright (C) 2005, 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <oh/config.hpp>
#endif

#include <oh/objecthandler.hpp>
#include <oh/exception.hpp>
#include <oh/iless.hpp>
#include <boost/regex.hpp>
#include <ostream>
#include <sstream>
#include <algorithm>
#include <map>

using std::string;

namespace ObjHandler {

    ObjectHandler *ObjectHandler::instance_;

    // std::map cannot be exported across DLL boundaries
    // so instead we use a static variables

    // Map object ID to object reference
    // the iless comparison functor class allows "case-preserving" behavior:
    //      storeObject("MyObject")         // store "MyObject"
    //      retrieveObject("MyObJeCt")      // retrieve "MyObject"
    //      storeObject("MYOBJECT")         // overwrite "MyObject"
    typedef std::map<string, boost::shared_ptr<Object>, my_iless> OhRepository;
    OhRepository repository_;

    ObjectHandler::ObjectHandler() {
        instance_ = this;
    }

    ObjectHandler::~ObjectHandler() {
        instance_ = 0;
    }

    ObjectHandler &ObjectHandler::instance() {
        if (instance_)
            return *instance_;
        else
            throw Exception("Attempt to reference uninitialized ObjectHandler object");
    }
 
    // TODO: implement Scott Meyers' "Effective STL" item 24
    string ObjectHandler::storeObject(
                                const string &objectID, 
                                const boost::shared_ptr<Object> &object) {
        repository_[objectID] = object;
        return objectID;
    }

    boost::shared_ptr<Object> ObjectHandler::retrieveObjectImpl(
            const string &objectID) const {
        OhRepository::const_iterator result = repository_.find(objectID);
        if (result != repository_.end())
            return result->second;
        else {
            std::ostringstream msg;
            msg << "ObjectHandler error: attempt to retrieve object with "
                "unknown ID '" << objectID << "'";
            throw Exception(msg.str());
        }
    }

    void ObjectHandler::deleteObject(const string &objectID) {
        repository_.erase(objectID);
    }

    void ObjectHandler::deleteAllObjects(const bool &deletePermanent) {
        if (deletePermanent)
            repository_.clear();
        else {
            OhRepository::iterator i = repository_.begin();
            while (i != repository_.end()) {
                if (!i->second->permanent())
                    // post-increment the iterator!!
                    repository_.erase(i++);
                else
                    ++i;
            }
        }
    }

    void ObjectHandler::dump(std::ostream& out) {
        out << "dump of all objects in ObjectHandler:" <<
            std::endl << std::endl;
        for (OhRepository::const_iterator i=repository_.begin();
             i!=repository_.end(); ++i) {
            boost::shared_ptr<Object> object = i->second;
            out << "Object with ID = " << i->first << ":" <<
                std::endl << *object.get();
        }
    }

    const int ObjectHandler::objectCount() {
        return repository_.size();
    }

    const std::vector<string> ObjectHandler::listObjectIDs(
            const string &regex) {
        std::vector<string> objectIDs;
        if (regex.empty()) {
            objectIDs.reserve(repository_.size());
            for (OhRepository::const_iterator i=repository_.begin();
                 i!=repository_.end();
                 ++i)
                objectIDs.push_back(i->first);
        } else {
            boost::regex r(regex, boost::regex::perl | boost::regex::icase);
            for (OhRepository::const_iterator i=repository_.begin();
                 i!=repository_.end();
                 ++i) {
                string objectID = i->first;
                if (regex_match(objectID, r)) objectIDs.push_back(objectID);
            }
        }
        return objectIDs;
    }

    void ObjectHandler::checkName(const string &objectID) {
        OhRepository::const_iterator result = repository_.find(objectID);
        if (result != repository_.end()) {
            std::ostringstream msg;
            msg << "ObjectHandler error: cannot create object with ID '" <<
                objectID << "' because an object with the equivalent name '" <<
                result->first << "' already exists";
            throw Exception(msg.str());
        }
    }

}
