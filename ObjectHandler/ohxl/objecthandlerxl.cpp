
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#include <oh/exception.hpp>
#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/conversions.hpp>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>
#include <boost/regex.hpp>

const boost::regex NAME_REGEX(".*~_[\\da-f]{5}");

namespace ObjHandler {

    void ObjectHandlerXL::collectGarbage() {
        ObjectList::iterator iter_current, iter_previous;
        iter_current = objectList_.begin();
        while (iter_current != objectList_.end()) {
            iter_previous = iter_current;
            iter_current++;
            std::string nameFull = iter_previous->first;
            obj_ptr object = iter_previous->second;
            if (!object->isValid())
                objectList_.erase(nameFull);
        }
    }

    void ObjectHandlerXL::deleteKey(const std::string &key) {
        for (ObjectList::iterator iter = objectList_.begin();
                iter != objectList_.end(); iter++) {
            std::string nameFull = iter->first;
            obj_ptr object = iter->second;
            if (object->getKey() == key) {
                objectList_.erase(nameFull);
                break;
            }
        }
    }

    bool ObjectHandlerXL::nameIsFull(const std::string &name) const {
       return regex_match(name, NAME_REGEX);
    }

    obj_ptr ObjectHandlerXL::retrieveObjectNameStub(const std::string &nameStub) const {
        bool objectFound = false;
        std::string nameFullFound;
        obj_ptr retrievedObject;
        ObjectList::const_iterator i;
        for (i=objectList_.begin(); i!=objectList_.end(); i++) {
            std::string nameFull = i->first;
            obj_ptr object = i->second;
            if (nameStub == object->getStubName()) {
                if (objectFound) {
                    std::ostringstream msg;
                    msg << "error retrieving object with instance name '" << nameStub
                        << "': two (& maybe more) objects exist with that instance name -"
                        << " #1: '" << nameFullFound
                        << "' #2: '" << nameFull << "'";
                    throw Exception(msg.str());
                } else {
                    retrievedObject = object;
                    objectFound = true;
                    nameFullFound = nameFull;
                }
            }
        }
        if (i != objectList_.end()) {
            std::ostringstream msg;
            msg << "ObjectHandler error: attempt to retrieve object "
                "with unknown instance name '" << nameStub << "'";
            throw Exception(msg.str());
        } else
            return retrievedObject;
    }

    obj_ptr ObjectHandlerXL::retrieveObject(const std::string &name) const {
        if (nameIsFull(name))
            return ObjectHandlerBase::retrieveObject(name);
        else
            return retrieveObjectNameStub(name);
    }

}
