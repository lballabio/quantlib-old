
/*
 Copyright (C) 2005 Ferdinando Ametrano
 Copyright (C) 2004, 2005 Eric Ehlers

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
#include <ostream>

namespace ObjHandler {

    void ObjectHandler::storeObject(const std::string &handle,
                                    const obj_ptr &object) {
        objectList_[handle] = object;
    }

    obj_ptr ObjectHandler::retrieveObject(const std::string &handle) const {
        ObjectList::const_iterator result = objectList_.find(handle);
        if (result!=objectList_.end())
            return result->second;
        else
            return obj_ptr();
    }

    void ObjectHandler::deleteObject(const std::string &handle) {
        objectList_.erase(handle);
    }

    void ObjectHandler::deleteAllObjects() {
        objectList_.clear();
    }

/*
    std::vector < std::string >ObjectHandler::getHandles() {
        std::vector < std::string >ret;
        for (ObjectList::const_iterator i=objectList_.begin();
                i!=objectList_.end(); i++)
            ret.push_back(i->first);
        return ret;
    }
*/

    void ObjectHandler::dump(std::ostream& out) {
        out << "dump of all objects in ObjectHandler:" << std::endl;
        for (ObjectList::const_iterator i=objectList_.begin();
            i!=objectList_.end(); i++) {
            obj_ptr object = i->second;
            out << "Object with handle = " << i->first << ":" << std::endl << *object.get();
        }
    }

}

