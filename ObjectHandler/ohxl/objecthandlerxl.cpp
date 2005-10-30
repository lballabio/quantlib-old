
/*
 Copyright (C) 2005 Eric Ehlers

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

#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/conversions.hpp>
#include <oh/exception.hpp>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>

const int KEY_WIDTH = 5;
const int KEY_BASE = 16;
const int KEY_MAX = pow((double)KEY_BASE, KEY_WIDTH);
const char KEY_DELIMITER = '~';

namespace ObjHandler {

    const std::string ObjectHandlerXL::getKey() {
        if (key_ > KEY_MAX)
            throw std::exception("ObjectHandlerXL::getKey: max key value exceeded");
        std::ostringstream s;
        s << '_' << std::setw(KEY_WIDTH) << std::setfill('0') << std::setbase(KEY_BASE) << key_++;
        return s.str();
    }

    const std::string ObjectHandlerXL::parseHandle(const std::string &handle) {
        const int delimOffset = handle.find(KEY_DELIMITER);
        if (delimOffset == std::string::npos) {
            std::ostringstream err;
            err << "key delimiter character '" << KEY_DELIMITER
                << "' not found in handle " << handle;
            throw std::exception(err.str().c_str());
        }
        const int keyLength = KEY_WIDTH + 2;
        if (delimOffset != handle.length() - keyLength) {
            std::ostringstream err;
            err << "expected to find key delimiter character " 
                << KEY_DELIMITER << " " << keyLength 
                << " characters before end of handle " << handle;
            throw std::exception(err.str().c_str());
        }
        const std::string key = handle.substr(delimOffset + 1, keyLength);
        return key;
    }

    const std::string ObjectHandlerXL::storeObject(
            const std::string &handleStub,
            const obj_ptr &object) {

        if (!gcEnabled_) {
            objectList_[handleStub] = object;
            return handleStub;
        }

        // XLOPERs which might need freeing in the event of an exception

        XLOPER xCaller;
        XLOPER xReftext;
        XLOPER xOldName;

        try {

            // validate stub

            if (handleStub.find(KEY_DELIMITER) != std::string::npos)
                throw std::exception("object handle must not contain " + KEY_DELIMITER);

            // obtain reference to calling cell

            Excel(xlfCaller, &xCaller, 0);

            // convert reference to full address

            Excel(xlfReftext, &xReftext, 1, &xCaller);

            // get name if any

            Excel(xlfGetDef, &xOldName, 1, &xReftext);
            Excel(xlFree, 0, 1, &xReftext);

            // if name - delete old name, old object

            if (xOldName.xltype == xltypeStr) {

                // delete name

                Excel(xlfSetName, 0, 1, &xOldName);

                // delete object

                std::string oldKey = operToScalarString(&xOldName);
                Excel(xlFree, 0, 1, &xOldName);
                for (ObjectList::iterator iter = objectList_.begin();
                        iter != objectList_.end(); iter++) {
                    std::string handle = iter->first;
                    if (handle.substr(handle.length() - KEY_WIDTH - 1) == oldKey) {
                        objectList_.erase(handle);
                        break;
                    }
                }

            }

            // derive handle

            const std::string key = getKey();
            const std::string handle = handleStub + KEY_DELIMITER + key;

            // name the calling cell

            XLOPER xRet;
            Excel(xlfSetName, &xRet, 2, TempStrStl(key), &xCaller);
            Excel(xlFree, 0, 1, &xCaller);
            if (xRet.xltype != xltypeBool || !xRet.val.boolean)
                throw std::exception("error on call to xlfSetName");

            // store the object

            objectList_[handle] = object;

            // return modified handle

            return handle;

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 3, &xCaller, &xReftext, &xOldName);

            // propagate the exception

            std::ostringstream err;
            err << "ObjectHandlerXL::storeObject: " << e.what();
            throw std::exception(err.str().c_str());
        }

    }

    void ObjectHandlerXL::deleteName(const std::string &handle) {
        try {
            const std::string key = parseHandle(handle);
            Excel(xlfSetName, 0, 1, TempStrStl(key));
        } catch (const std::exception &e) {
            std::ostringstream err;
            err << "ObjectHandlerXL::deleteObject: " << e.what();
            throw std::exception(err.str().c_str());
        }    
    }

    void ObjectHandlerXL::deleteObject(const std::string &handle) {
        if (gcEnabled_)
            deleteName(handle);
        objectList_.erase(handle);
    }

    void ObjectHandlerXL::deleteAllObjects() {
        if (gcEnabled_)
            for (ObjectList::iterator iter = objectList_.begin();
                    iter != objectList_.end(); iter++)
                deleteName(iter->first);
        objectList_.clear();
        key_ = 0;
    }

    bool ObjectHandlerXL::nameIsValid(const std::string &handle) {

        // XLOPERs which might need freeing in the event of an exception

        XLOPER xDef;
        XLOPER xRef;

        try {
        
            std::string key = handle.substr(handle.length() - KEY_WIDTH - 1);
            Excel(xlfGetName, &xDef, 1, TempStrStl(key));

            std::string address = operToScalarString(&xDef);
            Excel(xlfTextref, &xRef, 1, TempStrStl(address.substr(1)));

            bool ret = (xRef.xltype & (xltypeRef | xltypeSRef)) != 0;
            Excel(xlFree, 0, 2, &xDef, &xRef);
            return ret;

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 2, &xDef, &xRef);

            // propagate the exception

            std::ostringstream err;
            err << "ObjectHandlerXL::nameIsValid: " << e.what();
            throw std::exception(err.str().c_str());
        }

    }

    void ObjectHandlerXL::collectGarbage() {
        if (!gcEnabled_)
            throw Exception("can't run garbage collector because "
                "garbage collection is disabled");

        ObjectList::iterator iter_current, iter_previous;
        iter_current = objectList_.begin();
        while (iter_current != objectList_.end()) {
            iter_previous = iter_current;
            iter_current++;
            if (!nameIsValid(iter_previous->first))
                objectList_.erase(iter_previous->first);
        }

    }

    void ObjectHandlerXL::setGcEnabled(const bool &newValue) {
        if (gcEnabled_ != newValue) {
            deleteAllObjects();
            gcEnabled_ = newValue;
        }
    }

    const bool &ObjectHandlerXL::getGcEnabled() {
        return gcEnabled_;
    }

}

