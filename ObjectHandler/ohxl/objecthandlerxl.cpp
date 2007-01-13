
/*
 Copyright (C) 2007 Ferdinando Ametrano
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
#include <oh/utilities.hpp>
#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/callingrange.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/functioncall.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <xlsdk/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <iomanip>
#include <sstream>
#include <limits.h>

using std::string;

namespace ObjHandler {

    unsigned long ObjectHandlerXL::objectIDCount_ = 0;

    // std::map cannot be exported across DLL boundaries
    // so instead we use a static variable
    typedef std::map<string, boost::shared_ptr<CallingRange> > rangeMap;
    rangeMap callingRanges_;

    ObjectHandlerXL &ObjectHandlerXL::instance() {
        if (instance_) {
            ObjectHandlerXL *ret = dynamic_cast<ObjectHandlerXL*>(instance_);
            if (ret) return *ret;
        }
        throw Exception("Attempt to reference uninitialized ObjectHandlerXL object");
    }

    boost::shared_ptr<CallingRange> ObjectHandlerXL::getCallingRange(bool createIfNone) {

        // XLOPER which might need freeing in the event of an exception

        XLOPER xOldName;

        try {
            boost::shared_ptr<CallingRange> callingRange;

            // get name if any

            Excel(xlfGetDef, &xOldName, 1, FunctionCall::instance().getCallerAddress());

            // if name - return associated CallingRange object

            if (xOldName.xltype == xltypeStr) {
                string oldKey;
                operToScalar(xOldName, oldKey);
                Excel(xlFree, 0, 1, &xOldName);
                rangeMap::const_iterator i = callingRanges_.find(oldKey);
                if (i == callingRanges_.end()) {
                    std::ostringstream msg;
                    msg << "no calling range named " << oldKey;
                    throw Exception(msg.str());
                } else {
                    callingRange = i->second;
                }
            } else {
                if (createIfNone) {
                    callingRange = boost::shared_ptr<CallingRange>(new CallingRange);
                    callingRanges_[callingRange->getKey()] = callingRange;
                }
            }

            return callingRange;

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 1, &xOldName);

            // propagate the exception

            std::ostringstream err;
            err << "ObjectHandlerXL::getCallingRange(): " << e.what();
            throw Exception(err.str());
        }

    }

    void ObjectHandlerXL::resetCaller(const bool &createIfNone) {
        if (FunctionCall::instance().getCallerType() != Cell)
            return;
        boost::shared_ptr<CallingRange> callingRange = getCallingRange(createIfNone);
        if (callingRange) callingRange->update();
    }

    string ObjectHandlerXL::storeObject(
            const string &objectIDOrig,
            const boost::shared_ptr<Object> &object) {

        string objectID;
        if (objectIDOrig.empty()) {
            objectID = generateObjectID();
            object->setAnonymous();
        } else {
            objectID = objectIDOrig;
        }

        if (FunctionCall::instance().getCallerType() == Cell) {
            boost::shared_ptr<CallingRange> callingRange = 
                getCallingRange();
            callingRange->deleteObject(objectID, object);
            checkName(objectID);
            callingRange->registerObject(objectID, object);

            string objectIDCounter = 
                ObjectHandler::storeObject(objectID, object);
            return objectIDCounter + "#" + 
                callingRange->updateCount();
        } else {
            ObjectHandler::storeObject(objectID, object);
            return objectID;
        }
    }
    string ObjectHandlerXL::getStub(const string &objectID) const {
        int counterOffset = objectID.length() - 5;
        if (counterOffset >= 0 && objectID[counterOffset] == '#')
            return objectID.substr(0, counterOffset);
        else
            return objectID;
    }

    string ObjectHandlerXL::generateObjectID() {
        /*
        The user has provided a null string as the object ID so generate a name automatically.
        The name is in the format obj_XXXXX where XXXXX is a hexidecimal value which is COUNT_WIDTH
        digits long until the value exceeds COUNT_BASE^COUNT_WIDTH at which point the length grows 
        as necessary until ULONG_MAX is hit and an exception is thrown.
        */
        static const int COUNT_WIDTH = 5;
        static const int COUNT_BASE = 16;

        if (objectIDCount_ == ULONG_MAX) {
            std::ostringstream msg;
            msg << "ObjectHandler has exceeded the maximum limit of " << ULONG_MAX <<
                " names that can be automatically generated for 'anonymous objects'"
                " (objects where the user has supplied an empty string as an ID)."
                " Please invoke ohDeleteAllObjects(TRUE) or restart the application.";
            throw Exception(msg.str());
        }
        std::ostringstream s;
        s << "obj_" << std::setw(COUNT_WIDTH) << std::setfill('0') << std::setbase(COUNT_BASE) << objectIDCount_++;
        return s.str();
    }

    void ObjectHandlerXL::collectGarbage(const bool &deletePermanent) {
        rangeMap::iterator i = callingRanges_.begin();
        while (i != callingRanges_.end()) {
            boost::shared_ptr<CallingRange> callingRange = i->second;
            if (callingRange->isValid())
                ++i;
            else {
                callingRange->clearResidentObjects(deletePermanent);
                if (callingRange->empty())
                    // post-increment the iterator!!
                    callingRanges_.erase(i++);
                else
                    ++i;
            }
        }
    }

    void ObjectHandlerXL::deleteObject(const string &objectID) {
        ObjectHandler::deleteObject(getStub(objectID));
    }

    void ObjectHandlerXL::deleteAllObjects(const bool &deletePermanent) {
        if (deletePermanent) {
            callingRanges_.clear();
            objectIDCount_ = 0;
        }
        ObjectHandler::deleteAllObjects(deletePermanent);
    }

    void ObjectHandlerXL::dump(std::ostream& out) {
        ObjectHandler::dump(out);

        out << std::endl << "calling ranges:";
        if (callingRanges_.empty()) {
            out << " none." << std::endl;
        } else {
            out << std::endl << std::endl;
            for (rangeMap::const_iterator i = callingRanges_.begin();
                 i != callingRanges_.end();
                 ++i) {
                boost::shared_ptr<CallingRange> callingRange = i->second;
                out << *callingRange.get();
            }
        }
    }

    void ObjectHandlerXL::logError(const string& message, const bool &append) {
        std::ostringstream msgLog;
        if (FunctionCall::instance().getCallerType() == Cell) {
            FunctionCall::instance().setError();
            std::ostringstream msgCell;
            msgCell << FunctionCall::instance().getFunctionName() << " - " << message;
            boost::shared_ptr<CallingRange> callingRange = getCallingRange(true);
            callingRange->setErrorMessage(msgCell.str(), append);
            msgLog << FunctionCall::instance().getAddressString() << " - ";
        }
        msgLog << FunctionCall::instance().getFunctionName() << " - " << message;
        logMessage(msgLog.str(), 2);
    }

    string ObjectHandlerXL::retrieveError(const XLOPER *xRangeRef) {

        // XLOPER which might need freeing in the event of an exception

        XLOPER xRangeText;

        try {
            if (xRangeRef->xltype != xltypeRef && xRangeRef->xltype != xltypeSRef) {
                throw Exception("input parameter is not a range reference.");
            }
            Excel(xlfReftext, &xRangeText, 1, xRangeRef);
            string rangeStr;
            operToScalar(xRangeText, rangeStr);
            Excel(xlFree, 0, 1, &xRangeText);
            RangeReference selectionReference(rangeStr);

            for (rangeMap::const_iterator i = callingRanges_.begin();
                 i != callingRanges_.end();
                 ++i) {
                boost::shared_ptr<CallingRange> callingRange = i->second;
                if (callingRange->contains(selectionReference))
                    return callingRange->errorMessage();
            }
            return "";

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 1, &xRangeText);

            // propagate the exception

            std::ostringstream err;
            err << "ObjectHandlerXL::retrieveError(): " << e.what();
            throw Exception(err.str());
        }
    }

    void ObjectHandlerXL::clearError() {
        boost::shared_ptr<CallingRange> callingRange = getCallingRange();
        if (callingRange) callingRange->clearError();
    }

}
