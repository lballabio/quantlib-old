
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers

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

#include <ohxl/callingrange.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/functioncall.hpp>
#include <oh/objecthandler.hpp>
#include <oh/exception.hpp>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>

using std::string;

namespace ObjHandler {

    // FIXME this value needs to be configured at runtime
    const string libFunctionSignature = "ql";

    int CallingRange::keyCount_ = 0;

    CallingRange::CallingRange() : busy_(false), updateCount_(0), invocationCount_(0) {

        // name the calling range

        key_ = getKeyCount();
        const XLOPER *xCaller = FunctionCall::instance().getCallerReference();
        XLOPER xRet;
        Excel(xlfSetName, &xRet, 2, TempStrStl(key_), xCaller);
        if (xRet.xltype != xltypeBool || !xRet.val.boolean)
            throw Exception("error on call to xlfSetName");
    }

    CallingRange::~CallingRange() {

        // unname the calling range

        Excel(xlfSetName, 0, 1, TempStrStl(key_));
    }

    string CallingRange::getKeyCount() {
        static const int KEY_WIDTH = 5;
        static const int KEY_BASE = 16;
        static const double KEY_MAX = pow((double)KEY_BASE, KEY_WIDTH);

        if (keyCount_ > KEY_MAX)
            throw Exception("CallingRange::getKeyCount: max key value exceeded");
        std::ostringstream s;
        s << '_' << std::setw(KEY_WIDTH) << std::setfill('0') << std::setbase(KEY_BASE) << keyCount_++;
        return s.str();
    }

    typedef std::map<string, boost::shared_ptr<Object>, my_iless> OhRepository;

    void CallingRange::clearResidentObjects(bool deletePermanent) {
        if (deletePermanent) {
            for (OhRepository::iterator i = residentObjects_.begin();
                 i != residentObjects_.end();
                 ++i)
                ObjectHandler::instance().deleteObject(i->first);
            residentObjects_.clear();
        } else {
            OhRepository::iterator i = residentObjects_.begin();
            while (i != residentObjects_.end()) {
                if (i->second->permanent())
                    ++i;
                else {
                    ObjectHandler::instance().deleteObject(i->first);
                    // post-increment the iterator!!
                    residentObjects_.erase(i++);
                }
            }
        }
    }

    void CallingRange::deleteObject(const string &objectID,
                                    boost::shared_ptr<Object> object) {
        OhRepository::iterator i = residentObjects_.find(objectID);
        if (i != residentObjects_.end()) {
            ObjectHandler::instance().deleteObject(i->first);
            residentObjects_.erase(i);
        }
    }

    void CallingRange::registerObject(const string &objectID,
                                      boost::shared_ptr<Object> object) {
        residentObjects_[objectID] = object;
    }

    bool CallingRange::isValid() const {

        // XLOPERs which might need freeing in the event of an exception

        XLOPER xDef;
        XLOPER xRef;

        try {
        
            Excel(xlfGetName, &xDef, 1, TempStrStl(key_));

            string address;
            operToScalar(xDef, address);
            Excel(xlfTextref, &xRef, 1, TempStrStl(address.substr(1)));

            bool ret = (xRef.xltype & (xltypeRef | xltypeSRef)) != 0;
            Excel(xlFree, 0, 2, &xDef, &xRef);
            return ret;

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 2, &xDef, &xRef);

            // propagate the exception

            std::ostringstream err;
            err << "CallingRange::isValid: " << e.what();
            throw Exception(err.str());
        }

    }

    // FIXME cache this value
    string CallingRange::getAddressString() const {

        // XLOPER which might need freeing in the event of an exception

        XLOPER xDef;

        try {
        
            Excel(xlfGetName, &xDef, 1, TempStrStl(key_));

            string address;
            operToScalar(xDef, address);
            Excel(xlFree, 0, 1, &xDef);
            return address;

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 1, &xDef);

            // propagate the exception

            std::ostringstream err;
            err << "CallingRange::getAddressString: " << e.what();
            throw Exception(err.str());
        }

    }

    bool CallingRange::contains(const RangeReference &testReference) {
        if (!rangeReference_)
            rangeReference_ = boost::shared_ptr<RangeReference>(
                new RangeReference(getAddressString()));
        return rangeReference_->contains(testReference);
    }

    void CallingRange::setErrorMessage(const string &errorMessage, const bool &append) {
        if (append) {
            std::ostringstream err;
            err << errorMessage_ << std::endl << std::endl << errorMessage;
            errorMessage_ = err.str();
        } else {
            errorMessage_ = errorMessage;
        }
    }

    // FIXME use boost::regex here
    void CallingRange::setInvocationCount() {
        string formula = FunctionCall::instance().getFormula();

        // emergency/temporary fix/hack
        //OH_REQUIRE(formula.length() >= 3 && !my_iless()(formula.substr(0,3), "=ql"),
        //    "QuantLib functions cannot be nested inside non-QuantLib functions");

        invocationCount_ = -1;
        unsigned int index = 0;
        while (index < formula.length()) {
            index = formula.find(libFunctionSignature, index);
            if (index == string::npos) {
                return;
            } else {
                ++invocationCount_;
                ++index;
            }
        }
    }

    void CallingRange::update() {
        /*
        support for nested formulas e.g. "=qlFunc1(qlFunc2(), SUM(), qlFunc3(), xxx)"
        implement the following algorithm:
        - on first call to this formula - mark calling range as "busy" - clearNonPermanent()
        - intermediate calls - take no action
        - final call - mark calling range as "not busy"
        */
        if (busy_) {
            invocationCount_--;
            if (!invocationCount_)
                busy_ = false;
        } else {
            setInvocationCount();
            if (invocationCount_)
                busy_ = true;
            clearResidentObjects(false);
        }
    }

    string CallingRange::updateCount() {
        if (updateCount_ > 9999) updateCount_ = 0;
        std::ostringstream s;
        s << std::setw(4) << std::setfill('0') << updateCount_++;
        return s.str();
    }

    std::ostream& operator<<(std::ostream& out, const CallingRange &callingRange) {
        static const int COL_WIDTH = 20;
        out << std::setw(COL_WIDTH) << "name: " 
            << callingRange.getKey() << std::endl;
        out << std::setw(COL_WIDTH) << "reference: " 
            << callingRange.getAddressString() << std::endl;
        out << std::setw(COL_WIDTH) << "error: " 
            << callingRange.errorMessage() << std::endl;
        out << std::setw(COL_WIDTH) << "valid: " 
            << std::setiosflags(std::ios_base::boolalpha) 
            << callingRange.isValid() << std::endl;
        out << std::setw(COL_WIDTH) << "busy: " 
            << std::setiosflags(std::ios_base::boolalpha) 
            << callingRange.busy_ << std::endl;
        out << std::setw(COL_WIDTH) << "update count: " 
            << callingRange.updateCount_ << std::endl;
        out << std::setw(COL_WIDTH) << "invocation count: " 
            << callingRange.invocationCount_ << std::endl;
        out << std::setw(COL_WIDTH) << "resident objects: " 
            << callingRange.residentObjects_.size() << std::endl;
        OhRepository::const_iterator i;
        for (i = callingRange.residentObjects_.begin(); 
             i != callingRange.residentObjects_.end();
             ++i)
            out << std::setw(COL_WIDTH) << "       object ID: " 
                << i->first << std::endl;
        out << std::endl;
        return out;
    }

}
