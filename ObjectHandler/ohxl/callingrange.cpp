
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006, 2007 Eric Ehlers

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
#include <oh/exception.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/repositoryxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/xloper.hpp>
#include <iomanip>
#include <sstream>
#include <cmath>

namespace ObjectHandler {

    int CallingRange::keyCount_ = 0;
    const int CallingRange::KEY_WIDTH = 5;

    CallingRange::CallingRange() : key_(getKeyCount()), updateCount_(0) {

        // name the calling range

        XLOPER xRet;
        Excel(xlfSetName, &xRet, 2, TempStrStl(key_), FunctionCall::instance().callerReference());
        OH_REQUIRE(xRet.xltype == xltypeBool && xRet.val.boolean, "Error on call to xlfSetName");
    }

    CallingRange::~CallingRange() {

        // unname the calling range

        //Excel(xlfSetName, 0, 1, TempStrStl(key_));
    }

    std::string CallingRange::getKeyCount() {
        static const int KEY_BASE = 16;
        static const double KEY_MAX = pow((double)KEY_BASE, KEY_WIDTH);

        OH_REQUIRE(keyCount_ < KEY_MAX, "CallingRange::getKeyCount() : max key value exceeded");
        std::ostringstream s;
        s << '_' << std::setw(KEY_WIDTH) << std::setfill('0') << std::setbase(KEY_BASE) << keyCount_++;
        return s.str();
    }

    void CallingRange::clearResidentObjects(bool deletePermanent) {
        if (deletePermanent) {
            for (ObjectXLMap::iterator i = residentObjects_.begin();
                    i != residentObjects_.end(); ++i)
                Repository::instance().deleteObject(i->first);
            residentObjects_.clear();
        } else {
            ObjectXLMap::iterator i = residentObjects_.begin();
            while (i != residentObjects_.end()) {
                if (i->second->permanent()) {
                    ++i;
                } else {
                    Repository::instance().deleteObject(i->first);
                    residentObjects_.erase(i++);
                }
            }
        }
    }

    void CallingRange::registerObject(boost::shared_ptr<ObjectXL> objectXL) {
        residentObjects_[objectXL->id()] = objectXL;
    }

    bool CallingRange::valid() const {

        Xloper xDef, xRef;
        
        Excel(xlfGetName, &xDef, 1, TempStrStl(key_));

        std::string address;
        operToScalar(xDef(), address);
        Excel(xlfTextref, &xRef, 1, TempStrStl(address.substr(1)));

        bool ret = (xRef->xltype & (xltypeRef | xltypeSRef)) != 0;
        return ret;

    }

    std::string CallingRange::addressString() const {
        Xloper xDef;
        Excel(xlfGetName, &xDef, 1, TempStrStl(key_));
        std::string address;
        operToScalar(xDef(), address);
        return address;
    }

    std::string CallingRange::updateCount() {
        if (updateCount_ > 9999) updateCount_ = 0;
        std::ostringstream s;
        s << std::setw(4) << std::setfill('0') << updateCount_++;
        return s.str();
    }

    std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<CallingRange> &callingRange) {
        static const int COL_WIDTH = 20;
        out << "name: " << std::left << std::setw(COL_WIDTH)
            << callingRange->key() << std::endl;
        out << "reference: " << std::left << std::setw(COL_WIDTH) 
            << callingRange->addressString() << std::endl;
        out << "valid: "  << std::left << std::setw(COL_WIDTH)
            << std::setiosflags(std::ios_base::boolalpha) 
            << callingRange->valid() << std::endl;
        out << "update count: "  << std::left << std::setw(COL_WIDTH)
            << callingRange->updateCount_ << std::endl;
        out << "resident objects: " << std::left << std::setw(COL_WIDTH)
            << callingRange->residentObjects_.size() << std::endl;
        CallingRange::ObjectXLMap::const_iterator i;
        for (i = callingRange->residentObjects_.begin(); 
             i != callingRange->residentObjects_.end(); ++i)
            out << "       object ID: " << std::left << std::setw(COL_WIDTH) << i->first << std::endl;
        out << std::endl;
        return out;
    }

}


