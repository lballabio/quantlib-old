
/*
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

#include <ohxl/conversions.hpp>
#include <ohxl/instancenamexl.hpp>
#include <ohxl/functioncall.hpp>
#include <oh/exception.hpp>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>

const int KEY_WIDTH = 5;
const int KEY_BASE = 16;
const double KEY_MAX = pow((double)KEY_BASE, KEY_WIDTH);
const char KEY_DELIMITER = '~';

namespace ObjHandler {

int InstanceNameXL::keyCount_ = 0;

    std::string InstanceNameXL::getKeyCount() {
        if (keyCount_ > KEY_MAX)
            throw std::exception("InstanceNameXL::getKeyCount: max key value exceeded");
        std::ostringstream s;
        s << '_' << std::setw(KEY_WIDTH) << std::setfill('0') << std::setbase(KEY_BASE) << keyCount_++;
        return s.str();
    }

    InstanceNameXL::InstanceNameXL(const std::string &stubName) {
        stubName_ = stubName;

        if (stubName_.find(KEY_DELIMITER) != std::string::npos) {
            std::ostringstream err;
            err << "object instance name '" << stubName_ << "' is invalid "
                "because it contains the delimiter character '" << KEY_DELIMITER << "'";
            throw ObjHandler::Exception(err.str().c_str());
        }

        // derive key and full name

        key_ = getKeyCount();
        fullName_ = stubName_ + KEY_DELIMITER + key_;

        // name the calling cell

        const XLOPER *xCaller = FunctionCall::instance().getCallerReference();
        XLOPER xRet;
        Excel(xlfSetName, &xRet, 2, TempStrStl(key_), xCaller);
        if (xRet.xltype != xltypeBool || !xRet.val.boolean)
            throw Exception("error on call to xlfSetName");

    }

    InstanceNameXL::~InstanceNameXL() {
        Excel(xlfSetName, 0, 1, TempStrStl(key_));
    }

    bool InstanceNameXL::isValid() {

        // XLOPERs which might need freeing in the event of an exception

        XLOPER xDef;
        XLOPER xRef;

        try {
        
            Excel(xlfGetName, &xDef, 1, TempStrStl(key_));

            std::string address;
            operToScalar(address, xDef);
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
            throw Exception(err.str().c_str());
        }

    }

}

