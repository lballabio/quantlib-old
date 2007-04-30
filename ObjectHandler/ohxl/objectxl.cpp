
/*
 Copyright (C) 2007 Eric Ehlers

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

#include <ohxl/objectxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/callingrange.hpp>
#include <iomanip>

namespace ObjectHandler {

    ObjectXL::ObjectXL(const std::string &id, const boost::shared_ptr<Object> &object)
        : id_(id), idFull_(id), object_(object) {
    };

#ifdef COMPILING_XLL_DYNAMIC
    boost::shared_ptr<ObjectXL> ObjectXL::create(const std::string &id, const boost::shared_ptr<Object> &object) {
        return boost::shared_ptr<ObjectXL>(new ObjectXL(id, object));
    }
#endif

    std::string ObjectXL::getStub(const std::string &objectID) {
        int counterOffset = objectID.length() - CallingRange::keyWidth();
        if (counterOffset >= 0 && objectID[counterOffset] == '#')
            return objectID.substr(0, counterOffset);
        else
            return objectID;
    }

    void ObjectXL::setCallingRange(const boost::shared_ptr<CallingRange> &callingRange) {
        callingRange_ = callingRange;

        if (id_.empty())
            id_ = "obj" + callingRange->key();

        idFull_ = id_ + "#" + callingRange_->updateCount();
    }

    void ObjectXL::dump(std::ostream &out) {
        object_->dump(out);
        out << "Excel info:" << std::endl << std::endl;
        out << "full object ID = " << std::left << std::setw(OBJECT_LOG_COLUMN_WIDTH) 
            << idFull_ << std::endl;
        out << "calling range  = " << std::left << std::setw(OBJECT_LOG_COLUMN_WIDTH) 
            << callerAddress() << std::endl;
    }

    std::string ObjectXL::callerKey() const {
        if (callingRange_) {
            return callingRange_->key();
        } else {
            return "VBA";
        }
    }

    std::string ObjectXL::callerAddress() const {
        if (callingRange_) {
            return callingRange_->addressString();
        } else {
            return "VBA";
        }
    }

}

