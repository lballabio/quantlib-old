/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ohxl/objectxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/callingrange.hpp>
#include <boost/algorithm/string/case_conv.hpp>

using boost::algorithm::to_upper_copy;
using std::string;

namespace ObjectHandler {

    namespace {
        const string anonPrefix("obj");
        const string ANONPREFIX("OBJ");
        const char counterDelimiter = '#';
    }

    ObjectXL::ObjectXL(const string &id,
                       const boost::shared_ptr<Object> &object)
    : id_(id), idFull_(id), object_(object) {
        string ID = to_upper_copy(id);
        OH_REQUIRE(ID.rfind(ANONPREFIX, ANONPREFIX.size()-1)==string::npos,
                   id<<" is an invalid ID: cannot start with " << anonPrefix);
        OH_REQUIRE(id.find(counterDelimiter, 0)==string::npos,
                   id<<" is an invalid ID: cannot contain "<<counterDelimiter);
    };

    ObjectXL::~ObjectXL() {
        if (callingRange_) {
            callingRange_->unregisterObject(id_);
        }
    }

#ifdef COMPILING_XLL_DYNAMIC
    boost::shared_ptr<ObjectXL> ObjectXL::create(const string &id, const boost::shared_ptr<Object> &object) {
        return boost::shared_ptr<ObjectXL>(new ObjectXL(id, object));
    }
#endif

    string ObjectXL::getStub(const string &objectID) {
        int counterOffset = objectID.length() - CallingRange::keyWidth();
        if (counterOffset >= 0 && objectID[counterOffset] == counterDelimiter)
            return objectID.substr(0, counterOffset);
        else
            return objectID;
    }

    void ObjectXL::setCallingRange(const boost::shared_ptr<CallingRange> &callingRange) {
        callingRange_ = callingRange;

        if (id_.empty())
            id_ = anonPrefix + callingRange->key();

        idFull_ = id_ + counterDelimiter + callingRange_->updateCount();
    }

    void ObjectXL::dump(std::ostream &out) {
        object_->dump(out);
        out << "Excel info:" << std::endl << std::endl;
        out << "full object ID = " << std::left << std::setw(logColumnWidth_) 
            << idFull_ << std::endl;
        out << "calling range  = " << std::left << std::setw(logColumnWidth_) 
            << callerAddress() << std::endl << std::endl;
    }

    string ObjectXL::callerKey() const {
        if (callingRange_) {
            return callingRange_->key();
        } else {
            return "VBA";
        }
    }

    string ObjectXL::callerAddress() const {
        if (callingRange_) {
            return callingRange_->addressString();
        } else {
            return "VBA";
        }
    }

}

