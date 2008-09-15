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

#include <ohxl/objectwrapperxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/callingrange.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace ObjectHandler {

    ObjectWrapperXL::ObjectWrapperXL(
        const std::string &id,
        const boost::shared_ptr<Object> &object,
        const boost::shared_ptr<CallingRange> &callingRange)
            : id_(id), ObjectWrapper(object), callingRange_(callingRange) {
        idFull_ = callingRange_->updateID(id_);
    };

    ObjectWrapperXL::~ObjectWrapperXL() {
        callingRange_->unregisterObject(id_);
    }

    void  ObjectWrapperXL::dump(std::ostream& out) {
        ObjectWrapper::dump(out);
        out << "Excel info:" << std::endl << std::endl;
        out << "full object ID = " << std::left << std::setw(Object::logColumnWidth) 
            << idFull_ << std::endl;
        out << "calling range  = " << std::left << std::setw(Object::logColumnWidth) 
            << callerAddress() << std::endl << std::endl;
    }

    void ObjectWrapperXL::reset(boost::shared_ptr<Object> object) {
        ObjectWrapper::reset(object);
        idFull_ = callingRange_->updateID(id_);
    }

    std::string ObjectWrapperXL::callerKey() const {
        return callingRange_->key();
    }

    std::string ObjectWrapperXL::callerAddress() const {
        return callingRange_->addressString();
    }

    void ObjectWrapperXL::resetCaller(boost::shared_ptr<CallingRange> callingRange) {
        callingRange_->unregisterObject(id_);
        callingRange_ = callingRange;
    }

}

