
/*
 Copyright (C) 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_handleimpl_hpp
#define qla_handleimpl_hpp

#include <qlo/handle.hpp>
#include <oh/repository.hpp>
#include <ql/handle.hpp>

namespace QuantLibAddin {

    template <class ObjectClass, class LibraryClass>
    class RelinkableHandleImpl : public RelinkableHandle {

    public:

        RelinkableHandleImpl(const boost::shared_ptr<ObjectHandler::ValueObject> &properties,
            const std::string &objectId,
            bool permanent) : RelinkableHandle(properties, objectId, permanent) {

            linkTo(objectId);
        }

        const boost::shared_ptr<ObjectClass> &object() const {
            OH_REQUIRE(object_, "Attempt to retrieve null object reference");
            return object_;
        }

        const QuantLib::RelinkableHandle<LibraryClass> &handle() const {
            return handle_;
        }

    private:

        void linkTo(const std::string &objectId) {

            if (!objectId.empty()) {
                ObjectHandler::Repository::instance().retrieveObject(object_, objectId);
                boost::shared_ptr<LibraryClass> observable;
                object_->getLibraryObject(observable);
                handle_.linkTo(observable);
            }

            properties()->setProperty("CurrentLink", objectId);
        }

        bool empty() const { return handle_.empty(); }

        boost::shared_ptr<ObjectClass> object_;
        QuantLib::RelinkableHandle<LibraryClass> handle_;
    };

}

#endif
