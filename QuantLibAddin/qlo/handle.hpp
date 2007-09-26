
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

#ifndef qla_handle_hpp
#define qla_handle_hpp

#include <oh/repository.hpp>
#include <ql/handle.hpp>

namespace QuantLibAddin {

    class Handle : public ObjectHandler::Object {
    public:
        std::string currentLink() {
            return boost::any_cast<std::string>(propertyValue("CurrentLink"));
        }
        virtual bool empty() const = 0;
    protected:
        Handle(const boost::shared_ptr<ObjectHandler::ValueObject> &properties,
           const std::string &objectId,
           bool permanent) : ObjectHandler::Object(properties, permanent) {}
    };

    class RelinkableHandle : public Handle {
    public:
        virtual void linkTo(const std::string &objectID) = 0;
    protected:
        RelinkableHandle(const boost::shared_ptr<ObjectHandler::ValueObject> &properties,
           const std::string &objectId,
           bool permanent) : Handle(properties, objectId, permanent) {}
    };

    template <class ObjectClass, class LibraryClass>
    class RelinkableHandleImpl : public RelinkableHandle {

    public:

        QuantLib::RelinkableHandle<LibraryClass> &getHandle() { return relinkableHandle_; }

    protected:

        RelinkableHandleImpl(const boost::shared_ptr<ObjectHandler::ValueObject> &properties,
            const std::string &objectId,
            bool permanent) : RelinkableHandle(properties, objectId, permanent) {

            linkTo(objectId);
        }

    private:

        void linkTo(const std::string &objectId) {
            OH_GET_REFERENCE_DEFAULT(observable, objectId, ObjectClass, LibraryClass)
            relinkableHandle_.linkTo(observable);
            properties()->setProperty("CurrentLink", objectId);
        }

        bool empty() const { return relinkableHandle_.empty(); }

        QuantLib::RelinkableHandle<LibraryClass> relinkableHandle_;
    };

}

#endif

