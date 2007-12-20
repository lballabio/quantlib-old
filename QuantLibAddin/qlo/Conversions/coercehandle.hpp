/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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

#ifndef qlo_conversions_coercehandle_hpp
#define qlo_conversions_coercehandle_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handleimpl.hpp>

namespace QuantLibAddin {

    // CoerceHandle: Accept an ID of an Object in the Repository 
    // and return a QuantLib::Handle<LibraryClass>.
    // 1) If the ID is an empty string then return an empty handle
    // 2) If the Object is of class 
    //    QuantLibAddin::RelinkableHandleImpl<ObjectClass, LibraryClass>
    //    then return the contained QuantLib::Handle<LibraryClass>
    // 3) If the Object is of class QuantLibAddin::ObjectClass then convert it
    //    to a QuantLib::Handle<LibraryClass>
    // 4) Otherwise the Object is of an unexpected class so raise an exception

    template <class ObjectClass, class LibraryClass>
    bool objectToHandle(
        const boost::shared_ptr<ObjectHandler::Object> &object,
        QuantLib::Handle<LibraryClass> &out) {

        typedef RelinkableHandleImpl<ObjectClass, LibraryClass> HandleClass;
        boost::shared_ptr<HandleClass> handlePointer =
            boost::dynamic_pointer_cast<HandleClass>(object);
        if (handlePointer) {
            out = handlePointer->handle();
            return true;
        } else {
            return false;
        }
    }

    template <class ObjectClass, class LibraryClass>
    bool wrapObject(
        const boost::shared_ptr<ObjectHandler::Object> &object,
        QuantLib::Handle<LibraryClass> &out) {

        boost::shared_ptr<ObjectClass> qloPointer =
            boost::dynamic_pointer_cast<ObjectClass>(object);
        if (qloPointer) {
            boost::shared_ptr<LibraryClass> ret;
            qloPointer->getLibraryObject(ret);
            out = QuantLib::Handle<LibraryClass>(ret);
            return true;
        } else {
            return false;
        }
    }

    template <class ObjectClass, class LibraryClass>
    class CoerceHandle : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>, 
        QuantLib::Handle<LibraryClass> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            QuantLib::Handle<LibraryClass> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToHandle<ObjectClass, LibraryClass>, 
                wrapObject<ObjectClass, LibraryClass>, 
                0
            };
            return conversions; 
        };

        virtual bool inputMissing(const boost::shared_ptr<ObjectHandler::Object> &object) {
            return !object;
        }
    };
}

#endif

