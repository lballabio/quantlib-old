/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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

#ifndef qlo_conversions_coerceobject2_hpp
#define qlo_conversions_coerceobject2_hpp

#include <oh/conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <qlo/handleimpl.hpp>
#include <qlo/conversions/coerceobject.hpp>

namespace QuantLibAddin {

    // CoerceObject: Accept an id of an Object in the Repository
    // and return a boost::shared_ptr<QuantLibAddin::T>.
    //
    // This coercion is usually used to invoke a member function
    // QuantLibAddin::T::func() which wraps a QuantLib::T:func()
    //
    // Here is the logic for CoerceObject:
    // 1) If the Object is of class QuantLibAddin::T
    //    then return it
    // 2) If the Object is of class QuantLibAddin::Handle<T>
    //    then return the contained QuantLibAddin::T
    // 3) Otherwise the Object is of an unexpected class so raise an exception

    template <class ObjectTo>
    bool objectToObject(
        const boost::shared_ptr<ObjectHandler::Object> &in,
        boost::shared_ptr<ObjectTo> &out) {

        out = boost::dynamic_pointer_cast<ObjectTo>(in);
        return out;
    }

    template <class ObjectFrom, class LibraryFrom, class ObjectTo>
    bool handleToObject(
        const boost::shared_ptr<ObjectHandler::Object> &in,
        boost::shared_ptr<ObjectTo> &out) {

        // FIXME gcc doesn't like this typedef
        //typedef RelinkableHandleImpl<ObjectFrom, LibraryFrom> HandleClass;
        //boost::shared_ptr<HandleClass> handle =
        //    boost::dynamic_pointer_cast<HandleClass>(in);
        boost::shared_ptr<RelinkableHandleImpl<ObjectFrom, LibraryFrom> > handle =
            boost::dynamic_pointer_cast<RelinkableHandleImpl<ObjectFrom, LibraryFrom> >(in);

        if (!handle) return false;

        out = boost::dynamic_pointer_cast<ObjectTo>(handle->object());
        OH_REQUIRE(out, "unable to convert reference from class '"
            << typeid(ObjectFrom).name()<< "' to class '"
            << typeid(ObjectTo).name() << "'");

        return true;
    }

    template <class ObjectFrom, class LibraryFrom, class ObjectTo>
    class CoerceObject : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<ObjectTo> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<ObjectTo> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToObject<ObjectTo>,
                handleToObject<ObjectFrom, LibraryFrom, ObjectTo>,
                0
            };
            return conversions;
        };
    };

}

#endif

