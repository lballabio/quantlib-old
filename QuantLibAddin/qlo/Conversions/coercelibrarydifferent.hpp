
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

#ifndef qlo_conversions_coercelibrarydifferent_hpp
#define qlo_conversions_coercelibrarydifferent_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <qlo/Conversions/coercelibrarysame.hpp>

namespace QuantLibAddin {

    // CoerceLibraryDifferent: Accept a boost::shared_ptr<Object> and attempt to
    // downcast it to a boost::shared_ptr<LibraryClass>.
    //
    // The term "Different" refers to the fact that the class required is not
    // the same as the class that is used to wrap this object in a handle, e.g.
    //   Handle<Base>  ->  Derived
    // Compare to CoerceLibrarySame where one less cast is required.
    //
    // Here is the logic for CoerceLibraryDifferent:
    // 1) If the Object is of class QuantLibAddin::ObjectClass
    //    then return the contained boost::shared_ptr<LibraryClass>
    // 2) If the Object is of class QuantLibAddin::Handle
    //    then return the contained boost::shared_ptr<LibraryFrom>
    //    and cast it to the required class LibraryTo
    // 3) Otherwise the Object is of an unexpected class so raise an exception

    template <class ObjectFrom, class LibraryFrom, class LibraryTo>
    bool handleToLibraryDifferent(
        const boost::shared_ptr<ObjectHandler::Object> &in,
        boost::shared_ptr<LibraryTo> &out) {

        typedef RelinkableHandleImpl<ObjectFrom, LibraryFrom> HandleClass;
        boost::shared_ptr<HandleClass> handle =
            boost::dynamic_pointer_cast<HandleClass>(in);

        if (!handle) return false;

        boost::shared_ptr<LibraryFrom> libraryFrom =
            handle->handle().currentLink();
        OH_REQUIRE(libraryFrom, "unable to retrieve reference "
            "contained in handle");

        out = boost::dynamic_pointer_cast<LibraryTo>(libraryFrom);
        OH_REQUIRE(out, "unable to convert reference from class '"
            << typeid(LibraryFrom).name()<< "' to class '"
            << typeid(LibraryTo).name() << "'");

        return true;
    }

    template <class ObjectFrom, class LibraryFrom, class ObjectTo, class LibraryTo>
    class CoerceLibraryDifferent : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<LibraryTo> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<LibraryTo> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<ObjectTo, LibraryTo>,
                handleToLibraryDifferent<ObjectFrom, LibraryFrom, LibraryTo>,
                0
            };
            return conversions;
        };
    };

}

#endif

