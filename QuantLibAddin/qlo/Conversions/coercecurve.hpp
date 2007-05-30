
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

#ifndef qlo_conversions_coercecurve_hpp
#define qlo_conversions_coercecurve_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <qlo/Conversions/coerceobject.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/swaptionvolstructure.hpp>

namespace QuantLibAddin {

    template <class HandleClass, class LibraryClass>
    bool curveFromHandle2(
            const boost::shared_ptr<ObjectHandler::Object> &in,
            boost::shared_ptr<LibraryClass> &out) {

        boost::shared_ptr<QuantLibAddin::RelinkableHandle<HandleClass> > 
            handleCurve = boost::dynamic_pointer_cast<
                QuantLibAddin::RelinkableHandle<HandleClass> >(in);

        if (!handleCurve)
            return false;

        boost::shared_ptr<HandleClass> pointerCurve = 
            handleCurve->getHandle().currentLink();
        OH_REQUIRE(pointerCurve, "unable to retrieve reference"
                << " contained in handle");

        out = boost::dynamic_pointer_cast<LibraryClass>(pointerCurve);
        OH_REQUIRE(out, "unable to convert reference from " 
            << typeid(HandleClass).name()<< " to "
            << typeid(LibraryClass).name());

        return true;
    }

    template <class ObjectClass, class LibraryClass, class HandleClass>
    class CoerceCurve : public ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>, 
            boost::shared_ptr<LibraryClass> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToReference<ObjectClass, LibraryClass>,
                curveFromHandle2<HandleClass, LibraryClass>, 
                0
            };
            return conversions; 
        };
    };

}

#endif
