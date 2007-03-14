
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qlo_conversions_coercehandle_hpp
#define qlo_conversions_coercehandle_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>

namespace ObjHandler {

    // Accept an id of an Object in the Repository and return a QuantLib::RelinkableHandle<qlClass>.
    // 1) If the id is an empty string then return an empty handle
    // 2) If the Object is of class QuantLibAddin::RelinkableHandle then return the contained QuantLib::RelinkableHandle<qlClass>
    // 3) If the Object is of class QuantLibAddin::qloClass then convert it to a QuantLib::RelinkableHandle<qlClass>
    // 4) Otherwise the Object is of an unexpected class so raise an exception

    template <class qlClass>
    bool objectToHandle(
            const boost::shared_ptr<ObjHandler::Object> &in,
            QuantLib::RelinkableHandle<qlClass> &out) {
        boost::shared_ptr<QuantLibAddin::RelinkableHandle<qlClass> > handlePointer =
            boost::dynamic_pointer_cast<QuantLibAddin::RelinkableHandle<qlClass> >(in);
        if (handlePointer) {
            out = handlePointer->getHandle();
            return true;
        } else {
            return false;
        }
    }

    template <class qloClass, class qlClass>
    bool wrapObject(
            const boost::shared_ptr<ObjHandler::Object> &in,
            QuantLib::RelinkableHandle<qlClass> &out) {
        boost::shared_ptr<qloClass> qloPointer =
            boost::dynamic_pointer_cast<qloClass>(in);
        if (qloPointer) {
            boost::shared_ptr<qlClass> ret;
            qloPointer->getLibraryObject(ret);
            out.linkTo(ret);
            return true;
        } else {
            return false;
        }
    }

    template <class qloClass, class qlClass>
    class CoerceHandle : public ObjHandler::Coerce<
            boost::shared_ptr<ObjHandler::Object>, 
            QuantLib::RelinkableHandle<qlClass> > {
        typedef typename ObjHandler::Coerce<
            boost::shared_ptr<ObjHandler::Object>,
            QuantLib::RelinkableHandle<qlClass> >::Conversion Conversion; 
        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToHandle<qlClass>, 
                wrapObject<qloClass, qlClass>, 
                0
            };
            return conversions; 
        };
    };
}

#endif

