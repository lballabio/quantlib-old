
/*
 Copyright (C) 2006 Eric Ehlers

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

#include <qlo/Conversions/coercecurve.hpp>
#include <qlo/termstructures.hpp>

namespace ObjHandler {

    bool curveFromObject(
            const boost::shared_ptr<ObjHandler::Object> &in,
            boost::shared_ptr<QuantLib::TermStructure> &out) {
        boost::shared_ptr<QuantLibAddin::TermStructure> pointerTS =
            boost::dynamic_pointer_cast<QuantLibAddin::TermStructure>(in);
        if (pointerTS) {
            pointerTS->getLibraryObject(out);
            return true;
        } else {
            return false;
        }
    }

}

