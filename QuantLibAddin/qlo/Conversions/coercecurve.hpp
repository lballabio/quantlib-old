
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

#ifndef qlo_conversions_coercecurve_hpp
#define qlo_conversions_coercecurve_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <ql/yieldtermstructure.hpp>
#include <ql/swaptionvolstructure.hpp>

namespace ObjHandler {

    bool curveFromObject(
            const boost::shared_ptr<ObjHandler::Object> &in,
            boost::shared_ptr<QuantLib::TermStructure> &out);

    template <class CurveHandle>
    bool curveFromHandle(
            const boost::shared_ptr<ObjHandler::Object> &in,
            boost::shared_ptr<QuantLib::TermStructure> &out) {

        boost::shared_ptr<QuantLibAddin::RelinkableHandle<CurveHandle> > 
            handleCurve = boost::dynamic_pointer_cast<
                QuantLibAddin::RelinkableHandle<CurveHandle> >(in);

        if (!handleCurve)
            return false;

        OH_REQUIRE(!handleCurve->getHandle().empty(),
                "handle contains null reference");

        boost::shared_ptr<CurveHandle> pointerCurve = 
            handleCurve->getHandle().currentLink();
        OH_REQUIRE(pointerCurve, "unable to retrieve reference"
                << " contained in handle");

        out = boost::dynamic_pointer_cast<QuantLib::TermStructure>(pointerCurve);
        OH_REQUIRE(out, "unable to convert reference from " 
            << typeid(CurveHandle).name()
            << " to QuantLib::TermStructure");

        return true;
    }

    class CoerceCurve : public ObjHandler::Coerce<
            boost::shared_ptr<ObjHandler::Object>, 
            boost::shared_ptr<QuantLib::TermStructure> > {

    public:

        ~CoerceCurve() {}

    private:

        Conversion *getConversions() {
            static Conversion conversions[] = {
                curveFromObject, 
                curveFromHandle<QuantLib::YieldTermStructure>, 
                curveFromHandle<QuantLib::SwaptionVolatilityStructure>, 
                0
            };
            return conversions; 
        };
    };

}

#endif
