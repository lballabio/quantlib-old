
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

#ifndef qlo_conversions_coercetermstructure_hpp
#define qlo_conversions_coercetermstructure_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <qlo/Conversions/coercelibrarydifferent.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/swaptionvolstructure.hpp>
#include <ql/yieldtermstructure.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>

namespace QuantLibAddin {

    // CoerceTermStructure: A wrapper for handleToLibraryDifferent<> which hard-codes
    // those template parameters that are specific to Handle<TermStructure>

    template <class ObjectTermStructure, class LibraryTermStructure>
    class CoerceTermStructure : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<LibraryTermStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<ObjectTermStructure, LibraryTermStructure>,
                handleToLibraryDifferent<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure, LibraryTermStructure>,
                handleToLibraryDifferent<QuantLibAddin::SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure, LibraryTermStructure>,
                0
            };
            return conversions;
        };
    };

    // CoerceTermStructure: Specialization for QuantLib::YieldTermStructure -
    // wrap handleToLibrarySame<> instead of handleToLibraryDifferent<>

    template <>
    class CoerceTermStructure<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure>
        : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<QuantLib::YieldTermStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure>,
                handleToLibrarySame<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure>,
                0
            };
            return conversions;
        };
    };

    // CoerceTermStructure: Specialization for QuantLib::SwaptionVolatilityStructure -
    // wrap handleToLibrarySame<> instead of handleToLibraryDifferent<>

    template <>
    class CoerceTermStructure<QuantLibAddin::SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>
        : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<QuantLib::SwaptionVolatilityStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<QuantLibAddin::SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>,
                handleToLibrarySame<QuantLibAddin::SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>,
                0
            };
            return conversions;
        };
    };

    // CoerceTermStructureObject: A substitute for CoerceObject which hard-codes
    // those template parameters that are specific to Handle<TermStructure>.
    //
    // The difference between CoerceTermStructure and CoerceTermStructureObject is that
    // the former returns boost::shared_ptr<QuantLib::T> while
    // the latter returns boost::shared_ptr<QuantLibAddin::T>
    // where in either case T might be extracted from 
    // Handle<YieldTermStructure> or Handle<SwaptionVolatilityStructure>.

    template <class ObjectTermStructure>
    class CoerceTermStructureObject : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<ObjectTermStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToObject<ObjectTermStructure>,
                handleToObject<QuantLibAddin::SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure, ObjectTermStructure>,
                // At present CoerceTermStructureObject is not required for any classes derived from YieldTermStructure.
                // See file QuantLibAddin/gensrc/metadata/Types/types.xml, supertype "objectTermStructure"
                //handleToObject<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure, ObjectTermStructure>,
                0
            };
            return conversions;
        };
    };

}

#endif

