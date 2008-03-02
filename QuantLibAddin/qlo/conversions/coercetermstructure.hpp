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

#ifndef qlo_conversions_coercetermstructure_hpp
#define qlo_conversions_coercetermstructure_hpp

#include <oh/conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/handle.hpp>
#include <qlo/conversions/coerceobject.hpp>
#include <qlo/conversions/coercelibrarydifferent.hpp>
#include <qlo/yieldtermstructures.hpp>
#include <qlo/swaptionvolstructure.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>

namespace QuantLibAddin {

    // CoerceTermStructure: A wrapper for handleToLibraryDifferent<> which hard-codes
    // those template parameters that are specific to Handle<TermStructure>

    template <class ObjectTermStructure, class LibraryTermStructure>
    class CoerceTermStructure : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<LibraryTermStructure> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<LibraryTermStructure> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<ObjectTermStructure, LibraryTermStructure>,
                handleToLibraryDifferent<YieldTermStructure, QuantLib::YieldTermStructure, LibraryTermStructure>,
                handleToLibraryDifferent<SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure, LibraryTermStructure>,
                0
            };
            return conversions;
        };
    };

    // CoerceTermStructure: Specialization for QuantLib::YieldTermStructure -
    // wrap handleToLibrarySame<> instead of handleToLibraryDifferent<>

    template <>
    class CoerceTermStructure<YieldTermStructure, QuantLib::YieldTermStructure>
        : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<QuantLib::YieldTermStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<YieldTermStructure, QuantLib::YieldTermStructure>,
                handleToLibrarySame<YieldTermStructure, QuantLib::YieldTermStructure>,
                0
            };
            return conversions;
        };
    };

    // CoerceTermStructure: Specialization for QuantLib::SwaptionVolatilityStructure -
    // wrap handleToLibrarySame<> instead of handleToLibraryDifferent<>

    template <>
    class CoerceTermStructure<SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>
        : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<QuantLib::SwaptionVolatilityStructure> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>,
                handleToLibrarySame<SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure>,
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

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<ObjectTermStructure> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToObject<ObjectTermStructure>,
                handleToObject<SwaptionVolatilityStructure, QuantLib::SwaptionVolatilityStructure, ObjectTermStructure>,
                // At present CoerceTermStructureObject is not required for any classes derived from YieldTermStructure.
                // See file QuantLibAddin/gensrc/metadata/types/types.xml, supertype "objectTermStructure"
                //handleToObject<YieldTermStructure, QuantLib::YieldTermStructure, ObjectTermStructure>,
                0
            };
            return conversions;
        };
    };

}

#endif

