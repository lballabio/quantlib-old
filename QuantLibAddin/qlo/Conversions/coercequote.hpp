
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

#ifndef qlo_conversions_coercequote_hpp
#define qlo_conversions_coercequote_hpp

#include <qlo/Conversions/coerceobject.hpp>
#include <qlo/Conversions/coercelibrarydifferent.hpp>
#include <qlo/quotes.hpp>

namespace QuantLibAddin {

    // CoerceQuote: A wrapper for handleToLibraryDifferent<> which hard-codes
    // those template parameters that are specific to Handle<Quote>

    template <class ObjectQuote, class LibraryQuote>
    class CoerceQuote : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<LibraryQuote> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<LibraryQuote> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<ObjectQuote, LibraryQuote>,
                handleToLibraryDifferent<QuantLibAddin::Quote, QuantLib::Quote, LibraryQuote>,
                0
            };
            return conversions;
        };
    };

    // CoerceQuote: Specialization for QuantLib::Quote - wrap
    // handleToLibrarySame<> instead of handleToLibraryDifferent<>

    template <>
    class CoerceQuote<QuantLibAddin::Quote, QuantLib::Quote>
        : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<QuantLib::Quote> > {

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToLibrary<QuantLibAddin::Quote, QuantLib::Quote>,
                handleToLibrarySame<QuantLibAddin::Quote, QuantLib::Quote>,
                0
            };
            return conversions;
        };
    };

    // CoerceQuoteObject: A substitute for CoerceObject which hard-codes
    // those template parameters that are specific to Handle<Quote>.
    //
    // The difference between CoerceQuote and CoerceQuoteObject is that
    // the former returns boost::shared_ptr<QuantLib::T> while
    // the latter returns boost::shared_ptr<QuantLibAddin::T>
    // where in either case T inherits from Quote.

    template <class ObjectQuote>
    class CoerceQuoteObject : public ObjectHandler::Coerce<
        boost::shared_ptr<ObjectHandler::Object>,
        boost::shared_ptr<ObjectQuote> > {

        typedef typename ObjectHandler::Coerce<
            boost::shared_ptr<ObjectHandler::Object>,
            boost::shared_ptr<ObjectQuote> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                objectToObject<ObjectQuote>,
                handleToObject<QuantLibAddin::Quote, QuantLib::Quote, ObjectQuote>,
                0
            };
            return conversions;
        };
    };

}

#endif

