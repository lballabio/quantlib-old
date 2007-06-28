
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

#ifndef qla_convert_quote_hpp
#define qla_convert_quote_hpp

#include <oh/types.hpp>
#include <qlo/quotes.hpp>
#include <ql/quote.hpp>
#include <ql/quotes/simplequote.hpp>

namespace ObjectHandler {

    template <class V>
    struct VariantToScalar<V, boost::shared_ptr<QuantLib::Quote> > {
        boost::shared_ptr<QuantLib::Quote> operator()(const V &variant) {

            if (variant.type() == ObjectHandler::Double) {
                return boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(variant));
            } else if (variant.type() == ObjectHandler::String) {
                OH_GET_OBJECT(temp, variant, ObjectHandler::Object)
                return QuantLibAddin::CoerceObject<QuantLibAddin::Quote, QuantLib::Quote>()(temp);
            } else {
                OH_FAIL("unable to convert input value to QuantLib::Quote");
            }
        }
    };

}

#endif
