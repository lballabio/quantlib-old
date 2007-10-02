
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

#ifndef qla_convert_date_hpp
#define qla_convert_date_hpp

#include <oh/types.hpp>
#include <oh/Conversions/varianttoscalar.hpp>
#include <ql/utilities/dataparsers.hpp>
#include <ql/settings.hpp>

namespace ObjectHandler {

    template <class V>
    struct VariantToScalar<V, QuantLib::Date> {
        QuantLib::Date operator()(const V &variant) {

            if (variant.type() == ObjectHandler::Double || variant.type() == ObjectHandler::Long) {
                return QuantLib::Date(variant);
            } else if (variant.type() == ObjectHandler::String) {
                QuantLib::Date d = QuantLib::Settings::instance().evaluationDate();
                QuantLib::Period p = QuantLib::PeriodParser::parse(variant);
                return d + p;
            } else {
                OH_FAIL("invalid conversion");
            }
        }
    };

}

#endif
