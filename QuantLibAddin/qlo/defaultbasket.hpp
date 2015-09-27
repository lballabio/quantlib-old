/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Jose Aparicio

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

#ifndef qla_defaultbasket_hpp
#define qla_defaultbasket_hpp

#include <qlo/baseinstruments.hpp>

#include <ql/types.hpp>
#include <ql/time/date.hpp>

namespace QuantLib {
    class Basket;
    class Issuer;
}

namespace QuantLibAddin {

    class Basket // CreditBasket
        : public ObjectHandler::LibraryObject<QuantLib::Basket> {
    public:
        Basket(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::string>& names,
            const std::vector<boost::shared_ptr<QuantLib::Issuer> >& issuers,
            const std::vector<QuantLib::Real>& notionals,
            const QuantLib::Date& refDate,
            QuantLib::Real attachRatio,
            QuantLib::Real detachRatio,
            bool amortizes,
            bool permanent
            );

    };

   // QuantLib::Real numBasketDefaults(const boost::shared_ptr<QuantLib::Basket>&);
}

#endif
