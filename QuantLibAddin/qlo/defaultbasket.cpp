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

#include <boost/make_shared.hpp>

#include <qlo/defaultbasket.hpp>

#include <ql/currencies/europe.hpp>

#include <ql/experimental/credit/basket.hpp>


namespace QuantLibAddin {

    /* \todo: Add amortizing version
    */
    Basket::Basket(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,

        const std::vector<std::string>& names,
        const std::vector<boost::shared_ptr<QuantLib::Issuer> >& issuers,
        const std::vector<QuantLib::Real>& notionals,
        const QuantLib::Date& refDate,
        QuantLib::Real attachRatio,
        QuantLib::Real detachRatio,
        bool amortizes,

        bool permanent
        )
        : ObjectHandler::LibraryObject<QuantLib::Basket>(properties, permanent)
    {
        QL_REQUIRE(names.size() == issuers.size(), 
            "Different number of names and issuers.");

        std::vector<QuantLib::DefaultProbKey> contractTriggers(
            names.size(), 
            QuantLib::NorthAmericaCorpDefaultKey(QuantLib::EURCurrency(),
                QuantLib::SeniorSec, 
                QuantLib::Period(),
                1. // amount threshold
            ));

        boost::shared_ptr<QuantLib::Pool> pool(new QuantLib::Pool());
        for(QuantLib::Size i=0; i<names.size(); i++)
                pool->add(names[i], *issuers[i], contractTriggers[i]);
        libraryObject_ = 
            boost::make_shared<QuantLib::Basket>(
                QuantLib::Basket(refDate, names, notionals, pool,
                    attachRatio, detachRatio));
    }

}
