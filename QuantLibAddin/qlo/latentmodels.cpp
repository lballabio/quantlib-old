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
#include <qlo/latentmodels.hpp>

#include <ql/experimental/credit/defaultprobabilitylatentmodel.hpp>

namespace QuantLibAddin {

    GaussianDefProbLM::GaussianDefProbLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::Basket>& basket,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::GaussianDefProbLM>(properties, 
      permanent) {
        libraryObject_ = boost::make_shared<QuantLib::GaussianDefProbLM>(
            //basket,
            factorWeights,
            QuantLib::LatentModelIntegrationType::GaussianQuadrature);
        libraryObject_->resetBasket(basket);
    }

    TDefProbLM::TDefProbLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Integer>& tOrders,
        const boost::shared_ptr<QuantLib::Basket>& basket,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::TDefProbLM>(properties, 
      permanent) {
        QuantLib::TCopulaPolicy::initTraits initsT;
        initsT.tOrders = tOrders;
        libraryObject_ = boost::make_shared<QuantLib::TDefProbLM>(
            //basket,
            factorWeights,
            QuantLib::LatentModelIntegrationType::GaussianQuadrature,
            initsT);
        libraryObject_->resetBasket(basket);
    }

}
