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

#ifndef qla_latentmodels_hpp
#define qla_latentmodels_hpp

#include <oh/libraryobject.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    template<class copulaPolicy> class DefaultLatentModel;

    struct GaussianCopulaPolicy;
    class TCopulaPolicy;

    typedef DefaultLatentModel<GaussianCopulaPolicy> GaussianDefProbLM;
    typedef DefaultLatentModel<TCopulaPolicy> TDefProbLM;

    class Basket;
}

namespace QuantLibAddin {

    /* \todo: This needs to be turn into a factory with a common ancestor and
    all the available options (integration policy etc)
    */
    class GaussianDefProbLM  : 
        public ObjectHandler::LibraryObject<QuantLib::GaussianDefProbLM> {
    public:
        GaussianDefProbLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::Basket>& basket,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            bool permanent);
    };

    class TDefProbLM  : 
        public ObjectHandler::LibraryObject<QuantLib::TDefProbLM> {
    public:
        TDefProbLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Integer>& tOrders,
            const boost::shared_ptr<QuantLib::Basket>& basket,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            bool permanent);
    };

}

#endif
