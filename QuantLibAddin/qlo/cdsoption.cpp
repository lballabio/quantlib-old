/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Jose Aparicio

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

#include <qlo/cdsoption.hpp>

#include <ql/instruments/creditdefaultswap.hpp>
#include <ql/experimental/credit/cdsoption.hpp>
#include <ql/experimental/credit/blackcdsoptionengine.hpp>

namespace QuantLibAddin {

    CdsOption::CdsOption(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::CreditDefaultSwap>& swap,
        const boost::shared_ptr<QuantLib::Exercise>& exercise,
        bool permanent)
   : Instrument(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::CdsOption>(
            new QuantLib::CdsOption(swap, exercise));
    }

    BlackCdsOptionEngine::BlackCdsOptionEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure>& dfts,
        QuantLib::Real recoveryRate,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& yts,
        const QuantLib::Handle<QuantLib::Quote>& vol,
        bool permanent)
    : PricingEngine(properties, permanent) {
        libraryObject_ = 
            boost::shared_ptr<QuantLib::BlackCdsOptionEngine>(
            new QuantLib::BlackCdsOptionEngine(dfts, recoveryRate, yts, vol));
    }

}

