/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

 Copyright (C) 2013 Peter Caspers

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

#include <ql/experimental/models/cmsswaption.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/exercise.hpp>

namespace QuantLib {

    CmsSwaption::CmsSwaption(const boost::shared_ptr<CmsSwap>& swap,
                       const boost::shared_ptr<Exercise>& exercise)
    : Option(boost::shared_ptr<Payoff>(), exercise), swap_(swap) {
        registerWith(swap_);
    }

    bool CmsSwaption::isExpired() const {
        return detail::simple_event(exercise_->dates().back()).hasOccurred();
    }

    void CmsSwaption::setupArguments(PricingEngine::arguments* args) const {

        swap_->setupArguments(args);

        CmsSwaption::arguments* arguments =
            dynamic_cast<CmsSwaption::arguments*>(args);

        QL_REQUIRE(arguments != 0, "wrong argument type");

        arguments->swap = swap_;
        arguments->exercise = exercise_;
    }

    void CmsSwaption::arguments::validate() const {
        CmsSwap::arguments::validate();
        QL_REQUIRE(swap, "underlying cms swap not set");
        QL_REQUIRE(exercise, "exercise not set");
    }

}
