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

#include <ql/termstructures/volatility/swaption/gaussian1dswaptionsmilesection.hpp>
#include <ql/experimental/models/gaussian1dswaptionengine.hpp>
#include <ql/instruments/makeswaption.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/cashflows/cashflows.hpp>

namespace QuantLib {

    Real Gaussian1dSwaptionSmileSection::optionPrice(Rate strike,
                                                     Option::Type type,
                                                     Real discount) const {

        Handle<YieldTermStructure> discountYts =
            index_->discountingTermStructure(); // may be an empty handle

        boost::shared_ptr<IborIndex> ibor =
            index_->iborIndex(); // may be an empty handle

        boost::shared_ptr<PricingEngine> engine(new Gaussian1dSwaptionEngine(
            model_, 64, 7.0, true, false, // hard coded numerical
                                          // parameters here !!
            discountYts));

        Swaption swp = MakeSwaption(index_, expiry_, strike).withUnderlyingType(
                    type == Option::Call ? VanillaSwap::Payer : VanillaSwap::Receiver);

        swp.setPricingEngine(engine);

        Real npv = swp.NPV(); // discounted npv, needs to be lifted ...

        Leg fixLeg = swp.underlyingSwap()->fixedLeg();

        Real annuity = std::fabs(CashFlows::bps(
            fixLeg,
            discountYts.empty() ? **model_->termStructure() : **discountYts,
            false)) * 10000.0; // expiry is always in the future

        return npv / annuity * discount;
    }


    Volatility
    Gaussian1dSwaptionSmileSection::volatilityImpl(Rate strike) const {

        Real impliedVol = 0.0;
        Real forward = atmLevel();
        try {
            Option::Type type;
            if (strike >= forward)   // use otm option to imply
                type = Option::Call; // todo: implement put price in
            else
                type = Option::Put;
            impliedVol =
                blackFormulaImpliedStdDev(type, strike, forward,
                                          optionPrice(strike, type, 1.0), 1.0) /
                std::sqrt(exerciseTime());
        }
        catch (...) {
        }
        return impliedVol;
    }
}
