/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

#include <ql/experimental/volatility/stickybpvolswaptioncube.hpp>
#include <ql/experimental/volatility/multiplicativesmilesection.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/flatextrapolation2d.hpp>
#include <ql/quotes/simplequote.hpp>

#include <boost/make_shared.hpp>

namespace QuantLib {

StickyBpVolSwaptionCube::StickyBpVolSwaptionCube(
    const boost::shared_ptr<SwaptionVolatilityCube> &sourceCube,
    Handle<Quote> atmVolatilitySpread)
    : SwaptionVolatilityCube(
          sourceCube->atmVol(), sourceCube->optionTenors(),
          sourceCube->swapTenors(), sourceCube->strikeSpreads(),
          sourceCube->volSpreads(), sourceCube->swapIndexBase(),
          sourceCube->shortSwapIndexBase(), sourceCube->vegaWeightedSmileFit()),
      sourceCube_(sourceCube),
      atmVolatilitySpread_(atmVolatilitySpread) {

    registerWith(sourceCube_);
    registerWith(atmVolatilitySpread_);

    boost::shared_ptr<SwaptionVolatilityDiscrete> atm =
        boost::dynamic_pointer_cast<SwaptionVolatilityDiscrete>(
            *sourceCube->atmVol());

    // this should never happen (?)
    QL_REQUIRE(atm != NULL, "atm is not of type SwaptionVolatilityDiscrete");

    atmLevel_ = Matrix(optionDates().size(), swapTenors().size());

    for (Size j = 0; j < atm->swapTenors().size(); ++j)
        for (Size i = 0; i < atm->optionDates().size(); ++i)
            atmLevel_[i][j] = sourceCube->atmStrike(atm->optionDates()[i],
                                                    atm->swapTenors()[j]);

    originalAtm_ = FlatExtrapolator2D(boost::make_shared<BilinearInterpolation>(
        atm->swapLengths().begin(), atm->swapLengths().end(),
        atm->optionTimes().begin(), atm->optionTimes().end(), atmLevel_));

    originalAtm_.enableExtrapolation();
}

boost::shared_ptr<SmileSection>
StickyBpVolSwaptionCube::smileSectionImpl(Time optionTime,
                                          Time swapLength) const {

    boost::shared_ptr<SmileSection> source =
        sourceCube_->smileSection(optionTime, swapLength);

    Real newAtm = source->atmLevel();

    QL_REQUIRE(newAtm != Null<Real>(),
               "source smile section does not provide atm level");

    Real mul = originalAtm_(optionTime, swapLength) / newAtm;
    Real add = 0.0;
    if(!atmVolatilitySpread_.empty()) {
        if(atmVolatilitySpread_->isValid()) {
            add = atmVolatilitySpread_->value() * mul;
        }
    }

    boost::shared_ptr<SmileSection> tmp =
        boost::make_shared<MultiplicativeSmileSection>(
            source, Handle<Quote>(boost::make_shared<SimpleQuote>(mul)),
            Handle<Quote>(boost::make_shared<SimpleQuote>(add)));
    return tmp;
}

Volatility StickyBpVolSwaptionCube::volatilityImpl(Time optionTime,
                                                   Time swapLength,
                                                   Rate strike) const {
    return smileSectionImpl(optionTime, swapLength)->volatility(strike);
}
}
