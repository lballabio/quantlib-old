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

/*! \file gaussian1dswaptionvolatility.hpp
    \brief swaption cube that holds atm bp vol constant
*/

#ifndef quantlib_sticky_bpvol_swaptioncube_hpp
#define quantlib_sticky_bpvol_swaptioncube_hpp

#include <ql/experimental/volatility/stickybpvolswaptioncube.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>

namespace QuantLib {

class Quote;

/*! cube that holds the atm bp vol constant under rate shifts and
    adjusts the smile vols by multiplying them with the ratio of
    new and old atm vol. If the atm vol is shifted via the given
    spread, the same adjustment for the smile is applied. */

class StickyBpVolSwaptionCube : public SwaptionVolatilityCube {
  public:
    StickyBpVolSwaptionCube(
        const boost::shared_ptr<SwaptionVolatilityCube> &sourceCube,
        Handle<Quote> atmVolatilitySpread = Handle<Quote>());

  protected:
    boost::shared_ptr<SmileSection> smileSectionImpl(Time, Time) const;
    Volatility volatilityImpl(Time, Time, Rate) const;

  private:
    Handle<Quote> atmVolatilitySpread_;
    Interpolation2D originalAtm_;
    Matrix atmLevel_;
};
}

#endif
