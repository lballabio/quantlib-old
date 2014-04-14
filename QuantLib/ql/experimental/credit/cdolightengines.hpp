/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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

/*! \file cdolightengines.hpp
    \brief pricing engine(s) for cdo light instruments
*/

#ifndef quantlib_cdolight_engines_hpp
#define quantlib_cdolight_engines_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/defaulttermstructure.hpp>
#include <ql/math/randomnumbers/rngtraits.hpp>
#include <ql/pricingengine.hpp>
#include <ql/experimental/credit/cdolight.hpp>

namespace QuantLib {

class CdoLightMcEngine
    : public GenericEngine<CdoLight::arguments, CdoLight::results> {

  public:
    CdoLightMcEngine(
        const Handle<YieldTermStructure> &yieldCurve,
        const std::vector<Handle<DefaultProbabilityTermStructure>> &
            defaultCurves,
        const std::vector<Handle<Quote>> &recoveryRates,
        const Handle<Quote> &correlaiton, Size paths, BigNatural seed);
    void calculate() const;

  private:
    void generateDefaultTimes() const;
    Real pathPayoff() const;
    const Handle<YieldTermStructure> yieldCurve_;
    const std::vector<Handle<DefaultProbabilityTermStructure>> defaultCurves_;
    const std::vector<Handle<Quote>> recoveryRates_;
    const Handle<Quote> correlation_;
    Size paths_;
    BigNatural seed_;
    mutable PseudoRandom::rsg_type rsg_;
    mutable std::vector<Size> defaultIndexes_;
    mutable std::vector<Real> periodTimes_;
    mutable std::vector<std::vector<Real>> periodDefaultProbs_;
    mutable Size firstPeriod_;
    mutable Real attachment_, detachment_, poolNominal_;
};
}

#endif
