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

/*! \file gaussian1dscenariogenerator.hpp
    \brief single curve scenario generator based on a gaussian1d model
*/

#ifndef quantlib_xva_gaussian1dscenariogenerator_hpp
#define quantlib_xva_gaussian1dscenariogenerator_hpp

#include <ql/experimental/xva/scenariogenerator.hpp>
#include <ql/experimental/models/gaussian1dmodel.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/math/randomnumbers/mt19937uniformrng.hpp>
#include <ql/math/randomnumbers/inversecumulativerng.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

namespace QuantLib {

class Gaussian1dSingleCurveScenarioGenerator
    : public ScenarioGenerator<YieldTermStructure> {
  public:
    Gaussian1dSingleCurveScenarioGenerator(
        const boost::shared_ptr<Gaussian1dModel> &model,
        const unsigned long seed = 0);

    bool nextPath();
    const Date advance(const Period &suggestedStep = 1 * Days);
    const boost::shared_ptr<YieldTermStructure> state() const;

  private:
    boost::shared_ptr<Gaussian1dModel> model_;
    Real modelTime_, modelState_;
    boost::shared_ptr<MersenneTwisterUniformRng> mt_;
    boost::shared_ptr<InverseCumulativeRng<MersenneTwisterUniformRng,
                                           InverseCumulativeNormal> > icrng_;
};

} // namespace QuantLib

#endif
