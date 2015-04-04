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

#include <ql/experimental/xva/gaussian1dscenariogenerator.hpp>
#include <ql/experimental/models/gaussian1dyieldtermstructure.hpp>
#include <ql/processes/forwardmeasureprocess.hpp>
#include <boost/make_shared.hpp>

namespace QuantLib {

Gaussian1dSingleCurveScenarioGenerator::Gaussian1dSingleCurveScenarioGenerator(
    const boost::shared_ptr<Gaussian1dModel> &model, const unsigned long seed)
    : ScenarioGenerator<YieldTermStructure>(
          model->termStructure()->calendar(),
          model->termStructure()->dayCounter()),
      model_(model) {
    modelTime_ = time();
    modelState_ = model_->stateProcess()->x0();
    mt_ = boost::make_shared<MersenneTwisterUniformRng>(seed);
    icrng_ = boost::make_shared<InverseCumulativeRng<MersenneTwisterUniformRng,
                                                     InverseCumulativeNormal> >(
        *mt_);
}

bool Gaussian1dSingleCurveScenarioGenerator::nextPath() {
    ScenarioGenerator<YieldTermStructure>::nextPath();
    modelTime_ = time();
    return true;
}

const Date
Gaussian1dSingleCurveScenarioGenerator::advance(const Period &suggestedStep) {
    ScenarioGenerator<YieldTermStructure>::advance(suggestedStep);
    Real newModelTime_ = time();
    boost::shared_ptr<ForwardMeasureProcess> fmp =
        boost::dynamic_pointer_cast<ForwardMeasureProcess>(
            model_->stateProcess());
    if (fmp != NULL)
        if (newModelTime_ > fmp->getForwardMeasureTime())
            validHorizonDate_ = false;
    if (validHorizonDate_) {
        Real dt = newModelTime_ - modelTime_;
        Real dw = std::sqrt(dt) * icrng_->next().value;
        modelState_ =
            model_->stateProcess()->evolve(modelTime_, modelState_, dt, dw);
    }
    modelTime_ = newModelTime_;
    return horizonDate();
}

const boost::shared_ptr<YieldTermStructure>
Gaussian1dSingleCurveScenarioGenerator::state() const {
    return boost::make_shared<Gaussian1dYieldTermStructure>(
        model_, modelTime_, model_->y(modelState_, modelTime_));
}
} // namespace QuantLib
