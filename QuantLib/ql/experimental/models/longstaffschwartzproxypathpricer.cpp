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

#include <ql/experimental/models/longstaffschwartzproxypathpricer.hpp>

namespace QuantLib {

LongstaffSchwartzProxyPathPricer::LongstaffSchwartzProxyPathPricer(
    const TimeGrid &times,
    const boost::shared_ptr<EarlyExercisePathPricer<PathType> > &pricer,
    const boost::shared_ptr<YieldTermStructure> &termStructure)
    : LongstaffSchwartzPathPricer<PathType>(times, pricer, termStructure),
      coeffItm_(times.size() - 2), coeffOtm_(times.size() - 2) {}

void LongstaffSchwartzProxyPathPricer::post_processing(
    const Size i, const std::vector<StateType> &x_itm,
    const std::vector<Real> &y_itm, const std::vector<StateType> &x_otm,
    const std::vector<Real> &y_otm) {

    if (v_.size() <= x_itm.size()) {
        coeffItm_[i - 1] =
            GeneralLinearLeastSquares(x_itm, y_itm, v_).coefficients();
    } else {
        // see longstaffschwartzpricer.hpp
        coeffItm_[i - 1] = Array(v_.size(), 0.0);
    }

    if (v_.size() <= x_otm.size()) {
        coeffOtm_[i - 1] =
            GeneralLinearLeastSquares(x_otm, y_otm, v_).coefficients();
    } else {
        // see longstaffschwartzpricer.hpp
        coeffOtm_[i - 1] = Array(v_.size(), 0.0);
    }
}

} // namespace QuantLib
