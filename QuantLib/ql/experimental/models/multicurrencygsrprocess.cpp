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

#include <ql/experimental/models/multicurrencygsrprocess.hpp>

namespace QuantLib {

MultiCurrencyGsrProcess::MultiCurrencyGsrProcess(
    const std::vector<boost::shared_ptr<StochasticProcess1D> >
        &fx_gsr_processes,
    const Matrix &correlation)
    : StochasticProcessArray(fx_gsr_processes, correlation),
      correlation_(correlation) {

    n_ = (fx_gsr_processes.size() - 1) / 2;

    QL_REQUIRE(2 * n_ + 1 == fx_gsr_processes.size(),
               "2n+1 processes required (n is "
                   << n_ << ", but number of processes is "
                   << fx_gsr_processes.size());

    for (Size i = 0; i < n_; ++i) {
        boost::shared_ptr<GeneralizedBlackScholesProcess> tmp =
            boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                fx_gsr_processes[i]);
        QL_REQUIRE(
            tmp != NULL,
            "processes #0...(n-1) must be generalized black scholes processes, "
            "#" << i
                << " is not, n is "
                << n_);
    }

    for (Size i = n_; i < 2 * n_ + 1; ++i) {
        boost::shared_ptr<GsrProcessRiskNeutral> tmp =
            boost::dynamic_pointer_cast<GsrProcessRiskNeutral>(
                fx_gsr_processes[i]);
        QL_REQUIRE(tmp != NULL,
                   "processes #n...(2n) must be (risk neutral) gsr processes, #"
                       << i << " is not, n is " << n_);
    }
}

Disposable<Array> MultiCurrencyGsrProcess::drift(Time t, const Array &x) const {
    Array tmp(size());
    for (Size i = 0; i < size(); ++i) {
        tmp[i] = processes_[i]->drift(t, x[i]);
    }
    // drift adjustment due to foreign => domestic measure change
    for (Size i = n_ + 1; i < 2 * n_ + 1; ++i) {
        tmp[i] -= correlation_[i][i - (n_ + 1)] *
                  processes_[i]->diffusion(t, x[i]) *
                  processes_[i - (n_ + 1)]->diffusion(t, x[i - (n_ + 1)]);
    }
    return tmp;
}

Disposable<Array> MultiCurrencyGsrProcess::expectation(Time t0, const Array &x0,
                                                       Time dt) const {
    Array tmp(size());
    for (Size i = 0; i < size(); ++i)
        tmp[i] = processes_[i]->expectation(t0, x0[i], dt);
    return tmp;
    // adjustment due to foreign => domestic measure change
    Matrix c = covariance(t0, x0, dt);
    for (Size i = n_ + 1; i < 2 * n_ + 1; ++i) {
        tmp[i] -= c[i][i - (n_ + 1)];
    }
}

Disposable<Array> MultiCurrencyGsrProcess::evolve(Time t0, const Array &x0,
                                                  Time dt,
                                                  const Array &dw) const {

    const Array dz = sqrtCorrelation_ * dw;

    Array tmp(size());
    for (Size i = 0; i < size(); ++i)
        tmp[i] = processes_[i]->evolve(t0, x0[i], dt, dz[i]);
    // adjustment due to foreign => domestic measure change
    Matrix c = covariance(t0, x0, dt);
    for (Size i = n_ + 1; i < 2 * n_ + 1; ++i) {
        tmp[i] -= c[i][i - (n_ + 1)];
    }
    return tmp;
}

} // namespace QuantLib
