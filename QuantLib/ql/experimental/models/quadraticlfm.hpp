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

/*! \file quadraticlfm.hpp
    \brief deterministic lfm with quadratic local vol
*/

#ifndef quantlib_quadraticlfm_hpp
#define quantlib_quadraticlfm_hpp

#include <ql/types.hpp>
#include <ql/errors.hpp>
#include <ql/utilities/disposable.hpp>
#include <ql/math/array.hpp>

#include <vector>
#include <iostream>

namespace QuantLib {

class QuadraticLfm {

  public:
    /*! sigma, b and c are stored as references, i.e. if they change outside this class,
        the class reflects this change in computed values */
    QuadraticLfm(const std::vector<Real> &rateTimes,
                 const std::vector<Real> &initialForwards,
                 const std::vector<std::vector<std::vector<Real> > > &sigma,
                 const std::vector<std::vector<Real> > &b,
                 const std::vector<std::vector<Real> > &c);

    /* markovian projection local volatility \eta(t,s) */
    Disposable<Array> eta(const Size n, const Size m, const Size step, const Real t,
             const Array& s);

    Real eta(const Size n, const Size m, const Size step, const Real t, const Real s);

    /* dS_{n,m} / dL_i freezed at time zero */
    Real dSdL(const Size n, const Size m, const Size step, const Size i,
              const Real h = 1E-5);

    /* S_{n,m}(0) */
    Real S(const Size n, const Size m, const Size step = 1);

    /* P(0,t_n,t_m) */
    Real P(const Size n, const Size m);

    /* t_{q-1} <= t < t_q */
    int q(const Real t);

  private:
    const std::vector<Real> rateTimes_;
    std::vector<Real> initialForwards_;
    const std::vector<std::vector<std::vector<Real> > > &sigma_;
    const std::vector<std::vector<Real> > &b_, &c_;
    Size N_, K_;
};
}

#endif
