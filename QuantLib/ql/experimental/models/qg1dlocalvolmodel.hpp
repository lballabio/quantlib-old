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

/*! \file qg1dlocalvolmodel.hpp
    \brief base class for one factor quasi gaussian models with local
           volatility
*/

#ifndef quantlib_quasigaussian1d_model_hpp
#define quantlib_quasigaussian1d_model_hpp

#include <ql/handle.hpp>
#include <ql/math/integrals/integral.hpp>
#include <ql/math/integrals/simpsonintegral.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

#include <boost/make_shared.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

namespace QuantLib {

class Qg1dLocalVolModel {
  public:
    /*! the model is specified by a function \kappa(t) and a function g(t,x,y),
        with \kappa(t) = -h'(t) / h(t), \sigma_f(t,T) = g(t)h(T), the HJM
        specification
        df(t,T) = \sigma_f(t,T) ( ( \int_t^T sigma_f(t,u) du ) du + dW(t) )
        and
        dx = (y - \kappa x) dt + \sigma_f(t,t) dW
        dy = (\sigma_f(t,t)^2 - 2 \kappa y) dt
        x(0) = y(0) = 0 */
    Qg1dLocalVolModel(const boost::shared_ptr<Integrator> integrator = NULL);

    virtual Real kappa(const Real t) const = 0;
    virtual Real g(const Real t, const Real x, const Real y) const = 0;

    virtual Real h(const Real t) const;

    /*! \int_t^T h(s) ds / h(t) */
    virtual Real G(const Real t, const Real T) const;

    virtual Real sigma_f(const Real t, const Real T, const Real x,
                         const Real y) const;

    Real zerobond(const Real T, const Real t, const Real x, const Real y,
                  const Handle<YieldTermStructure> &yts) const;

  protected:
    virtual Real yApprox(const Real t) const;
    /*! sigma_f(t,t,0,0)^2*h(t)^{-2},
        precondition (not checked) is t > 0 */
    virtual Real sigma_r_0_0_h_sqr(const Real t) const;

  private:
    boost::shared_ptr<Integrator> integrator_;
};

// inline

Qg1dLocalVolModel::Qg1dLocalVolModel(
    const boost::shared_ptr<Integrator> integrator)
    : integrator_(integrator) {
    if (integrator_ == NULL) {
        // default integrator if none is given
        integrator_ = boost::make_shared<SimpsonIntegral>(1E-10, 100);
    }
}

Real Qg1dLocalVolModel::h(const Real t) const {
    return std::exp(integrator_->operator()(
        boost::bind(&Qg1dLocalVolModel::kappa, this, _1), 0.0, t));
}

Real Qg1dLocalVolModel::G(const Real t, const Real T) const {
    return integrator_->operator()(boost::bind(&Qg1dLocalVolModel::h, this, _1),
                                   t, T);
}

Real Qg1dLocalVolModel::sigma_f(const Real t, const Real T, const Real x,
                                const Real y) const {
    return g(t, x, y) * h(T);
}

Real Qg1dLocalVolModel::sigma_r_0_0_h_sqr(const Real t) const {
    Real tmp = sigma_f(t, t, 0.0, 0.0) / h(t);
    return tmp * tmp;
}

Real Qg1dLocalVolModel::yApprox(const Real t) const {
    if (t < 1E-10)
        return 0.0;
    Real tmp = h(t);
    return tmp * tmp *
           integrator_->operator()(
               boost::bind(&Qg1dLocalVolModel::sigma_r_0_0_h_sqr, this, _1),
               0.0, t);
}

Real Qg1dLocalVolModel::zerobond(const Real T, const Real t, const Real x,
                                 const Real y,
                                 const Handle<YieldTermStructure> &yts) const {
    Real tmp = G(t, T);
    return yts->discount(T) / yts->discount(t) *
           std::exp(-G(t, T) * x - 0.5 * tmp * tmp * y);
}

} // namespace QuantLib

#endif
