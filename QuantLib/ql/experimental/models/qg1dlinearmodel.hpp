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

/*! \file Qg1dLinearModel.hpp
    \brief quasi gaussian 1d model with linear local volatility
*/

#ifndef quantlib_quasigaussian1d_linear_model_hpp
#define quantlib_quasigaussian1d_linear_model_hpp

#include <ql/experimental/models/qg1dlocalvolmodel.hpp>
#include <ql/models/model.hpp>

namespace QuantLib {

class Qg1dLinearModel
    : public Qg1dLocalVolModel,
      public TermStructureConsistentModel,
      public virtual Observer /* delete if deriving from CalibratedModel */
{
  public:
    /*! the model is specified by
        \sigma_f(t,t,x,y) = \lambda(t) * ( \alpha(t) + \beta(t) * x ),
        h(t) = e^{-\int_0^t \kappa(s) ds,
        g(t,x,y) = sigma_f(t,t,x,y) / h(t),
        with \lambda, \alpha, \beta, \kappa stepwise constant on
        a common grid */
    Qg1dLinearModel(const Handle<YieldTermStructure> &yts,
                          const std::vector<Date> stepDates,
                          const std::vector<Real> &lambda,
                          const std::vector<Real> &alpha,
                          const std::vector<Real> &beta,
                          const std::vector<Real> &kappa);

    Real kappa(const Real t) const;
    Real g(const Real t, const Real x, const Real y) const;

    Real lambda(const Real t) const;
    Real alpha(const Real t) const;
    Real beta(const Real t) const;

    void update();

  private:
    void updateTimes() const;
    void initialize();

    mutable std::vector<Real> volsteptimes_;
    mutable Array volsteptimesArray_;

    const Handle<YieldTermStructure> yts_;
    const std::vector<Date> stepDates_;
    const std::vector<Real> lambda_, alpha_, beta_, kappa_;
};

} // namespace QuantLib

#endif
