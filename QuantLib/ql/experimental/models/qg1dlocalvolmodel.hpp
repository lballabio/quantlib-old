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
#include <ql/indexes/swapindex.hpp>
#include <ql/math/integrals/integral.hpp>
#include <ql/math/integrals/simpsonintegral.hpp>
#include <ql/math/solvers1d/brent.hpp>
#include <ql/models/model.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

#include <boost/make_shared.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

namespace QuantLib {

class Qg1dLocalVolModel : public TermStructureConsistentModel {
  public:
    /*! the model is specified by a function \kappa(t) and a function g(t,x,y),
        with \kappa(t) = -h'(t) / h(t), \sigma_f(t,T) = g(t)h(T), the HJM
        specification
        df(t,T) = \sigma_f(t,T) ( ( \int_t^T sigma_f(t,u) du ) du + dW(t) )
        and
        dx = (y - \kappa x) dt + \sigma_f(t,t) dW
        dy = (\sigma_f(t,t)^2 - 2 \kappa y) dt
        x(0) = y(0) = 0 */
    Qg1dLocalVolModel(const Handle<YieldTermStructure> &yts);

    /* core interface, these methods must be implemented by derived classes
       the other virtual methods may be overwritten by more efficient versions
       applicable to the particular model specification. */
    virtual Real kappa(const Real t) const = 0;
    virtual Real g(const Real t, const Real x, const Real y) const = 0;

    virtual Real h(const Real t) const;

    /*! \int_t^T h(s) ds / h(t) */
    virtual Real G(const Real t, const Real T) const;

    virtual Real sigma_f(const Real t, const Real T, const Real x,
                         const Real y) const;

    Real zerobond(const Real T, const Real t, const Real x, const Real y,
                  const Handle<YieldTermStructure> &yts) const;

    /*! swap rate is calculated with forward = discount, no indexed coupons
        T0 is the start date of the swap, fixedTimes the payment times of
        the fixed leg and taus the year fractions of the fixed leg */
    Real swapRate(const Real T0, const Real t,
                  const std::vector<Real> &fixedTimes,
                  const std::vector<Real> &taus, const Real x, const Real y,
                  const Handle<YieldTermStructure> &yts) const;

    Real dSwapRateDx(const Real T0, const Real t,
                     const std::vector<Real> &fixedTimes,
                     const std::vector<Real> &taus, const Real x, const Real y,
                     const Handle<YieldTermStructure> &yts) const;

    /*! local volatility using yApprox und sInvX (see Piterbarg, equation 13.19
        and what follows immediately after that), the same comments as for
        the swap rate approximation apply */
    Real phi(const Real t, const Real s, const Real T0,
             const std::vector<Real> &fixedTimes, const std::vector<Real> &taus,
             const Handle<YieldTermStructure> &yts) const;

    /*! date based variants, only the forwarding curve from the swap index
        (if given) is used and no indexed coupons are used, see above */
    Real zerobond(const Date &maturiy, const Date &referenceDate, const Real x,
                  const Real y, const Handle<YieldTermStructure> &yts);

    Real swapRate(const Date &startDate, const Date &referenceDate,
                  const boost::shared_ptr<SwapIndex> &index,
                  const Period &tenor, const Real x, const Real y) const;

    Real dSwapRateDx(const Date &startDate, const Date &referenceDate,
                     const boost::shared_ptr<SwapIndex> &index,
                     const Period &tenor, const Real x, const Real y) const;

    /*! utilitiy function that fills T0, tau and a times vector based on
      a given swap index */
    void timesAndTaus(const Date &startDate,
                      const boost::shared_ptr<SwapIndex> &index,
                      const Period &tenor, Real &T0, std::vector<Real> &taus,
                      std::vector<Real> &times) const;

  protected:
    virtual Real yApprox(const Real t) const;
    /*! this is \overline{x(t)} in Piterbarg */
    // virtual Real xApprox(const Real t, const Real T0,
    //                      const std::vector<Real> &fixedTimes,
    //                      const std::vector<Real> &taus,
    //                      const Handle<YieldTermStructure> &yts) const;
    /*! approximate inversion of s with y = yApprox fixed */
    // virtual Real xi(const Real t, const Real T0,
    //                 const std::vector<Real> &fixedTimes,
    //                 const std::vector<Real> &taus,
    //                 const Handle<YieldTermStructure> &yts, const Real s)
    //                 const;

    /*! numerical inversion of s with y = yApprox fixed,
        i.e. this is X(t,s) in Piterbarg's notation */
    Real sInvX(const Real t, const Real T0, const std::vector<Real> &fixedTimes,
               const std::vector<Real> &taus,
               const Handle<YieldTermStructure> &yts, const Real s) const;

    /*! sigma_f(t,t,0,0)^2*h(t)^{-2},
            precondition (not checked) is t > 0 */
    virtual Real sigma_r_0_0_h_sqr(const Real t) const;

    boost::shared_ptr<Integrator> integrator_;

  private:
    Real sInvX_helper(const Real t, const Real T0,
                      const std::vector<Real> &fixedTimes,
                      const std::vector<Real> &taus,
                      const Handle<YieldTermStructure> &yts, const Real s,
                      const Real x) const;
};

// inline

inline Real Qg1dLocalVolModel::h(const Real t) const {
    return std::exp(-integrator_->operator()(
        boost::bind(&Qg1dLocalVolModel::kappa, this, _1), 0.0, t));
}

inline Real Qg1dLocalVolModel::G(const Real t, const Real T) const {
    return integrator_->operator()(boost::bind(&Qg1dLocalVolModel::h, this, _1),
                                   t, T);
}

inline Real Qg1dLocalVolModel::sigma_f(const Real t, const Real T, const Real x,
                                       const Real y) const {
    return g(t, x, y) * h(T);
}

inline Real Qg1dLocalVolModel::yApprox(const Real t) const {
    if (t < 1E-10)
        return 0.0;
    Real tmp = h(t);
    return tmp * tmp *
           integrator_->operator()(
               boost::bind(&Qg1dLocalVolModel::sigma_r_0_0_h_sqr, this, _1),
               0.0, t);
}

inline Real Qg1dLocalVolModel::sigma_r_0_0_h_sqr(const Real t) const {
    Real tmp = g(t, 0.0, 0.0); // this is sigma_f(t, t, 0.0, 0.0) / h(t);
    return tmp * tmp;
}

inline Real
Qg1dLocalVolModel::phi(const Real t, const Real s, const Real T0,
                       const std::vector<Real> &fixedTimes,
                       const std::vector<Real> &taus,
                       const Handle<YieldTermStructure> &yts) const {
    Real x = sInvX(t, T0, fixedTimes, taus, yts, s);
    Real y = yApprox(t);
    return dSwapRateDx(T0, t, fixedTimes, taus, x, y, yts);
}

inline Real Qg1dLocalVolModel::sInvX_helper(
    const Real t, const Real T0, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Handle<YieldTermStructure> &yts,
    const Real s, const Real x) const {
    Real y = yApprox(t);
    return swapRate(T0, t, fixedTimes, taus, x, y, yts) - s;
}

inline Real Qg1dLocalVolModel::sInvX(const Real t, const Real T0,
                                     const std::vector<Real> &fixedTimes,
                                     const std::vector<Real> &taus,
                                     const Handle<YieldTermStructure> &yts,
                                     const Real s) const {
    Brent b;
    boost::function<Real(Real)> f =
        boost::bind(&Qg1dLocalVolModel::sInvX_helper, this, t, T0, fixedTimes,
                    taus, yts, s, _1);
    return b.solve(f, 1E-7, 0.0, 0.01);
}

} // namespace QuantLib

#endif
