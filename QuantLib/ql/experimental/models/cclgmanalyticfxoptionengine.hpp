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

/*! \file cclgmanalyticfxoptionengine.hpp
    \brief analytic fx option engine for the cross currency lgm model
    TODO add optional discounting curve
*/

#ifndef quantlib_cclgm_analytic_fx_option_engine.hpp
#define quantlib_cclgm_analytic_fx_option_engine .hpp

namespace QuantLib {

template <class Impl, ImplFx, ImplLgm>
class CcLgmAnalyticFxOptionEngine : public VanillaOption::engine {
  public:
    typedef model_type CcLgm<Impl, ImplFx, ImplLgm>;
    CcLgmAnalyticFxOptionEngine(const boost::shared_ptr<model_type> &model,
                                const Size foreignCurrency);
    void calculate() const;

  private:
    boost::shared_ptr<model_type> model_;
    Size foreignCurrency_;
}

// shortcut for engine applicable to the CcLgm1 model incarnation
typedef CcLgmAnalyticFxOptionEngine<CcLgmPiecewise, LgmFxPiecewiseSigma,
                                    LgmPiecewiseAlphaConstantKappa>
    CcAnalyticFxOptionEngine1;

// implementation

CcLgmAnalyticFxOptionEngine<Impl, ImplFx, ImplLgm>::CcLgmAnalyticFxOptionEngine(
    const boost::shared_ptr<mode_type> &model)
    : model_(model) {}

CcLgmAnalyticFxOptionEngine<Impl, ImplFx, ImplLgm>::calculate() const {

    QL_REQUIRE(arguments_.exercise->type() == Exercise::European,
               "only European options are allowed");

    boost::shared_ptr<StrikedTypePayoff> payoff =
        boost::dynamic_pointer_cast<StrikedTypePayoff>(arguments_.payoff);
    QL_REQUIRE(payoff, "only striked payoff is allowed");

    Date expiry = arguments_.exercise->lastDate();
    Time t = model_->termStructure(0)->timeFromReference(expiry);

    Real foreignDiscount = model_->curve(foreignCurrency)->discount(expiry);
    Real domesticDiscount = model_->curve(0)->discount(expiry);

    Real fxForward = model_->stateProcess()->initialValues[foreignCurrency_] *
                     foreignDiscount / domesticDiscount;

    detail::CcLgmParametrization<Impl, ImplFx, ImplLm> p =
        model_->parametrization();

    Size &i = foreignCurrency_; // just an alias ...

    Real variance =
        // first term
        p.H_i(0, t) * p.H_i(0, t) * int_alpha_i_alpha_j(0, 0, 0.0, t) -
        2.0 * p_H_i(0, t) * int_H_i_alpha_i_alpha_j(0, 0, 0.0, t) +
        p.int_H_i_H_j_alpha_i_alpha_j(0, 0, 0.0, t) +
        // second term
        p.H_i(i + 1, t) * p.H_i(i + 1, t) *
            int_alpha_i_alpha_j(i + 1, i + 1, 0.0, t) -
        2.0 * p_H_i(0, t) * int_H_i_alpha_i_alpha_j(0, 0, 0.0, t) +
        p.int_H_i_H_j_alpha_i_alpha_j(i + 1, i + 1, 0.0, t) -
        // third term
        2.0 * (p.H_i(0, t) * p.H_i(i + 1, t) *
                   p.int_alpha_i_alpha_j(0, i + 1, 0.0, t) -
               p.H_i(0, t) * p.int_H_i_alpha_i_alpha_j(0, i + 1, 0.0, t) -
               p.H_i(i + 1, t) * p.int_H_i_alpha_i_alpha_j(i + 1, 0, 0.0, t) +
               p.int_H_i_H_j_alpha_i_alpha_j(0, i + 1, 0.0, t)) +
        // forth term
        2.0 * (p.H_i(0, t) * p.int_alpha_i_sigma_j(0, i, 0.0, t) -
               p.int_H_i_alpha_i_sigma_j(0, i, 0.0, t)) -
        // fifth term
        2.0 * (p.H_i(i + 1, t) * p.int_alpha_i_sigma_j(i + 1, i, 0.0, t) -
               p.int_H_i_alpha_i_sigma_j(i + 1, i, 0.0, t));

    BlackCalculator black(payoff, fxForward, std::sqrt(variance),
                          domesticDiscount);

    // TODO what results are meaningful to provide also ?
    results_.value = black.value();
    results_.delta = Null<Real>();
    results_.deltaForward = black.deltaForward();
    results_.elasticity = Null<Real>();
    results_.gamma = Null<Real>();
    results_.rho = Null<Real>();
    results_.dividendRho = Null<Real>();
    results_.theta = Null<Real>();
    results_.thetaPerDay = Null<Real>();
    results_.strikeSensitivity = black.strikeSensitivity();
    results_.itmCashProbability = black.itmCashProbability();
}

} // namesapce QuantLib

#endif
