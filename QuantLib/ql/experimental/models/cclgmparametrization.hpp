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

/*! \file cclgmparametrization.hpp
    \brief cross currency lgm parametrization
*/

#ifndef quantlib_cclgm_parametrization_hpp
#define quantlib_cclgm_parametrization_hpp

#include <ql/math/integrals/integral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/patterns/curiouslyrecurring.hpp>
#include <ql/experimental/models/lgmparametrization.hpp>
#include <ql/experimental/models/lgmfxparametrization.hpp>

#include <vector>

#include <boost/make_shared.hpp>
#include <boost/bind.hpp>

namespace QuantLib {

template <class Impl, class ImplFx, class ImplLgm>
class CcLgmParametrization : public CuriouslyRecurringTemplate<Impl> {
  public:
    //! inspectors
    void update() const;

    //! constructor (with step size for numerical differentiation)
    CcLgmParametrization(
        const std::vector<boost::shared_ptr<LgmFxParametrization<ImplFx> > >
            &fxParametrizations,
        const std::vector<boost::shared_ptr<LgmParametrization<ImplLgm> > >
            &lgmParametrizations)
        : h_(1E-6), fxParametrizations_(fxParametrizations),
          lgmParametrizations_(lgmParametrizations) {

        QL_REQUIRE(fxParametrizations.size() == lgmParametrizations.size(),
                   "number of fx parametrizations ("
                       << fxParametrizations.size()
                       << ") and lgm parametrizations ("
                       << lgmParametrizations.size() << ") are different");

        integrator_ =
            boost::make_shared<GaussLobattoIntegral>(1000, 1E-8, 1E-8, true);
    }

    //! inspectors

    const Real alpha_i_alpha_j(const Size i, const Size j, const Real t) const;
    const Real H_i_alpha_i_alpha_j(const Size i, const Size j,
                                   const Real t) const;
    const Real H_i_H_j_alpha_i_alpha_j(const Size i, const Size j,
                                       const Real t) const;
    const Real H_i_alpha_i_sigma_j(const Size i, const Size j,
                                   const Real t) const;

    const Real int_alpha_i_alpha_j(const Size i, const Size j, const Real a,
                                   const Real b) const;
    const Real int_H_i_alpha_i_alpha_j(const Size i, const Size j, const Real a,
                                       const Real b) const;
    const Real int_H_i_H_j_alpha_i_alpha_j(const Size i, const Size j,
                                           const Real a, const Real b) const;
    const Real int_H_i_alpha_i_sigma_j(const Size i, const Size j, const Real a,
                                       const Real b) const;

    //! interface (optional, default implementation uses numerical integration)
    const Real int_alpha_i_alpha_j_impl(const Size i, const Size j,
                                        const Real a, const Real b) const;
    const Real int_H_i_alpha_i_alpha_j_impl(const Size i, const Size j,
                                            const Real a, const Real b) const;
    const Real int_H_i_H_j_alpha_i_alpha_j_impl(const Size i, const Size j,
                                                const Real a,
                                                const Real b) const;
    const Real int_H_i_alpha_i_sigma_j_impl(const Size i, const Size j,
                                            const Real a, const Real b) const;

  private:
    const Real h_;
    std::vector<boost::shared_ptr<LgmFxParametrization<ImplFx> > >
        fxParametrizations_;
    std::vector<boost::shared_ptr<LgmParametrization<ImplLgm> > >
        lgmParametrizations_;
    boost::shared_ptr<Integrator> integrator_;
};

// inline

template <class Impl, class ImplFx, class ImplLgm>
inline const Real CcLgmParametrization<Impl, ImplFx, ImplLgm>::alpha_i_alpha_j(
    const Size i, const Size j, const Real t) const {
    return lgmParametrizations_[i]->alpha(t) *
           lgmParametrizations_[j]->alpha(t);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::H_i_alpha_i_alpha_j(
    const Size i, const Size j, const Real t) const {
    return lgmParametrizations_[i]->H(t) * lgmParametrizations_[i]->alpha(t) *
           lgmParametrizations_[j]->alpha(t);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::H_i_H_j_alpha_i_alpha_j(
    const Size i, const Size j, const Real t) const {
    return lgmParametrizations_[i]->H(t) * lgmParametrizations_[j]->H(t) *
           lgmParametrizations_[i]->alpha(t) *
           lgmParametrizations_[j]->alpha(t);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::H_i_alpha_i_sigma_j(
    const Size i, const Size j, const Real t) const {
    return lgmParametrizations_[i]->H(t) * lgmParametrizations_[j]->H(t) *
           lgmParametrizations_[i]->alpha(t) * fxParametrizations_[j]->sigma(t);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_alpha_i_alpha_j(
    const Size i, const Size j, const Real a, const Real b) const {
    return this->impl().int_alpha_i_alpha_j_impl(i, j, a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_alpha_i_alpha_j(
    const Size i, const Size j, const Real a, const Real b) const {
    return this->impl().int_H_i_alpha_i_alpha_j_impl(i, j, a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_H_j_alpha_i_alpha_j(
    const Size i, const Size j, const Real a, const Real b) const {
    return this->impl().int_H_i_H_j_alpha_i_alpha_j_impl(i, j, a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_alpha_i_sigma_j(
    const Size i, const Size j, const Real a, const Real b) const {
    return this->impl().int_H_i_alpha_i_alpha_j_impl(i, j, a, b);
}

// default implementation

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_alpha_i_alpha_j_impl(
    const Size i, const Size j, const Real a, const Real b) const {

    return integrator_(
        boost::bind(
            &CcLgmParametrization<Impl, ImplFx, ImplLgm>::alpha_i_alpha_j, i, j,
            _1),
        a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_alpha_i_alpha_j_impl(
    const Size i, const Size j, const Real a, const Real b) const {
    return integrator_(
        boost::bind(
            &CcLgmParametrization<Impl, ImplFx, ImplLgm>::H_i_alpha_i_alpha_j,
            i, j, _1),
        a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_H_j_alpha_i_alpha_j_impl(
    const Size i, const Size j, const Real a, const Real b) const {
    return integrator_(
        boost::bind(&CcLgmParametrization<Impl, ImplFx,
                                          ImplLgm>::H_i_H_j_alpha_i_alpha_j,
                    i, j, _1),
        a, b);
}

template <class Impl, class ImplFx, class ImplLgm>
inline const Real
CcLgmParametrization<Impl, ImplFx, ImplLgm>::int_H_i_alpha_i_sigma_j_impl(
    const Size i, const Size j, const Real a, const Real b) const {
    return integrator_(
        boost::bind(
            &CcLgmParametrization<Impl, ImplFx, ImplLgm>::H_i_alpha_i_sigma_j,
            i, j, _1),
        a, b);
}

} // namespace QuantLib

#endif
