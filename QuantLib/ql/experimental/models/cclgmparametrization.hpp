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

namespace QuantLib {

template <class Impl>
class CcLgmParametrization : public CuriouslyRecurringTemplate<Impl> {
  public:
    //! inspectors
    void update() const;

    //! constructor (with step size for numerical differentiation)
    CcLgmParametrization(const std::vector<const boost::shared_ptr<
                             LgmFxParametrization<Impl::fx_parametrization> > >
                             &fxParametrizations,
                         const std::vector<const boost::shared_ptr<
                             LgmParametrization<Impl::lgm_parametrization> > >
                             &lgm_parametrization)
        : fxParametrizations_(fxParametrizations),
          lgmParametrizations_(lgmParametrizations), h_(1E-6) {}

    //! interface
    const Real int_alpha_i_alpha_j(const Size i, const Size j, const Real a,
                                   const Real b);
    const Real int_H_i_alpha_i_alpha_j(const Size i, const Size j, const Real a,
                                       const Real b);
    const Real int_H_i_H_j_alpha_i_alpha_j(const Size i, const Size j,
                                           const Real a, const Real b);
    const Real int_H_i_alpha_i_sigma_j(const Size i, const Size j, const Real a,
                                       const Real b);

  private:
    const Real h_;
    std::vector<const boost::shared_ptr<
        LgmFxParametrization<Impl::fx_parametrization> > > fxParametrizations_;
    std::vector<const boost::shared_ptr<
        LgmParametrization<Impl::lgm_parametrization> > > lgmParametrizations_;
};

} // namespace QuantLib

#endif
