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

/*! \file lgmparametrization.hpp
    \brief basis class for zeta and H functions
*/

#ifndef quantlib_lgm_parametrization_hpp
#define quantlib_lgm_parametrization_hpp

#include <ql/patterns/curiouslyrecurring.hpp>
#include <ql/types.hpp>
#include <ql/errors.hpp>

namespace QuantLib {
template <class T>
class LgmParametrization : public CuriouslyRecurringTemplate<T> {
  public:
    //! inspectors
    void update() const;
    const Real zeta(const Time t) const;
    const Real alpha(const Time t) const;
    const Real H(const Time t) const;
    const Real Hprime(const Time t) const;

    //! corresponding Hull White parameters
    const Real HullWhiteSigma(Time t) const;
    const Real HullWhiteKappa(Time t) const;

    //! constructor with step for numerical differentiation
    LgmParametrization() : h_(1E-6) {}

    //! interface
    const void updateImpl() const {}         // optional to implement (.)
    const Real zetaImpl(const Time) const;   // must be implemented   (*)
    const Real alphaImpl(const Time) const;  // (.)
    const Real HImpl(const Time) const;      // (*)
    const Real HprimeImpl(const Time) const; // (.)

  private:
    const Real h_;
};

// inline

template <class T> inline void LgmParametrization<T>::update() const {
    return this->impl().updateImpl();
}

template <class T>
inline const Real LgmParametrization<T>::zeta(const Time t) const {
    return this->impl().zetaImpl(t);
}

template <class T>
inline const Real LgmParametrization<T>::alpha(const Time t) const {
    return this->impl().alphaImpl(t);
}

template <class T>
inline const Real LgmParametrization<T>::H(const Time t) const {
    return this->impl().HImpl(t);
}

template <class T>
inline const Real LgmParametrization<T>::Hprime(const Time t) const {
    return this->impl().HprimeImpl(t);
}

// default implementations

template <class T> inline
const Real LgmParametrization<T>::zetaImpl(const Time t) const {
    QL_FAIL("zeta implemnentation not provided");
}

template <class T> inline
const Real LgmParametrization<T>::alphaImpl(const Time t) const {
    return (zeta(t + 0.5 * h_) - zeta(t - 0.5 * h_)) / h_;
}

template <class T> inline
const Real LgmParametrization<T>::HImpl(const Time t) const {
    QL_FAIL("H implementation not provided");
}

template <class T> inline
const Real LgmParametrization<T>::HprimeImpl(const Time t) const {
    return (Hprime(t + 0.5 * h_) - Hprime(t - 0.5 * h_)) / h_;
}

} // namespace QuantLib

#endif
