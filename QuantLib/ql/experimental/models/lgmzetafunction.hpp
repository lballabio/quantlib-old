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

/*! \file lgmzetafunction.hpp
    \brief basis class for zeta function lgm model parametrization
*/

#ifndef quantlib_lgm_zetafunction_hpp
#define quantlib_lgm_zetafunction_hpp

#include <ql/patterns/curiouslyrecurring.hpp>
#include <ql/types.hpp>
#include <ql/errors.hpp>

namespace QuantLib {

template <class T>
class LgmZetaFunction : public CuriouslyRecurringTemplate<T> {
  public:
    void update() const;
    const Real zeta(const Time t) const;
    const Real alpha(const Time t) const;

  protected:
    LgmZetaFunction() : h_(1E-6) {}
    const void updateImpl() const {}
    const Real zetaImpl(const Time) const;
    const Real alphaImpl(const Time) const;

  private:
    const Real h_;
};

// inline

template <class T> void LgmZetaFunction<T>::update() const {
    return this->impl().updateImpl();
}

template <class T> const Real LgmZetaFunction<T>::zeta(const Time t) const {
    return this->impl().zetaImpl(t);
}

template <class T> const Real LgmZetaFunction<T>::alpha(const Time t) const {
    return this->impl().alphaImpl(t);
}

template <class T> const Real LgmZetaFunction<T>::zetaImpl(const Time t) const {
    QL_FAIL("zeta implemnentation not provided");
}

template <class T>
const Real LgmZetaFunction<T>::alphaImpl(const Time t) const {
    return (zeta(t + 0.5 * h_) - zeta(t - 0.5 * h_)) / h_;
}

} // namespace QuantLib

#endif
