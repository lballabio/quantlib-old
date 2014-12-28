/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2008 StatPro Italia srl

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

/*! \file linearinterpolation.hpp
    \brief linear interpolation between discrete points
*/

#ifndef quantlib_linear_interpolation_hpp
#define quantlib_linear_interpolation_hpp

#include <ql/math/interpolation.hpp>
#include <vector>

namespace QuantLib {

namespace detail {

template <class I1, class I2, class T = Real>
class LinearInterpolationImpl
    : public Interpolation_t<T>::template templateImpl<I1, I2> {
  public:
    LinearInterpolationImpl(const I1 &xBegin, const I1 &xEnd, const I2 &yBegin)
        : Interpolation_t<T>::template templateImpl<I1, I2>(xBegin, xEnd, yBegin),
          primitiveConst_(xEnd - xBegin), s_(xEnd - xBegin) {}
    void update() {
        primitiveConst_[0] = 0.0;
        for (Size i = 1; i < Size(this->xEnd_ - this->xBegin_); ++i) {
            Real dx = this->xBegin_[i] - this->xBegin_[i - 1];
            s_[i - 1] = (this->yBegin_[i] - this->yBegin_[i - 1]) / dx;
            primitiveConst_[i] =
                primitiveConst_[i - 1] +
                dx * (this->yBegin_[i - 1] + 0.5 * dx * s_[i - 1]);
        }
    }
    T value(Real x) const {
        Size i = this->locate(x);
        return this->yBegin_[i] + (x - this->xBegin_[i]) * s_[i];
    }
    T primitive(Real x) const {
        Size i = this->locate(x);
        T dx = x - this->xBegin_[i];
        return primitiveConst_[i] + dx * (this->yBegin_[i] + 0.5 * dx * s_[i]);
    }
    T derivative(Real x) const {
        Size i = this->locate(x);
        return s_[i];
    }
    T secondDerivative(Real) const { return 0.0; }

  private:
    std::vector<T> primitiveConst_, s_;
};
}

//! %Linear interpolation between discrete points
template <class T> class LinearInterpolation_t : public Interpolation_t<T> {
  public:
    /*! \pre the \f$ x \f$ values must be sorted. */
    template <class I1, class I2>
    LinearInterpolation_t(const I1 &xBegin, const I1 &xEnd, const I2 &yBegin) {
        this->impl_ = boost::shared_ptr<typename Interpolation_t<T>::Impl>(
            new detail::LinearInterpolationImpl<I1, I2, T>(xBegin, xEnd,
                                                           yBegin));
        this->impl_->update();
    }
};

typedef LinearInterpolation_t<Real> LinearInterpolation;

//! %Linear-interpolation factory and traits
template <class T> class Linear_t {
  public:
    template <class I1, class I2>
    Interpolation_t<T> interpolate(const I1 &xBegin, const I1 &xEnd,
                                   const I2 &yBegin) const {
        return LinearInterpolation_t<T>(xBegin, xEnd, yBegin);
    }
    static const bool global = false;
    static const Size requiredPoints = 2;
};

typedef Linear_t<Real> Linear;
}

#endif
