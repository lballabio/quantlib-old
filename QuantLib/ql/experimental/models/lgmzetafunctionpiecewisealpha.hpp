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

/*! \file lgmzetafunctionpiecewisealpha.hpp
    \brief zeta function with piecewise alpha
*/

#ifndef quantlib_lgm_zetafunctionpiecewisealpha_hpp
#define quantlib_lgm_zetafunctionpiecewisealpha_hpp

#include <ql/experimental/models/lgmzetafunction.hpp>
#include <vector>

namespace QuantLib {

class LgmZetaFunctionPiecewiseAlpha
    : public LgmZetaFunction<LgmZetaFunctionPiecewiseAlpha> {
  public:
    LgmZetaFunctionPiecewiseAlpha(const std::vector<Real> &times,
                                  const std::vector<Real> &alphas);

    const Real zetaImpl(const Time t) const;
    const Real alphaImpl(const Time t) const;
    const void updateImpl() const;

  private:
    const std::vector<Real> &times_, &alphas_;
    mutable std::vector<Real> zetas_;
};

} // namespace QuantLib

#endif
