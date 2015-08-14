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

#include <ql/experimental/models/cclgmpiecewise.hpp>

namespace QuantLib {

namespace detail {

CcLgmPiecewise::CcLgmPiecewise(
    const std::vector<boost::shared_ptr<
        LgmFxParametrization<LgmFxPiecewiseSigma> > > &fxParametrizations,
    const std::vector<
        boost::shared_ptr<LgmParametrization<LgmPiecewiseAlphaConstantKappa> > >
        &lgmParametrizations,
    const Matrix &correlation)
    : CcLgmParametrization<CcLgmPiecewise, LgmFxPiecewiseSigma,
                           LgmPiecewiseAlphaConstantKappa>(fxParametrizations,
                                                           lgmParametrizations),
      correlation_(correlation), n_(fxParametrizations.size()) {
    QL_REQUIRE(correlation_.rows() == 2 * n_ + 1 &&
                   correlation_.columns() == 2 * n_ + 1,
               "correlation matrix is "
                   << correlation_.rows() << " x " << correlation_.columns()
                   << ", expected " << (2 * n_ + 1) << " x " << (2 * n_ + 1));
}

} // namespace detail

} // namespace QuantLib
