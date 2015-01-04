/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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


#include <ql/experimental/models/quadraticlfm.hpp>

#include <algorithm>

namespace QuantLib {

int QuadraticLfm::q(const Real t) {
    QL_REQUIRE(t>=0.0 && t< rateTimes_[n_-2], "at time " << t << " all forwards are dead");
    return std::upper_bound(rateTimes_.begin(), rateTimes_.end(), t) -
           rateTimes_.begin();
}



}
