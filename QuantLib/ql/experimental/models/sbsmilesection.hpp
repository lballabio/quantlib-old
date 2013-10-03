/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file sbsmilesection.hpp
    \brief smilesection using $S_B$ as it's density
*/

#ifndef quantlib_sb_smile_section_hpp
#define quantlib_sb_smile_section_hpp

#include <ql/qldefines.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/math/interpolation.hpp>
#include <ql/math/interpolations/cubicinterpolation.hpp>

namespace QuantLib {

    class SbSmileSection : public SmileSection {

      public:
        SbSmileSection(const Real timeToExpiry,
                       const Real forward,
                       const Real lambda,
                       const Real delta,
                       const Real gamma,
                       const Size resolution = 100,
                       const Real lower = -50.0,
                       const Real upper = 50.0);

        Real minStrike() const { return -QL_MAX_REAL; }
        Real maxStrike() const { return QL_MAX_REAL; }
        Real atmLevel() const { return f_; }

        Real optionPrice(Rate strike, Option::Type type = Option::Call,
                         Real discount = 1.0) const;
        Real digitalOptionPrice(Rate strike, Option::Type type = Option::Call,
                                Real discount = 1.0, Real gap = 1.0e-5) const;
        Real density(Rate strike, Real discount = 1.0, Real gap = 1.0E-4) const;

      protected:
        Volatility volatilityImpl(Rate strike) const;

      private:
        void update();
        Size index(Real strike) const;
        Real f_, xi_, lambda_, delta_, gamma_;
        Real minStrike_ , maxStrike_;
        std::vector<Real> k_, d_;
        boost::shared_ptr<CubicInterpolation> density_;
        std::vector<Real> eta_, kappa_;
        Real etaSum_,adjustment_,expectation_;

    };

}

#endif
