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

#include <ql/termstructures/volatility/noarbsabr.hpp>
#include <ql/math/integrals/gaussianquadratures.hpp>

#include <boost/make_shared.hpp>
#include <boost/math/special_functions/gamma.hpp>

namespace QuantLib {

    namespace detail {

        class D0Interpolator {

#include "noarbsabrabsprobs.dat"

            static constexpr Real tau[] = {
                0.25,  0.5,  0.75,  1.0,  1.25,  1.5,  1.75,  2.0,  2.25,  2.5,
                2.75,  3.0,  3.25,  3.5,  3.75,  4.0,  4.25,  4.5,  4.75,  5.0,
                5.25,  5.5,  5.75,  6.0,  6.25,  6.5,  6.75,  7.0,  7.25,  7.5,
                7.75,  8.0,  8.25,  8.5,  8.75,  9.0,  9.25,  9.5,  9.75,  10.0,
                10.25, 10.5, 10.75, 11.0, 11.25, 11.5, 11.75, 12.0, 12.25, 12.5,
                12.75, 13.0, 13.25, 13.5, 13.75, 14.0, 14.25, 14.5, 14.75, 15.0,
                15.25, 15.5, 15.75, 16.0, 16.25, 16.5, 16.75, 17.0, 17.25, 17.5,
                17.75, 18.0, 18.25, 18.5, 18.75, 19.0, 19.25, 19.5, 19.75, 20.0,
                20.25, 20.5, 20.75, 21.0, 21.25, 21.5, 21.75, 22.0, 22.25, 22.5,
                22.75, 23.0, 23.25, 23.5, 23.75, 24.0, 24.25, 24.5, 24.75, 25.0,
                25.25, 25.5, 25.75, 26.0, 26.25, 26.5, 26.75, 27.0, 27.25, 27.5,
                27.75, 28.0, 28.25, 28.5, 28.75, 29.0, 29.25, 29.5, 29.75, 30.0
            };

            static constexpr Real sigmaI[] = {
                1,    0.8,  0.7,  0.6,  0.5,  0.45,  0.4, 0.35,  0.3,
                0.27, 0.24, 0.21, 0.18, 0.15, 0.125, 0.1, 0.075, 0.05
            };

            static constexpr Real rho[] = { -0.75, -0.50, -0.25, 0.00,
                                        0.25,  0.50,  0.75 };

            static constexpr Real nu[] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 };

            static constexpr Real beta[] = { 0.01, 0.1, 0.2, 0.3, 0.4,
                                         0.5,  0.6, 0.7, 0.8, 0.9 };

            D0Interpolator(const Real forward, const Real expiryTime,
                           const Real alpha, const Real beta, const Real nu,
                           const Real rho,
                           const Size order = 32)
                : forward_(forward), expiryTime_(expiryTime), alpha_(alpha),
                  beta_(beta), nu_(nu), rho_(rho),
                  gamma_(1.0 / (2.0 * (1.0 - beta_))) {

                gaussLaguerre =
                    boost::make_shared<GaussLaguerreIntegration>(order, gamma_);
            }

            const Real operator()(const Real y) {

                return std::exp(-y) * std::pow(y, gamma_) /
                       boost::math::tgamma(gamma_);
            }

          private:
            boost::shared_ptr<GaussianQuadrature> gaussLaguerre;
            const Real forward_, expiryTime_, alpha_, beta_, nu_, rho_, gamma_;

        }; // class D0Interpolator

    } // namespace detail

} // namespace QuantLib
