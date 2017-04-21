/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2017 Klaus Spanderen

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

/*! \file gbsmrndcalculator.hpp
    \brief risk neutral terminal density calculator for the
           Black-Scholes-Merton model with skew dependent volatility
*/

#include <ql/math/functional.hpp>
#include <ql/math/solvers1d/brent.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/pricingengines/blackcalculator.hpp>
#include <ql/experimental/finitedifferences/gbsmrndcalculator.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif
#include <boost/bind.hpp>
#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic pop
#endif

#include <boost/function.hpp>
#include <boost/make_shared.hpp>

namespace QuantLib {

    GBSMRNDCalculator::GBSMRNDCalculator(
        const boost::shared_ptr<GeneralizedBlackScholesProcess>& process)
    : process_(process) { }

    Real GBSMRNDCalculator::pdf(Real k, Time t) const {
        const Real dk = 1e-3*k;

        return (cdf(k+dk, t) - cdf(k-dk, t)) / (2*dk);
    }

    Real GBSMRNDCalculator::cdf(Real k, Time t) const {
        const Handle<BlackVolTermStructure> volTS
            = process_->blackVolatility();

        const Real dk = 1e-3*k;
        const Real dvol_dk
            = (volTS->blackVol(t, k+dk) - volTS->blackVol(t, k-dk)) / (2*dk);

        const DiscountFactor dR
            = process_->riskFreeRate()->discount(t, true);
        const DiscountFactor dD
            = process_->dividendYield()->discount(t, true);

        const Real forward = process_->x0() * dD / dR;
        const Real stdDev = std::sqrt(
            process_->blackVolatility()->blackVariance(t, k, true));

        if (forward <= k) {
            const BlackCalculator calc(Option::Call, k, forward, stdDev, dR);

            return 1.0 + (  calc.strikeSensitivity()
                          + calc.vega(t) * dvol_dk) /dR;
        }
        else {
            const BlackCalculator calc(Option::Put, k, forward, stdDev, dR);

            return (  calc.strikeSensitivity()
                    + calc.vega(t) * dvol_dk) /dR;
        }
    }

    Real GBSMRNDCalculator::invcdf(Real q, Time t) const {
        const Real fwd = process_->x0()
            / process_->riskFreeRate()->discount(t, true)
            * process_->dividendYield()->discount(t, true);

        const Volatility atmVariance = std::sqrt(
            process_->blackVolatility()->blackVariance(t, fwd, true));

        const Real atmX = InverseCumulativeNormal()(q);

        const Real guess = fwd*std::exp(atmVariance*atmX);

        Real lower = guess;
        while (guess/lower < 65535.0 && cdf(lower, t) > q)
            lower*=0.5;

        Real upper = guess;
        while (upper/guess < 65535.0 && cdf(upper, t) < q) upper*=2;

        QL_REQUIRE(guess/lower < 65535.0 && upper/guess < 65535.0,
                "Could not find an start interval with ("
                << lower << ", " << upper << ") -> ("
                << cdf(lower, t) << ", " << cdf(upper, t) << ")");

        return Brent().solve(
            compose(std::bind2nd(std::minus<Real>(), q),
                boost::function<Real(Real)>(
                    boost::bind(&GBSMRNDCalculator::cdf, this, _1, t))),
            1e-10, 0.5*(lower+upper), lower, upper);
    }
}
