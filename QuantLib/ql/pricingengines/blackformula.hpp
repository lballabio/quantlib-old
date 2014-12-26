/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2003, 2004, 2005, 2006, 2008 Ferdinando Ametrano
 Copyright (C) 2006 Mark Joshi
 Copyright (C) 2006 StatPro Italia srl
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2013 Gary Kennedy
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

/*! \file blackformula.hpp
    \brief Black formula
*/

#ifndef quantlib_blackformula_hpp
#define quantlib_blackformula_hpp

#include <ql/option.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/math/errorfunction.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#if defined(__GNUC__) &&                                                       \
    (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif
#include <boost/math/special_functions/atanh.hpp>
#if defined(__GNUC__) &&                                                       \
    (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic pop
#endif

namespace {
template <class T> void checkParameters(T strike, T forward, T displacement) {
    QL_REQUIRE(displacement >= 0.0,
               "displacement (" << displacement << ") must be non-negative");
    QL_REQUIRE(strike + displacement >= 0.0,
               "strike + displacement (" << strike << " + " << displacement
                                         << ") must be non-negative");
    QL_REQUIRE(forward + displacement > 0.0,
               "forward + displacement (" << forward << " + " << displacement
                                          << ") must be positive");
}
}

namespace QuantLib {

using std::log;
using std::max;
using std::sqrt;

/*! Black 1976 formula
    \warning instead of volatility it uses standard deviation,
             i.e. volatility*sqrt(timeToMaturity)
*/
template <class T = Real>
T blackFormula(Option::Type optionType, T strike, T forward, T stdDev,
               T discount = 1.0, T displacement = 0.0) {
    checkParameters(strike, forward, displacement);
    QL_REQUIRE(stdDev >= 0.0, "stdDev (" << stdDev << ") must be non-negative");
    QL_REQUIRE(discount > 0.0, "discount (" << discount
                                            << ") must be positive");

    if (stdDev == 0.0)
        return max<T>((forward - strike) * optionType, 0.0) * discount;
    forward = forward + displacement;
    strike = strike + displacement;

    // since displacement is non-negative strike==0 iff displacement==0
    // so returning forward*discount is OK
    if (strike == 0.0)
        return (optionType == Option::Call ? forward * discount : 0.0);

    T d1 = log(forward / strike) / stdDev + 0.5 * stdDev;
    T d2 = d1 - stdDev;
    CumulativeNormalDistribution_t<T> phi;
    T nd1 = phi(optionType * d1);
    T nd2 = phi(optionType * d2);
    T result = discount * optionType * (forward * nd1 - strike * nd2);
    QL_ENSURE(result >= 0.0, "negative value ("
                                 << result << ") for " << stdDev << " stdDev, "
                                 << optionType << " option, " << strike
                                 << " strike , " << forward << " forward");
    return result;
}

/*! Black 1976 formula
    \warning instead of volatility it uses standard deviation,
             i.e. volatility*sqrt(timeToMaturity)
*/
template <class T = Real>
T blackFormula(const boost::shared_ptr<PlainVanillaPayoff> &payoff, T forward,
               T stdDev, T discount = 1.0, T displacement = 0.0) {
    return blackFormula<T>(payoff->optionType(), payoff->strike(), forward,
                           stdDev, discount, displacement);
}

/*! Approximated Black 1976 implied standard deviation,
    i.e. volatility*sqrt(timeToMaturity).

    It is calculated using Brenner and Subrahmanyan (1988) and Feinstein
    (1988) approximation for at-the-money forward option, with the
    extended moneyness approximation by Corrado and Miller (1996)
    tape safe implementation
*/

template <class T = Real>
T blackFormulaImpliedStdDevApproximation(Option::Type optionType, T strike,
                                         T forward, T blackPrice,
                                         T discount = 1.0,
                                         T displacement = 0.0) {
    checkParameters(strike, forward, displacement);
    QL_REQUIRE(blackPrice >= 0.0, "blackPrice (" << blackPrice
                                                 << ") must be non-negative");
    QL_REQUIRE(discount > 0.0, "discount (" << discount
                                            << ") must be positive");

    forward = forward + displacement;
    strike = strike + displacement;

    // Brenner-Subrahmanyan (1988) and Feinstein (1988) ATM approx.
    T result0 = blackPrice / discount * sqrt(2.0 * M_PI) / forward;

    // Corrado and Miller extended moneyness approximation
    T moneynessDelta = optionType * (forward - strike);
    T moneynessDelta_2 = moneynessDelta / 2.0;
    T temp = blackPrice / discount - moneynessDelta_2;
    T moneynessDelta_PI = moneynessDelta * moneynessDelta / M_PI;
    T temp2 = temp * temp - moneynessDelta_PI;
    // approximation breaks down, 2 alternatives:
    // 1. zero it
    temp2 = CondExpLt(temp2, 0.0, 0.0, temp2);
    // 2. Manaster-Koehler (1982) efficient Newton-Raphson seed
    // return std::fabs(std::log(forward/strike))*std::sqrt(2.0);
    temp2 = sqrt(temp2);
    temp += temp2;
    temp *= sqrt(2.0 * M_PI);
    T result1 = temp / (forward + strike);

    T stdDev = CondExpEq(strike, forward, result0, result1);
    QL_ENSURE(stdDev >= 0.0, "stdDev (" << stdDev << ") must be non-negative");
    return stdDev;
}

/*! Approximated Black 1976 implied standard deviation,
    i.e. volatility*sqrt(timeToMaturity).

    It is calculated using Brenner and Subrahmanyan (1988) and Feinstein
    (1988) approximation for at-the-money forward option, with the
    extended moneyness approximation by Corrado and Miller (1996)
    tape safe implementation
*/
template <class T = Real>
T blackFormulaImpliedStdDevApproximation(
    const boost::shared_ptr<PlainVanillaPayoff> &payoff, T forward,
    T blackPrice, T discount = 1.0, T displacement = 0.0) {
    return blackFormulaImpliedStdDevApproximation<T>(
        payoff->optionType(), payoff->strike(), forward, blackPrice, discount,
        displacement);
}

// helper class
template <class T> class BlackImpliedStdDevHelper_t {
  public:
    BlackImpliedStdDevHelper_t(Option::Type optionType, T strike, T forward,
                               T undiscountedBlackPrice, T displacement = 0.0)
        : halfOptionType_(0.5 * optionType),
          signedStrike_(optionType * (strike + displacement)),
          signedForward_(optionType * (forward + displacement)),
          undiscountedBlackPrice_(undiscountedBlackPrice) {
        checkParameters(strike, forward, displacement);
        QL_REQUIRE(undiscountedBlackPrice >= 0.0,
                   "undiscounted Black price (" << undiscountedBlackPrice
                                                << ") must be non-negative");
        signedMoneyness_ = optionType * log((forward + displacement) /
                                            (strike + displacement));
    }
    T operator()(T stdDev) const {
#if defined(QL_EXTRA_SAFETY_CHECKS)
        QL_REQUIRE(stdDev >= 0.0, "stdDev (" << stdDev
                                             << ") must be non-negative");
#endif
        if (stdDev == 0.0)
            return max<T>(signedForward_ - signedStrike_, Real(0.0)) -
                   undiscountedBlackPrice_;
        T temp = halfOptionType_ * stdDev;
        T d = signedMoneyness_ / stdDev;
        T signedD1 = d + temp;
        T signedD2 = d - temp;
        T result = signedForward_ * N_(signedD1) - signedStrike_ * N_(signedD2);
        // numerical inaccuracies can yield a negative answer
        return max<T>(T(0.0), result) - undiscountedBlackPrice_;
    }
    T derivative(T stdDev) const {
#if defined(QL_EXTRA_SAFETY_CHECKS)
        QL_REQUIRE(stdDev >= 0.0, "stdDev (" << stdDev
                                             << ") must be non-negative");
#endif
        T signedD1 = signedMoneyness_ / stdDev + halfOptionType_ * stdDev;
        return signedForward_ * N_.derivative(signedD1);
    }

  private:
    T halfOptionType_;
    T signedStrike_, signedForward_;
    T undiscountedBlackPrice_, signedMoneyness_;
    CumulativeNormalDistribution_t<T> N_;
};

typedef BlackImpliedStdDevHelper_t<Real> BlackImpliedStdDevHelper;

/*! Black 1976 implied standard deviation,
    i.e. volatility*sqrt(timeToMaturity)
*/
template <class T = Real>
T blackFormulaImpliedStdDev(Option::Type optionType, T strike, T forward,
                            T blackPrice, T discount = 1.0,
                            T displacement = 0.0, T guess = Null<Real>(),
                            T accuracy = 1.0e-6, Natural maxIterations = 100) {
    checkParameters(strike, forward, displacement);

    QL_REQUIRE(discount > 0.0, "discount (" << discount
                                            << ") must be positive");

    QL_REQUIRE(blackPrice >= 0.0, "option price (" << blackPrice
                                                   << ") must be non-negative");
    // check the price of the "other" option implied by put-call paity
    T otherOptionPrice =
        blackPrice - optionType * (forward - strike) * discount;
    QL_REQUIRE(
        otherOptionPrice >= 0.0,
        "negative " << Option::Type(-1 * optionType) << " price ("
                    << otherOptionPrice
                    << ") implied by put-call parity. No solution exists for "
                    << optionType << " strike " << strike << ", forward "
                    << forward << ", price " << blackPrice << ", deflator "
                    << discount);

    // solve for the out-of-the-money option which has
    // greater vega/price ratio, i.e.
    // it is numerically more robust for implied vol calculations
    if (optionType == Option::Put && strike > forward) {
        optionType = Option::Call;
        blackPrice = otherOptionPrice;
    }
    if (optionType == Option::Call && strike < forward) {
        optionType = Option::Put;
        blackPrice = otherOptionPrice;
    }

    strike = strike + displacement;
    forward = forward + displacement;

    if (guess == Null<Real>())
        guess = blackFormulaImpliedStdDevApproximation(
            optionType, strike, forward, blackPrice, discount, displacement);
    else
        QL_REQUIRE(guess >= 0.0, "stdDev guess (" << guess
                                                  << ") must be non-negative");
    BlackImpliedStdDevHelper f(optionType, strike, forward,
                               blackPrice / discount);
    NewtonSafe solver;
    solver.setMaxEvaluations(maxIterations);
    T minSdtDev = 0.0, maxStdDev = 24.0; // 24 = 300% * sqrt(60)
    T stdDev = solver.solve(f, accuracy, guess, minSdtDev, maxStdDev);
    QL_ENSURE(stdDev >= 0.0, "stdDev (" << stdDev << ") must be non-negative");
    return stdDev;
}

/*! Black 1976 implied standard deviation,
    i.e. volatility*sqrt(timeToMaturity)
*/
template <class T = Real>
T blackFormulaImpliedStdDev(const boost::shared_ptr<PlainVanillaPayoff> &payoff,
                            T forward, T blackPrice, T discount = 1.0,
                            T displacement = 0.0, T guess = Null<Real>(),
                            T accuracy = 1.0e-6, Natural maxIterations = 100) {
    return blackFormulaImpliedStdDev(
        payoff->optionType(), payoff->strike(), forward, blackPrice, discount,
        displacement, guess, accuracy, maxIterations);
}

/*! Black 1976 probability of being in the money (in the bond martingale
    measure), i.e. N(d2).
    It is a risk-neutral probability, not the real world one.
    \warning instead of volatility it uses standard deviation,
             i.e. volatility*sqrt(timeToMaturity)
*/
Real blackFormulaCashItmProbability(Option::Type optionType, Real strike,
                                    Real forward, Real stdDev,
                                    Real displacement = 0.0);

/*! Black 1976 probability of being in the money (in the bond martingale
    measure), i.e. N(d2).
    It is a risk-neutral probability, not the real world one.
    \warning instead of volatility it uses standard deviation,
             i.e. volatility*sqrt(timeToMaturity)
*/
Real blackFormulaCashItmProbability(
    const boost::shared_ptr<PlainVanillaPayoff> &payoff, Real forward,
    Real stdDev, Real displacement = 0.0);

/*! Black 1976 formula for standard deviation derivative
    \warning instead of volatility it uses standard deviation, i.e.
             volatility*sqrt(timeToMaturity), and it returns the
             derivative with respect to the standard deviation.
             If T is the time to maturity Black vega would be
             blackStdDevDerivative(strike, forward, stdDev)*sqrt(T)
*/
Real blackFormulaStdDevDerivative(Real strike, Real forward, Real stdDev,
                                  Real discount = 1.0, Real displacement = 0.0);

/*! Black 1976 formula for  derivative with respect to implied vol, this
    is basically the vega, but if you want 1% change multiply by 1%
*/
Real blackFormulaVolDerivative(Real strike, Real forward, Real stdDev,
                               Real expiry, Real discount = 1.0,
                               Real displacement = 0.0);

/*! Black 1976 formula for standard deviation derivative
    \warning instead of volatility it uses standard deviation, i.e.
             volatility*sqrt(timeToMaturity), and it returns the
             derivative with respect to the standard deviation.
             If T is the time to maturity Black vega would be
             blackStdDevDerivative(strike, forward, stdDev)*sqrt(T)
*/
Real blackFormulaStdDevDerivative(
    const boost::shared_ptr<PlainVanillaPayoff> &payoff, Real forward,
    Real stdDev, Real discount = 1.0, Real displacement = 0.0);

/*! Black style formula when forward is normal rather than
    log-normal. This is essentially the model of Bachelier.

    \warning Bachelier model needs absolute volatility, not
             percentage volatility. Standard deviation is
             absoluteVolatility*sqrt(timeToMaturity)
*/
Real bachelierBlackFormula(Option::Type optionType, Real strike, Real forward,
                           Real stdDev, Real discount = 1.0);

/*! Black style formula when forward is normal rather than
    log-normal. This is essentially the model of Bachelier.

    \warning Bachelier model needs absolute volatility, not
             percentage volatility. Standard deviation is
             absoluteVolatility*sqrt(timeToMaturity)
*/
Real bachelierBlackFormula(const boost::shared_ptr<PlainVanillaPayoff> &payoff,
                           Real forward, Real stdDev, Real discount = 1.0);
/*! Approximated Bachelier implied volatility

    It is calculated using  the analytic implied volatility approximation
    of J. Choi, K Kim and M. Kwak (2009), “Numerical Approximation of the
    Implied Volatility Under Arithmetic Brownian Motion”,
    Applied Math. Finance, 16(3), pp. 261-268.
*/
Real bachelierBlackFormulaImpliedVol(Option::Type optionType, Real strike,
                                     Real forward, Real tte,
                                     Real bachelierPrice, Real discount = 1.0);
}

#endif
