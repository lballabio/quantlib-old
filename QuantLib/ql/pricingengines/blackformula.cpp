/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2012 Ferdinando Ametrano
 Copyright (C) 2006 Mark Joshi
 Copyright (C) 2006 StatPro Italia srl
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2007 Chiara Fornarola

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

#include <ql/pricingengines/blackformula.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif
#include <boost/math/special_functions/atanh.hpp>
#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic pop
#endif

// namespace {
//     void checkParameters(QuantLib::Real strike,
//                          QuantLib::Real forward,
//                          QuantLib::Real displacement)
//     {
//         QL_REQUIRE(displacement >= 0.0, "displacement ("
//                                             << displacement
//                                             << ") must be non-negative");
//         QL_REQUIRE(strike + displacement >= 0.0,
//                    "strike + displacement (" << strike << " + " << displacement
//                                              << ") must be non-negative");
//         QL_REQUIRE(forward + displacement > 0.0, "forward + displacement ("
//                                                      << forward << " + "
//                                                      << displacement
//                                                      << ") must be positive");
//     }
// }

namespace QuantLib {

    Real blackFormulaCashItmProbability(Option::Type optionType,
                                        Real strike,
                                        Real forward,
                                        Real stdDev,
                                        Real displacement) {
        checkParameters(strike, forward, displacement);
        if (stdDev==0.0)
            return (forward*optionType > strike*optionType ? 1.0 : 0.0);

        forward = forward + displacement;
        strike = strike + displacement;
        if (strike==0.0)
            return (optionType==Option::Call ? 1.0 : 0.0);
        Real d2 = std::log(forward/strike)/stdDev - 0.5*stdDev;
        CumulativeNormalDistribution phi;
        return phi(optionType*d2);
    }


    Real blackFormulaCashItmProbability(
                        const boost::shared_ptr<PlainVanillaPayoff>& payoff,
                        Real forward,
                        Real stdDev,
                        Real displacement) {
        return blackFormulaCashItmProbability(payoff->optionType(),
            payoff->strike(), forward, stdDev , displacement);
    }


    Real blackFormulaVolDerivative(Rate strike,
                                      Rate forward,
                                      Real stdDev,
                                      Real expiry,
                                      Real discount,
                                      Real displacement)
    {
        return  blackFormulaStdDevDerivative(strike,
                                     forward,
                                     stdDev,
                                     discount,
                                     displacement)*std::sqrt(expiry);
    }

    Real blackFormulaStdDevDerivative(Rate strike,
                                      Rate forward,
                                      Real stdDev,
                                      Real discount,
                                      Real displacement)
    {
        checkParameters(strike, forward, displacement);
        QL_REQUIRE(stdDev>=0.0,
                   "stdDev (" << stdDev << ") must be non-negative");
        QL_REQUIRE(discount>0.0,
                   "discount (" << discount << ") must be positive");

        forward = forward + displacement;
        strike = strike + displacement;

        if (stdDev==0.0 || strike==0.0)
            return 0.0;

        Real d1 = std::log(forward/strike)/stdDev + .5*stdDev;
        return discount * forward *
            CumulativeNormalDistribution().derivative(d1);
    }

    Real blackFormulaStdDevDerivative(
                        const boost::shared_ptr<PlainVanillaPayoff>& payoff,
                        Real forward,
                        Real stdDev,
                        Real discount,
                        Real displacement) {
        return blackFormulaStdDevDerivative(payoff->strike(), forward,
                                     stdDev, discount, displacement);
    }


    Real bachelierBlackFormula(Option::Type optionType,
                               Real strike,
                               Real forward,
                               Real stdDev,
                               Real discount)
    {
        QL_REQUIRE(stdDev>=0.0,
                   "stdDev (" << stdDev << ") must be non-negative");
        QL_REQUIRE(discount>0.0,
                   "discount (" << discount << ") must be positive");
        Real d = (forward-strike)*optionType, h = d/stdDev;
        if (stdDev==0.0)
            return discount*std::max(d, 0.0);
        CumulativeNormalDistribution phi;
        Real result = discount*(stdDev*phi.derivative(h) + d*phi(h));
        QL_ENSURE(result>=0.0,
                  "negative value (" << result << ") for " <<
                  stdDev << " stdDev, " <<
                  optionType << " option, " <<
                  strike << " strike , " <<
                  forward << " forward");
        return result;
    }

    Real bachelierBlackFormula(
                        const boost::shared_ptr<PlainVanillaPayoff>& payoff,
                        Real forward,
                        Real stdDev,
                        Real discount) {
        return bachelierBlackFormula(payoff->optionType(),
            payoff->strike(), forward, stdDev, discount);
    }

    static Real h(Real eta) {

        const static Real  A0          = 3.994961687345134e-1;
        const static Real  A1          = 2.100960795068497e+1;
        const static Real  A2          = 4.980340217855084e+1;
        const static Real  A3          = 5.988761102690991e+2;
        const static Real  A4          = 1.848489695437094e+3;
        const static Real  A5          = 6.106322407867059e+3;
        const static Real  A6          = 2.493415285349361e+4;
        const static Real  A7          = 1.266458051348246e+4;

        const static Real  B0          = 1.000000000000000e+0;
        const static Real  B1          = 4.990534153589422e+1;
        const static Real  B2          = 3.093573936743112e+1;
        const static Real  B3          = 1.495105008310999e+3;
        const static Real  B4          = 1.323614537899738e+3;
        const static Real  B5          = 1.598919697679745e+4;
        const static Real  B6          = 2.392008891720782e+4;
        const static Real  B7          = 3.608817108375034e+3;
        const static Real  B8          = -2.067719486400926e+2;
        const static Real  B9          = 1.174240599306013e+1;

        QL_REQUIRE(eta>=0.0,
                       "eta (" << eta << ") must be non-negative");

        const Real num = A0 + eta * (A1 + eta * (A2 + eta * (A3 + eta * (A4 + eta
                    * (A5 + eta * (A6 + eta * A7))))));

        const Real den = B0 + eta * (B1 + eta * (B2 + eta * (B3 + eta * (B4 + eta
                    * (B5 + eta * (B6 + eta * (B7 + eta * (B8 + eta * B9))))))));

        return std::sqrt(eta) * (num / den);

    }

    Real bachelierBlackFormulaImpliedVol(Option::Type optionType,
                                   Real strike,
                                   Real forward,
                                   Real tte,
                                   Real bachelierPrice,
                                   Real discount) {

        const static Real SQRT_QL_EPSILON = std::sqrt(QL_EPSILON);

        QL_REQUIRE(tte>0.0,
                   "tte (" << tte << ") must be positive");

        Real forwardPremium = bachelierPrice/discount;

        Real straddlePremium;
        if (optionType==Option::Call){
            straddlePremium = 2.0 * forwardPremium - (forward - strike);
        } else {
            straddlePremium = 2.0 * forwardPremium + (forward - strike);
        }

        Real nu = (forward - strike) / straddlePremium;
        QL_REQUIRE(nu<=1.0,
                   "nu (" << nu << ") must be <= 1.0");
        QL_REQUIRE(nu>=-1.0,
                     "nu (" << nu << ") must be >= -1.0");

        nu = std::max(-1.0 + QL_EPSILON, std::min(nu,1.0 - QL_EPSILON));

        // nu / arctanh(nu) -> 1 as nu -> 0
        Real eta = (std::fabs(nu) < SQRT_QL_EPSILON) ? 1.0 : nu / boost::math::atanh(nu);

        Real heta = h(eta);

        Real impliedBpvol = std::sqrt(M_PI / (2 * tte)) * straddlePremium * heta;

        return impliedBpvol;
    }
}
