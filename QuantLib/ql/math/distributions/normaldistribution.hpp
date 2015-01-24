/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2010 Kakhkhor Abdijalilov
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

/*! \file normaldistribution.hpp
    \brief normal, cumulative and inverse cumulative distributions
*/

#ifndef quantlib_normal_distribution_hpp
#define quantlib_normal_distribution_hpp

#include <ql/math/errorfunction.hpp>
#include <ql/errors.hpp>

namespace QuantLib {

using std::exp;
using std::abs;

//! Normal distribution function
/*! Given x, it returns its probability in a Gaussian normal distribution.
    It provides the first derivative too.

    \test the correctness of the returned value is tested by
          checking it against numerical calculations. Cross-checks
          are also performed against the
          CumulativeNormalDistribution and InverseCumulativeNormal
          classes.
*/
template <class T = Real>
class NormalDistribution_t : public std::unary_function<T, T> {
  public:
    NormalDistribution_t(T average = 0.0, T sigma = 1.0);
    // function
    T operator()(T x) const;
    T derivative(T x) const;

  private:
    T average_, sigma_, normalizationFactor_, denominator_,
        derNormalizationFactor_;
};

typedef NormalDistribution_t<Real> NormalDistribution;
typedef NormalDistribution GaussianDistribution;

//! Cumulative normal distribution function
/*! Given x it provides an approximation to the
    integral of the gaussian normal distribution:
    formula here ...

    For this implementation see M. Abramowitz and I. Stegun,
    Handbook of Mathematical Functions,
    Dover Publications, New York (1972)
*/
template <class T = Real>
class CumulativeNormalDistribution_t : public std::unary_function<T, T> {
  public:
    CumulativeNormalDistribution_t(T average = 0.0, T sigma = 1.0);
    // function
    T operator()(T x) const;
    T derivative(T x) const;

  private:
    T average_, sigma_;
    NormalDistribution_t<T> gaussian_;
    ErrorFunction_t<T> errorFunction_;
};

typedef CumulativeNormalDistribution_t<Real> CumulativeNormalDistribution;

template <class T> T CumulativeNormalDistribution_t<T>::operator()(T z) const {
    // QL_REQUIRE(!(z >= average_ && 2.0*average_-z > average_),
    //           "not a real number. ");
    z = (z - average_) / sigma_;

    T result = 0.5 * (1.0 + errorFunction_(z * M_SQRT_2));
    if (result <= 1e-8) { // todo: investigate the threshold level
        // Asymptotic expansion for very negative z following (26.2.12)
        // on page 408 in M. Abramowitz and A. Stegun,
        // Pocketbook of Mathematical Functions, ISBN 3-87144818-4.
        T sum = 1.0, zsqr = z * z, i = 1.0, g = 1.0, x, y, a = QL_MAX_REAL,
          lasta;
        do {
            lasta = a;
            x = (4.0 * i - 3.0) / zsqr;
            y = x * ((4.0 * i - 1) / zsqr);
            a = g * (x - y);
            sum -= a;
            g *= y;
            i+=1.0;
            a = abs(a);
        } while (lasta > a && a >= abs(sum * QL_EPSILON));
        result = -gaussian_(z) / z * sum;
    }
    return result;
}

//! Inverse cumulative normal distribution function
/*! Given x between zero and one as
  the integral value of a gaussian normal distribution
  this class provides the value y such that
  formula here ...

  It use Acklam's approximation:
  by Peter J. Acklam, University of Oslo, Statistics Division.
  URL: http://home.online.no/~pjacklam/notes/invnorm/index.html

  This class can also be used to generate a gaussian normal
  distribution from a uniform distribution.
  This is especially useful when a gaussian normal distribution
  is generated from a low discrepancy uniform distribution:
  in this case the traditional Box-Muller approach and its
  variants would not preserve the sequence's low-discrepancy.

*/
class InverseCumulativeNormal : public std::unary_function<Real, Real> {
  public:
    InverseCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    // function
    Real operator()(Real x) const {
        return average_ + sigma_ * standard_value(x);
    }
    // value for average=0, sigma=1
    /* Compared to operator(), this method avoids 2 floating point
       operations (we use average=0 and sigma=1 most of the
       time). The speed difference is noticeable.
    */
    static Real standard_value(Real x) {
        Real z;
        if (x < x_low_ || x_high_ < x) {
            z = tail_value(x);
        } else {
            z = x - 0.5;
            Real r = z * z;
            z = (((((a1_ * r + a2_) * r + a3_) * r + a4_) * r + a5_) * r +
                 a6_) *
                z /
                (((((b1_ * r + b2_) * r + b3_) * r + b4_) * r + b5_) * r + 1.0);
        }

// The relative error of the approximation has absolute value less
// than 1.15e-9.  One iteration of Halley's rational method (third
// order) gives full machine precision.
// #define REFINE_TO_FULL_MACHINE_PRECISION_USING_HALLEYS_METHOD
#ifdef REFINE_TO_FULL_MACHINE_PRECISION_USING_HALLEYS_METHOD
        // error (f_(z) - x) divided by the cumulative's derivative
        const Real r = (f_(z) - x) * M_SQRT2 * M_SQRTPI * exp(0.5 * z * z);
        //  Halley's method
        z -= r / (1 + 0.5 * z * r);
#endif

        return z;
    }

  private:
    /* Handling tails moved into a separate method, which should
       make the inlining of operator() and standard_value method
       easier. tail_value is called rarely and doesn't need to be
       inlined.
    */
    static Real tail_value(Real x);
#if defined(QL_PATCH_SOLARIS)
    CumulativeNormalDistribution f_;
#else
    static const CumulativeNormalDistribution f_;
#endif
    Real average_, sigma_;
    static const Real a1_;
    static const Real a2_;
    static const Real a3_;
    static const Real a4_;
    static const Real a5_;
    static const Real a6_;
    static const Real b1_;
    static const Real b2_;
    static const Real b3_;
    static const Real b4_;
    static const Real b5_;
    static const Real c1_;
    static const Real c2_;
    static const Real c3_;
    static const Real c4_;
    static const Real c5_;
    static const Real c6_;
    static const Real d1_;
    static const Real d2_;
    static const Real d3_;
    static const Real d4_;
    static const Real x_low_;
    static const Real x_high_;
};

// backward compatibility
typedef InverseCumulativeNormal InvCumulativeNormalDistribution;

//! Moro Inverse cumulative normal distribution class
/*! Given x between zero and one as
    the integral value of a gaussian normal distribution
    this class provides the value y such that
    formula here ...

    It uses Beasly and Springer approximation, with an improved
    approximation for the tails. See Boris Moro,
    "The Full Monte", 1995, Risk Magazine.

    This class can also be used to generate a gaussian normal
    distribution from a uniform distribution.
    This is especially useful when a gaussian normal distribution
    is generated from a low discrepancy uniform distribution:
    in this case the traditional Box-Muller approach and its
    variants would not preserve the sequence's low-discrepancy.

    Peter J. Acklam's approximation is better and is available
    as QuantLib::InverseCumulativeNormal
*/
class MoroInverseCumulativeNormal : public std::unary_function<Real, Real> {
  public:
    MoroInverseCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    // function
    Real operator()(Real x) const;

  private:
    Real average_, sigma_;
    static const Real a0_;
    static const Real a1_;
    static const Real a2_;
    static const Real a3_;
    static const Real b0_;
    static const Real b1_;
    static const Real b2_;
    static const Real b3_;
    static const Real c0_;
    static const Real c1_;
    static const Real c2_;
    static const Real c3_;
    static const Real c4_;
    static const Real c5_;
    static const Real c6_;
    static const Real c7_;
    static const Real c8_;
};

//! Maddock's Inverse cumulative normal distribution class
/*! Given x between zero and one as
    the integral value of a gaussian normal distribution
    this class provides the value y such that
    formula here ...

    From the boost documentation:
     These functions use a rational approximation devised by
     John Maddock to calculate an initial approximation to the
     result that is accurate to ~10^-19, then only if that has
     insufficient accuracy compared to the epsilon for type double,
     do we clean up the result using Halley iteration.
*/
class MaddockInverseCumulativeNormal : public std::unary_function<Real, Real> {
  public:
    MaddockInverseCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x) const;

  private:
    const Real average_, sigma_;
};

//! Maddock's cumulative normal distribution class
class MaddockCumulativeNormal : public std::unary_function<Real, Real> {
  public:
    MaddockCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x) const;

  private:
    const Real average_, sigma_;
};

// inline definitions

template <class T>
inline NormalDistribution_t<T>::NormalDistribution_t(T average, T sigma)
    : average_(average), sigma_(sigma) {

    QL_REQUIRE(sigma_ > 0.0, "sigma must be greater than 0.0 ("
                                 << sigma_ << " not allowed)");

    normalizationFactor_ = M_SQRT_2 * M_1_SQRTPI / sigma_;
    derNormalizationFactor_ = sigma_ * sigma_;
    denominator_ = 2.0 * derNormalizationFactor_;
}

template <class T> inline T NormalDistribution_t<T>::operator()(T x) const {
    T deltax = x - average_;
    T exponent = -(deltax * deltax) / denominator_;
    // debian alpha had some strange problem in the very-low range
    return exponent <= -690.0 ? 0.0 : // exp(x) < 1.0e-300 anyway
               normalizationFactor_ * exp(exponent);
}

template <class T> inline T NormalDistribution_t<T>::derivative(T x) const {
    return ((*this)(x) * (average_ - x)) / derNormalizationFactor_;
}

template <class T>
inline CumulativeNormalDistribution_t<T>::CumulativeNormalDistribution_t(
    T average, T sigma)
    : average_(average), sigma_(sigma) {

    QL_REQUIRE(sigma_ > 0.0, "sigma must be greater than 0.0 ("
                                 << sigma_ << " not allowed)");
}

template <class T>
inline T CumulativeNormalDistribution_t<T>::derivative(T x) const {
    T xn = (x - average_) / sigma_;
    return gaussian_(xn) / sigma_;
}

inline InverseCumulativeNormal::InverseCumulativeNormal(Real average,
                                                        Real sigma)
    : average_(average), sigma_(sigma) {

    QL_REQUIRE(sigma_ > 0.0, "sigma must be greater than 0.0 ("
                                 << sigma_ << " not allowed)");
}

inline MoroInverseCumulativeNormal::MoroInverseCumulativeNormal(Real average,
                                                                Real sigma)
    : average_(average), sigma_(sigma) {

    QL_REQUIRE(sigma_ > 0.0, "sigma must be greater than 0.0 ("
                                 << sigma_ << " not allowed)");
}
}

#endif
