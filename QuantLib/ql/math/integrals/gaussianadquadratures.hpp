/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Klaus Spanderen

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

/*! \file gaussianadquadratures.hpp
    \brief Integral of a 1-dimensional function using the Gauss quadratures
*/

#ifndef quantlib_gaussian_ad_quadratures_hpp
#define quantlib_gaussian_ad_quadratures_hpp

#include <ql/math/array.hpp>
#include <ql/math/integrals/gaussianquadratures.hpp>

#include <cppad/cppad.hpp>


namespace QuantLib {
    class GaussianOrthogonalPolynomial;

    class GaussianADQuadrature {
      public:
        GaussianADQuadrature(Size n,
                           const GaussianOrthogonalPolynomial& p) {
        	GaussianQuadrature gq(n, p);
        	x_ = gq.x();
        	w_ = gq.weights();
        }

        template <class F>
        CppAD::AD<Real> operator()(const F& f) const {
        	CppAD::AD<Real> sum = 0.0;
            for (Integer i = order()-1; i >= 0; --i) {
                sum += w_[i] * f(x_[i]);
            }
            return sum;
        }

        Size order() const { return x_.size(); }
        const Array& weights() { return w_; }
        const Array& x()       { return x_; }

      protected:
        Array x_, w_;
    };


    //! generalized Gauss-Laguerre integration
    /*! This class performs a 1-dimensional Gauss-Laguerre integration.
        \f[
        \int_{0}^{\inf} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x;s)=x^s \exp{-x}
        \f]
        and \f[ s > -1 \f]
    */
    class GaussLaguerreADIntegration : public GaussianADQuadrature {
      public:
        GaussLaguerreADIntegration(Size n, Real s = 0.0)
        : GaussianADQuadrature(n, GaussLaguerrePolynomial(s)) {}
    };

    //! generalized Gauss-Hermite integration
    /*! This class performs a 1-dimensional Gauss-Hermite integration.
        \f[
        \int_{-\inf}^{\inf} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x;\mu)=|x|^{2\mu} \exp{-x*x}
        \f]
        and \f[ \mu > -0.5 \f]
    */
    class GaussHermiteADIntegration : public GaussianADQuadrature {
      public:
        GaussHermiteADIntegration(Size n, Real mu = 0.0)
        : GaussianADQuadrature(n, GaussHermitePolynomial(mu)) {}
    };
    //! Gauss-Jacobi integration
    /*! This class performs a 1-dimensional Gauss-Jacobi integration.
        \f[
        \int_{-1}^{1} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x;\alpha,\beta)=(1-x)^\alpha (1+x)^\beta
        \f]
    */
    class GaussJacobiADIntegration : public GaussianADQuadrature {
      public:
        GaussJacobiADIntegration(Size n, Real alpha, Real beta)
        : GaussianADQuadrature(n, GaussJacobiPolynomial(alpha, beta)) {}
    };

    //! Gauss-Hyperbolic integration
    /*! This class performs a 1-dimensional Gauss-Hyperbolic integration.
        \f[
        \int_{-\inf}^{\inf} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x)=1/cosh(x)
        \f]
    */
    class GaussHyperbolicADIntegration : public GaussianADQuadrature {
      public:
        GaussHyperbolicADIntegration(Size n)
        : GaussianADQuadrature(n, GaussHyperbolicPolynomial()) {}
    };

    //! Gauss-Legendre integration
    /*! This class performs a 1-dimensional Gauss-Legendre integration.
        \f[
        \int_{-1}^{1} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x)=1
        \f]
    */
    class GaussLegendreADIntegration : public GaussianADQuadrature {
      public:
        GaussLegendreADIntegration(Size n)
        : GaussianADQuadrature(n, GaussJacobiPolynomial(0.0, 0.0)) {}
    };

    //! Gauss-Chebyshev integration
    /*! This class performs a 1-dimensional Gauss-Chebyshev integration.
        \f[
        \int_{-1}^{1} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x)=(1-x^2)^{-1/2}
        \f]
    */
    class GaussChebyshevADIntegration : public GaussianADQuadrature {
      public:
        GaussChebyshevADIntegration(Size n)
        : GaussianADQuadrature(n, GaussJacobiPolynomial(-0.5, -0.5)) {}
    };

    //! Gauss-Chebyshev integration (second kind)
    /*! This class performs a 1-dimensional Gauss-Chebyshev integration.
        \f[
        \int_{-1}^{1} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x)=(1-x^2)^{1/2}
        \f]
    */
    class GaussChebyshev2ndADIntegration : public GaussianADQuadrature {
      public:
        GaussChebyshev2ndADIntegration(Size n)
      : GaussianADQuadrature(n, GaussJacobiPolynomial(0.5, 0.5)) {}
    };

    //! Gauss-Gegenbauer integration
    /*! This class performs a 1-dimensional Gauss-Gegenbauer integration.
        \f[
        \int_{-1}^{1} f(x) \mathrm{d}x
        \f]
        The weighting function is
        \f[
            w(x)=(1-x^2)^{\lambda-1/2}
        \f]
    */
    class GaussGegenbauerADIntegration : public GaussianADQuadrature {
      public:
        GaussGegenbauerADIntegration(Size n, Real lambda)
        : GaussianADQuadrature(n, GaussJacobiPolynomial(lambda-0.5, lambda-0.5))
        {}
    };
}

#endif
