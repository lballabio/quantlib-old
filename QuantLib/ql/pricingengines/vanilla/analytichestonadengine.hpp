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

/*! \file analytichestonadengine.hpp
    \brief analytic Heston-model engine
*/

#ifndef quantlib_analytic_heston_ad_engine_hpp
#define quantlib_analytic_heston_ad_engine_hpp

#include <ql/math/integrals/integral.hpp>
#include <ql/math/integrals/gaussianadquadratures.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/models/equity/hestonmodel.hpp>
#include <ql/instruments/vanillaoption.hpp>

#include <boost/function.hpp>
#include <cppad/cppad.hpp>

#include <complex>

namespace QuantLib {

    //! analytic Heston-model engine based on Fourier transform
    class AnalyticHestonADEngine
        : public GenericModelEngine<HestonModel,
                                    VanillaOption::arguments,
                                    VanillaOption::results> {
      public:
        class Integration;
        enum ComplexLogFormula { Gatheral, BranchCorrection };

        // Constructor using Laguerre integration
        // and Gatheral's version of complex log.
        AnalyticHestonADEngine(const boost::shared_ptr<HestonModel>& model,
                               Size integrationOrder = 144);


        void calculate() const;
        Size numberOfEvaluations() const;

        static void doCalculation(Real riskFreeDiscount,
                                             Real dividendDiscount,
                                             Real spotPrice,
                                             Real strikePrice,
                                             Real term,
                                             Real kappa, Real theta, Real sigma, Real v0, Real rho,
                                             const TypePayoff& type,
                                             const Integration& integration,
                                             const ComplexLogFormula cpxLog,
                                             const AnalyticHestonADEngine* const enginePtr,
                                             VanillaOption::results& results,
                                             Size& evaluations);

      protected:

      private:
        class Fj_Helper;

        mutable Size evaluations_;
        const ComplexLogFormula cpxLog_;
        const boost::shared_ptr<Integration> integration_;
    };


    class AnalyticHestonADEngine::Integration {
      public:
        // non adaptive integration algorithms based on Gaussian quadrature
        static Integration gaussLaguerre    (Size integrationOrder = 128);
        static Integration gaussLegendre    (Size integrationOrder = 128);
        static Integration gaussChebyshev   (Size integrationOrder = 128);
        static Integration gaussChebyshev2nd(Size integrationOrder = 128);

        CppAD::AD<Real> calculate(
        	const boost::function<CppAD::AD<Real>(Real)>& f) const;

        Size numberOfEvaluations() const;

      private:
        enum Algorithm
            { GaussLaguerre, GaussLegendre,
              GaussChebyshev, GaussChebyshev2nd };

        Integration(Algorithm intAlgo,
                    const boost::shared_ptr<GaussianADQuadrature>& quadrature);

        const Algorithm intAlgo_;
        const boost::shared_ptr<GaussianADQuadrature> gaussianQuadrature_;
    };
}

#endif
