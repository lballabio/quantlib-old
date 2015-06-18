/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Ferdinando Ametrano
 Copyright (C) 2015 Paolo Mazzocchi

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

#include <ql/experimental/tenorbasis/tenorbasiscalibration.hpp>
#include <ql/math/optimization/method.hpp>
#include <ql/math/optimization/constraint.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#include <ql/math/polynomialmathfunction.hpp>
#include <ql/math/abcdmathfunction.hpp>

namespace QuantLib {

    // to constrained <- from unconstrained
    Array AbcdCalibration2::AbcdParametersTransformation::direct(const Array& x) const {
        // c <- ~c with c>0
        //y_[2] = x[2]*x[2] + QL_EPSILON;
        //y_[2] = std::abs(x[2]) + QL_EPSILON;
        y_[2] = std::exp(x[2]);

        // d <- ~d with d>=0
        //y_[3] = x[3]*x[3];
        //y_[3] = std::abs(x[3]);
        //y_[3] = std::exp(x[3]);
        y_[3] = std::exp(-x[3]*x[3]); // 1>=d>0

        // a <- ~a with a+d>=0
        y_[0] = x[0]*x[0] - y_[3];
        //y_[0] = std::abs(x[0]) - y_[3];
        //y_[0] = std::exp(x[0]) - y_[3];
        //y_[0] = std::exp(-x[0]*x[0]) - y_[3]; // 1>=a>0

        // b <- ~b with b>=0
        y_[1] = x[1]*x[1];
        //y_[1] = std::abs(x[1]);
        //y_[1] = std::exp(x[1]);

        return y_;
    }

    // to unconstrained <- from constrained
    Array AbcdCalibration2::AbcdParametersTransformation::inverse(const Array& x) const {
        // ~c <- c with c>0
        //y_[2] = std::sqrt(x[2]); // arbitrary sign for y_
        //y_[2] = x[2]; // arbitrary sign for y_
        y_[2] = std::log(x[2]);

        // ~d <- d with d>=0
        //y_[3] = std::sqrt(x[3]); // arbitrary sign for y_
        //y_[3] = x[3]; // arbitrary sign for y_
        //y_[3] = (x[3]==0 ? QL_MIN_REAL : std::log(x[3]));
        y_[3] = std::sqrt((x[3]==0 ? -std::log(QL_EPSILON) : -std::log(x[3]))); // arbitrary sign for y_, 1>=d>0

        // ~a <- a with a+d>=0
        y_[0] = std::sqrt(x[0] + x[3]); // arbitrary sign for y_
        //y_[0] = (x[0] + x[3]); // arbitrary sign for y_
        //y_[0] = (x[0]+x[3]==0 ? QL_MIN_REAL : std::log(x[0]+x[3]));
        //y_[0] = std::sqrt((x[0]+x[3]==0 ? -std::log(QL_EPSILON) : -std::log(x[0]+x[3]))); // arbitrary sign for y_, 1>=a>0

        // ~b <- b with b>0
        y_[1] = std::sqrt(x[1]); // arbitrary sign for y_
        //y_[1] = x[1]; // arbitrary sign for y_
        //y_[1] = std::log(x[1]);

        return y_;
    }

    AbcdCalibration2::AbcdCalibration2(
                    const std::vector<Time>& t,
                    const std::vector<Rate>& rates,
                    const std::vector<Real>& weights,
                    Real a, Real b, Real c, Real d,
                    bool aIsFixed, bool bIsFixed, bool cIsFixed, bool dIsFixed,
                    const boost::shared_ptr<EndCriteria>& endCriteria,
                    const boost::shared_ptr<OptimizationMethod>& optMethod)
    : t_(t), rates_(rates), weights_(weights), a_(a), b_(b), c_(c), d_(d),
      aIsFixed_(aIsFixed), bIsFixed_(bIsFixed), cIsFixed_(cIsFixed), 
      dIsFixed_(dIsFixed),abcdEndCriteria_(EndCriteria::None), 
      endCriteria_(endCriteria), optMethod_(optMethod) {

        QL_REQUIRE(t.size() == rates.size(),
            "mismatch between number of t (" << t.size() <<
            ") and rates (" << rates.size() << ")");

        if (weights.empty())
            weights_ = std::vector<Real>(t.size(), 1.0);

        QL_REQUIRE(weights_.size() == rates.size(),
            "mismatch between number of weights (" << weights_.size() <<
            ") and rates (" << rates.size() << ")");
        initialize_();
    }

    AbcdCalibration2::AbcdCalibration2(
                        const std::vector<Time>& t,
                        const std::vector<Rate>& rates,
                        const std::vector<Real>& weights,
                        std::vector<Real> coeff,
                        const std::vector<bool>& fixedCoeff,
                        const boost::shared_ptr<EndCriteria>& endCriteria,
                        const boost::shared_ptr<OptimizationMethod>& optMethod)
    : t_(t), rates_(rates), weights_(weights), 
      abcdEndCriteria_(EndCriteria::None), endCriteria_(endCriteria),
      optMethod_(optMethod) {

        QL_REQUIRE(coeff.size() == 4, "input vector must be of size four");
        a_ = coeff[0];
        b_ = coeff[1];
        c_ = coeff[2];
        d_ = coeff[3];

        QL_REQUIRE(fixedCoeff.size() == 4, "fixed parameters vector must"
                                            "be of size four");
        aIsFixed_ = fixedCoeff[0];
        bIsFixed_ = fixedCoeff[1];
        cIsFixed_ = fixedCoeff[2];
        dIsFixed_ = fixedCoeff[3];

        validateAbcdParameters(a_, b_, c_, d_);

        QL_REQUIRE(t.size() == rates.size(),
            "mismatch between number of t (" << t.size() <<
            ") and rates (" << rates.size() << ")");

        if (weights.empty())
            weights_ = std::vector<Real>(t.size(), 1.0);

        QL_REQUIRE(weights_.size() == rates.size(),
            "mismatch between number of weights (" << weights_.size() <<
            ") and rates (" << rates.size() << ")");
        initialize_();
    }

    void AbcdCalibration2::initialize_(){
        // weight normalization
        Real weightsSum = 0.0;
        for (Size i = 0; i<t_.size(); i++)
            weightsSum += weights_[i];
        for (Size i = 0; i<t_.size(); i++)
            weights_[i] /= weightsSum;

        // if no optimization method or endCriteria is provided, we provide one
        if (!optMethod_) {
            Real epsfcn = 1.0e-9;
            Real xtol = 1.0e-9;
            Real gtol = 1.0e-9;
            bool useCostFunctionsJacobian = false;
            optMethod_ = boost::shared_ptr<OptimizationMethod>(new
                LevenbergMarquardt(epsfcn, xtol, gtol, useCostFunctionsJacobian));
        }
        if (!endCriteria_) {
            Size maxIterations = 10000;
            Size maxStationaryStateIterations = 1000;
            Real rootEpsilon = 1.0e-9;
            Real functionEpsilon = 0.3e-9;     // Why 0.3e-4 ?
            Real gradientNormEpsilon = 0.3e-9; // Why 0.3e-4 ?
            endCriteria_ = boost::shared_ptr<EndCriteria>(new
                EndCriteria(maxIterations, maxStationaryStateIterations,
                            rootEpsilon, functionEpsilon, gradientNormEpsilon));
        }
    }

    std::vector<Real> AbcdCalibration2::coefficients() const{
        std::vector<Real> coeff;
        coeff[0] = a_;
        coeff[1] = b_;
        coeff[2] = c_;
        coeff[3] = d_;
        return coeff;
    }

    void AbcdCalibration2::compute() {
        // there is nothing to optimize
        if (aIsFixed_ && bIsFixed_ && cIsFixed_ && dIsFixed_) {
            abcdEndCriteria_ = EndCriteria::None;
            //error_ = interpolationError();
            //maxError_ = interpolationMaxError();
            return;
        }
        else {

            AbcdError costFunction(this);
            transformation_ = boost::shared_ptr<ParametersTransformation>(new
                AbcdParametersTransformation);

            Array guess(4);
            guess[0] = a_;
            guess[1] = b_;
            guess[2] = c_;
            guess[3] = d_;

            std::vector<bool> parameterAreFixed(4);
            parameterAreFixed[0] = aIsFixed_;
            parameterAreFixed[1] = bIsFixed_;
            parameterAreFixed[2] = cIsFixed_;
            parameterAreFixed[3] = dIsFixed_;

            Array inversedTransformatedGuess(transformation_->inverse(guess));

            ProjectedCostFunction projectedAbcdCostFunction(costFunction,
                inversedTransformatedGuess, parameterAreFixed);

            Array projectedGuess
                (projectedAbcdCostFunction.project(inversedTransformatedGuess));

            NoConstraint constraint;
            Problem problem(projectedAbcdCostFunction, constraint, projectedGuess);
            abcdEndCriteria_ = optMethod_->minimize(problem, *endCriteria_);
            Array projectedResult(problem.currentValue());
            Array transfResult(projectedAbcdCostFunction.include(projectedResult));

            Array result = transformation_->direct(transfResult);
            validateAbcdParameters(a_, b_, c_, d_);
            a_ = result[0];
            b_ = result[1];
            c_ = result[2];
            d_ = result[3];
        }
    }

    Real AbcdCalibration2::value(Time t) const {
        AbcdMathFunction model(a_, b_, c_, d_);
        return model(t);
    }

    std::vector<Real> AbcdCalibration2::k() const {
        std::vector<Real> k(t_.size());
        for (Size i = 0; i<t_.size(); i++) {
            k[i] = rates_[i] / value(t_[i]);
        }
        return k;
    }

    Real AbcdCalibration2::error() const {
        Size n = t_.size();
        Real error, squaredError = 0.0;
        for (Size i = 0; i<t_.size(); i++) {
            error = (value(t_[i]) - rates_[i]);
            squaredError += error * error * weights_[i];
        }
        return std::sqrt(n*squaredError / (n - 1));
    }

    Real AbcdCalibration2::maxError() const {
        Real error, maxError = QL_MIN_REAL;
        for (Size i = 0; i<t_.size(); i++) {
            error = std::fabs(value(t_[i]) - rates_[i]);
            maxError = std::max(maxError, error);
        }
        return maxError;
    }

    // calculate weighted differences
    Disposable<Array> AbcdCalibration2::errors() const {
        Array results(t_.size());
        for (Size i = 0; i<t_.size(); i++) {
            results[i] = (value(t_[i]) - rates_[i])* std::sqrt(weights_[i]);
        }
        return results;
    }

    EndCriteria::Type AbcdCalibration2::endCriteria() const{
        return abcdEndCriteria_;
    }

    PolynomialCalibration::PolynomialCalibration(
                           const std::vector<Time>& t,
                           const std::vector<Rate>& rates,
                           const std::vector<Real>& weights,
                           std::vector<Real> coeff,
                           const std::vector<bool>& fixedCoeff,
                           const boost::shared_ptr<EndCriteria>& endCriteria,
                           const boost::shared_ptr<OptimizationMethod>& method)
    : fixedCoeff_(fixedCoeff), coeff_(coeff), endCriteria_(endCriteria),
      polynomialEndCriteria_(EndCriteria::None), optMethod_(method), 
      t_(t), rates_(rates), weights_(weights) {

        QL_REQUIRE(t.size() == rates.size(),
            "mismatch between number of t (" << t.size() <<
            ") and rates (" << rates.size() << ")");

        if (fixedCoeff.empty())
            fixedCoeff_ = std::vector<bool>(coeff_.size(), false);

        if (weights.empty())
            weights_ = std::vector<Real>(t.size(), 1.0);

        QL_REQUIRE(weights_.size() == rates.size(),
            "mismatch between number of weights (" << weights_.size() <<
            ") and rates (" << rates.size() << ")");

        // weight normalization
        Real weightsSum = 0.0;
        for (Size i = 0; i<t_.size(); i++)
            weightsSum += weights_[i];
        for (Size i = 0; i<t_.size(); i++)
            weights_[i] /= weightsSum;

        // if no optimization method or endCriteria is provided, we provide one
        if (!optMethod_)
            optMethod_ = boost::shared_ptr<OptimizationMethod>(new
            LevenbergMarquardt(1e-8, 1e-8, 1e-8));
        //method_ = boost::shared_ptr<OptimizationMethod>(new
        //    Simplex(0.01));
        if (!endCriteria_)
            //endCriteria_ = boost::shared_ptr<EndCriteria>(new
            //    EndCriteria(60000, 100, 1e-8, 1e-8, 1e-8));
            endCriteria_ = boost::shared_ptr<EndCriteria>(new
            EndCriteria(1000, 100, 1.0e-8, 0.3e-4, 0.3e-4));   // Why 0.3e-4 ?
    }

    void PolynomialCalibration::compute() {
        // there is nothing to optimize
        QL_REQUIRE(fixedCoeff_.size() == coeff_.size(),
            "mismatch between number of coefficients (" << coeff_.size() <<
            ") and fixed coefficients (" << fixedCoeff_.size() << ")");
        bool check = true;
        for (Size i = 0; i<fixedCoeff_.size(); ++i){
            if (fixedCoeff_[i]==false)
                check = false;
        }

        if (check) {
            polynomialEndCriteria_ = EndCriteria::None;
            return;
        }
        else {

            PolynomialError costFunction(this);
            //transformation_ = boost::shared_ptr<ParametersTransformation>(new
            //    AbcdParametersTransformation);

            Array guess(coeff_.size());
            std::vector<bool> parameterAreFixed(coeff_.size());

            for (Size i = 0; i<coeff_.size(); ++i){
                guess[i] = coeff_[i];
                parameterAreFixed[i] = fixedCoeff_[i];
            }

            ProjectedCostFunction projectedPolynomialCostFunction(costFunction,
                guess, parameterAreFixed);

            Array projectedGuess
                              (projectedPolynomialCostFunction.project(guess));

            NoConstraint constraint;
            Problem problem
                 (projectedPolynomialCostFunction, constraint, projectedGuess);
            polynomialEndCriteria_ = 
                                  optMethod_->minimize(problem, *endCriteria_);
            Array projectedResult(problem.currentValue());
            Array result
                    (projectedPolynomialCostFunction.include(projectedResult));

            for (Size i = 0; i<coeff_.size(); ++i){
                coeff_[i] = result[i];
            }
        }
    }

    Real PolynomialCalibration::value(Time t) const {
        PolynomialFunction model(coeff_);
        return model(t);
    }

    std::vector<Real> PolynomialCalibration::k() const {
        std::vector<Real> k(t_.size());
        for (Size i = 0; i<t_.size(); i++) {
            k[i] = rates_[i] / value(t_[i]);
        }
        return k;
    }

    Real PolynomialCalibration::error() const {
        Size n = t_.size();
        Real error, squaredError = 0.0;
        for (Size i = 0; i<t_.size(); i++) {
            error = (value(t_[i]) - rates_[i]);
            squaredError += error * error * weights_[i];
        }
        return std::sqrt(n*squaredError / (n - 1));
    }

    Real PolynomialCalibration::maxError() const {
        Real error, maxError = QL_MIN_REAL;
        for (Size i = 0; i<t_.size(); i++) {
            error = std::fabs(value(t_[i]) - rates_[i]);
            maxError = std::max(maxError, error);
        }
        return maxError;
    }

    // calculate weighted differences
    Disposable<Array> PolynomialCalibration::errors() const {
        Array results(t_.size());
        for (Size i = 0; i<t_.size(); i++) {
            results[i] = (value(t_[i]) - rates_[i])* std::sqrt(weights_[i]);
        }
        return results;
    }

    EndCriteria::Type PolynomialCalibration::endCriteria() const{
        return polynomialEndCriteria_;
    }

}
