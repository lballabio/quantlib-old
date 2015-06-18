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

#ifndef quantlib_abcdcalibration_hpp
#define quantlib_abcdcalibration_hpp


#include <ql/math/optimization/endcriteria.hpp>
#include <ql/math/optimization/projectedcostfunction.hpp>
#include <ql/math/array.hpp>

namespace QuantLib {
    
    class Quote;
    class OptimizationMethod;
    class ParametersTransformation;

    class AbcdCalibration2 {
      private:
        class AbcdError : public CostFunction {
          public:
            AbcdError(AbcdCalibration2* abcd) : abcd_(abcd) {}

            Real value(const Array& x) const {
                const Array y = abcd_->transformation_->direct(x);
                abcd_->a_ = y[0];
                abcd_->b_ = y[1];
                abcd_->c_ = y[2];
                abcd_->d_ = y[3];
                return abcd_->error();
            }
            Disposable<Array> values(const Array& x) const {
                const Array y = abcd_->transformation_->direct(x);
                abcd_->a_ = y[0];
                abcd_->b_ = y[1];
                abcd_->c_ = y[2];
                abcd_->d_ = y[3];
                return abcd_->errors();
            }
          private:
            AbcdCalibration2* abcd_;
        };

        class AbcdParametersTransformation : public ParametersTransformation {
          public:
            AbcdParametersTransformation() : y_(Array(4)) {}
            // to constrained <- from unconstrained
            Array direct(const Array& x) const;
            // to unconstrained <- from constrained
            Array inverse(const Array& x) const;
          private:
            mutable Array y_;
        };

      public:
        AbcdCalibration2(const std::vector<Time>& t,
                         const std::vector<Rate>& rates,
                         const std::vector<Real>& weights,
                         Real aGuess = -0.06,
                         Real bGuess = 0.17,
                         Real cGuess = 0.54,
                         Real dGuess = 0.17,
                         bool aIsFixed = false,
                         bool bIsFixed = false,
                         bool cIsFixed = false,
                         bool dIsFixed = false,
                         const boost::shared_ptr<EndCriteria>& endCriteria
                             = boost::shared_ptr<EndCriteria>(),
                         const boost::shared_ptr<OptimizationMethod>& method
                             = boost::shared_ptr<OptimizationMethod>());
        AbcdCalibration2(const std::vector<Time>& t,
                         const std::vector<Rate>& rates,
                         const std::vector<Real>& weights,
                         std::vector<Real> coeff,
                         const std::vector<bool>& fixedCoeff,
                         const boost::shared_ptr<EndCriteria>& endCriteria
                             = boost::shared_ptr<EndCriteria>(),
                         const boost::shared_ptr<OptimizationMethod>& method
                             = boost::shared_ptr<OptimizationMethod>());

        //! adjustment factors needed to match Mkt value
        std::vector<Real> k() const;
        void compute();
        //calibration results
        Real value(Time t) const;
        Real error() const;
        Real maxError() const;
        Disposable<Array> errors() const;
        EndCriteria::Type endCriteria() const;
        Real a() const { return a_; }
        Real b() const { return b_; }
        Real c() const { return c_; }
        Real d() const { return d_; }
        std::vector<Real> coefficients() const;

    private:
        bool aIsFixed_, bIsFixed_, cIsFixed_, dIsFixed_;
        Real a_, b_, c_, d_;
        boost::shared_ptr<ParametersTransformation> transformation_;
        void initialize_();
        // optimization method used for fitting
        mutable EndCriteria::Type abcdEndCriteria_;
        boost::shared_ptr<EndCriteria> endCriteria_;
        boost::shared_ptr<OptimizationMethod> optMethod_;
        std::vector<Time> t_;
        std::vector<Rate> rates_;
        std::vector<Real> weights_;
    };

    class PolynomialCalibration {

    private:

        class PolynomialError : public CostFunction {
        public:
            PolynomialError(PolynomialCalibration* p) : p_(p) {}

            Real value(const Array& x) const {
                for (Size i = 0; i < x.size(); ++i)
                    p_->coeff_[i] = x[i];
                return p_->error();
            }
            Disposable<Array> values(const Array& x) const {
                for (Size i = 0; i < x.size(); ++i)
                    p_->coeff_[i] = x[i];
                return p_->errors();
            }
        private:
            PolynomialCalibration* p_;
        };

    public:
        PolynomialCalibration(
                            const std::vector<Time>& t,
                            const std::vector<Rate>& rates,
                            const std::vector<Real>& weights,
                            std::vector<Real> coeff, 
                            const std::vector<bool>& fixedCoeff,
                            const boost::shared_ptr<EndCriteria>& endCriteria
                                = boost::shared_ptr<EndCriteria>(),
                            const boost::shared_ptr<OptimizationMethod>& method
                                = boost::shared_ptr<OptimizationMethod>());

        //! adjustment factors needed to match Mkt value
        std::vector<Real> k() const;
        void compute();
        //calibration results
        Real value(Time t) const;
        Real error() const;
        Real maxError() const;
        Disposable<Array> errors() const;
        EndCriteria::Type endCriteria() const;

        /*! Inspectors */
        const std::vector<Real>& coeff() { return coeff_; }

        std::vector<bool> fixedCoeff_;
        std::vector<Real> coeff_;

    private:
        // optimization method used for fitting
        mutable EndCriteria::Type polynomialEndCriteria_;
        boost::shared_ptr<EndCriteria> endCriteria_;
        boost::shared_ptr<OptimizationMethod> optMethod_;
        std::vector<Time> t_;
        std::vector<Rate> rates_;
        std::vector<Real> weights_;
    };

}

#endif
