/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2011 Peter Caspers

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

/*! \file svismilesmilesection.hpp
    \brief Arbitrage free smile section using the 
           svi parametrization by Gatheral
*/

#ifndef quantlib_svi_smile_section_hpp
#define quantlib_svi_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/math/optimization/bfgs.hpp>
#include <ql/math/optimization/costfunction.hpp>
#include <ql/math/optimization/constraint.hpp>
#include <ql/math/optimization/endcriteria.hpp>
#include <ql/pricingengines/blackformula.hpp>

namespace QuantLib {

class SviSmileSection : public SmileSection {

  public:
    SviSmileSection(
        const boost::shared_ptr<SmileSection> source,
        const Real atm = Null<Real>(),
        const std::vector<Real> &moneynessGrid = std::vector<Real>());

    Real minStrike() const { return 0.0; }
    Real maxStrike() const { return QL_MAX_REAL; }
    Real atmLevel() const { return f_; }

  protected:
    Volatility volatilityImpl(Rate strike) const;
    Real varianceImpl(Rate strike) const;

  private:
    class SviCostFunction : public CostFunction {
      public:
        SviCostFunction(SviSmileSection *sviSection) : svi_(sviSection) {}
        Real value(const Array &x) const {
            Real res = 0.0;
            Array vals = values(x);
            for (Size i = 0; i < vals.size(); i++) {
                res += vals[i] * vals[i];
            }
            return sqrt(res) / vals.size();
        }
        Disposable<Array> values(const Array &x) const {
            svi_->a_ = x[0];
            svi_->b_ = x[1];
            svi_->s_ = x[2];
            svi_->r_ = x[3];
            svi_->m_ = x[4];
            Array res(svi_->k_.size());
            for (Size i = 0; i < svi_->k_.size(); i++) {
                res[i] = svi_->source_->variance(svi_->k_[i]) -
                         svi_->variance(svi_->k_[i]);
            }
            return res;
        }

      private:
        SviSmileSection *svi_;
    };

    class SviConstraint : public Constraint {
      private:
        class Impl : public Constraint::Impl {
          private:
            Real T_;

          public:
            Impl(const Real T) : T_(T) {}
            bool test(const Array &params) const {
                return params[1] * (1.0 + fabs(params[3])) < 4.0 / T_;
            }
        };

      public:
        SviConstraint(const Real T)
            : Constraint(boost::shared_ptr<Constraint::Impl>(
                  new SviConstraint::Impl(T))) {}
    };

    void compute();
    boost::shared_ptr<SmileSection> source_;
    std::vector<Real> k_;
    Real f_;
    Real a_, b_, s_, r_, m_; // svi parameters
};
}

#endif
