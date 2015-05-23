/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file splinedensitysmilesection.hpp
    \brief smilesection using spline density
*/

#ifndef quantlib_splinedensity_smile_section_hpp
#define quantlib_splinedensity_smile_section_hpp

#include <ql/qldefines.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/termstructures/volatility/smilesectionutils.hpp>
#include <ql/math/interpolation.hpp>
#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/optimization/costfunction.hpp>

namespace QuantLib {

    class SplineDensitySmileSection : public SmileSection {

      public:
        SplineDensitySmileSection(const boost::shared_ptr<SmileSection> source,
                                  const Real atm = Null<Real>(),
                                  const Real lowerBound = 0.0,
                                  const Real upperBound = 2.0,
                                  const bool deleteArbitragePoints = false,
                                  const std::vector<Real> &moneynessGrid =
                                      std::vector<Real>());

        Real minStrike() const { return lowerBound_; }
        Real maxStrike() const { return QL_MAX_REAL; }
        Real atmLevel() const { return f_; }

        Real leftCoreStrike() const { return k_[1]; }
        Real rightCoreStrike() const { return k_[k_.size() - 2]; }

        Real optionPrice(Rate strike, Option::Type type = Option::Call,
                         Real discount = 1.0) const;
        Real digitalOptionPrice(Rate strike, Option::Type type = Option::Call,
                                Real discount = 1.0, Real gap = 1.0e-5) const;
        Real density(Rate strike, Real discount = 1.0, Real gap = 1.0E-4) const;

      protected:
        Volatility volatilityImpl(Rate strike) const;

      private:
        void update();
        Disposable<Array> setDensity(const Array &d);
        Real setDensity(const Real d, const Size i);
        Size index(Real strike) const;

        const boost::shared_ptr<SmileSection> source_;
        const Real lowerBound_, upperBound_;
        Real f_;
        std::vector<Real> k_, c_, d_;
        std::vector<Real> lambda_, eta_;
        boost::shared_ptr<SmileSectionUtils> ssutils_;

        //boost::shared_ptr<CubicInterpolation> density_;
        boost::shared_ptr<LinearInterpolation> density_;
        Real etaSum_,adjustment_,expectation_;

        class SplineDensityCostFunction : public CostFunction {
          public:
            SplineDensityCostFunction(SplineDensitySmileSection *section)
                : section_(section) {}
            Real value(const Array &x) const {
                Real res = 0.0;
                Array vals = values(x);
                for (Size i = 0; i < vals.size(); i++) {
                    res += vals[i] * vals[i];
                }
                return sqrt(res) / vals.size();
            }
            Disposable<Array> values(const Array &x) const {
                Array res = section_->setDensity(x);
                return res;
            }

          private:
            SplineDensitySmileSection *section_;
        };

        class SplineDensityCostFunction2 : public CostFunction {
          public:
            SplineDensityCostFunction2(SplineDensitySmileSection *section,
                                      const Size i)
                : section_(section), i_(i) {}
            Real value(const Array &x) const {
                Real res = 0.0;
                Array vals = values(x);
                for (Size i = 0; i < vals.size(); i++) {
                    res += vals[i] * vals[i];
                }
                return sqrt(res) / vals.size();
            }
            Disposable<Array> values(const Array &x) const {
                Array res = Array(1, section_->setDensity(x[0], i_));
                return res;
            }

          private:
            SplineDensitySmileSection *section_;
            const Size i_;
        };
    };
}

#endif
