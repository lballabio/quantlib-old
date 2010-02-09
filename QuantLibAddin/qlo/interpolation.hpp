/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2009, 2010 Ferdinando Ametrano

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

#ifndef qla_interpolation_hpp
#define qla_interpolation_hpp

#include <qlo/extrapolator.hpp>

#include <ql/math/interpolations/mixedinterpolation.hpp>
#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>
#include <ql/math/interpolations/abcdinterpolation.hpp>

#include <ql/patterns/lazyobject.hpp>
#include <ql/quote.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class Extrapolator;
    class EndCriteria;
    class OptimizationMethod;

    template<class T>
    class Handle;

    class Quote;
}

namespace QuantLibAddin {

    class Interpolation : public Extrapolator, public QuantLib::LazyObject {
      public:
        QuantLib::Real operator()(QuantLib::Real x,
                                  bool allowExtrapolation) const {
            calculate();
            return qlInterpolation_->operator()(x, allowExtrapolation);
        }
        QuantLib::Real primitive(QuantLib::Real x,
                                 bool allowExtrapolation) const {
            calculate();
            return qlInterpolation_->primitive(x, allowExtrapolation);
        }
        QuantLib::Real derivative(QuantLib::Real x,
                                  bool allowExtrapolation) const {
            calculate();
            return qlInterpolation_->derivative(x, allowExtrapolation);
        }
        QuantLib::Real secondDerivative(QuantLib::Real x,
                                        bool allowExtrapolation) const {
            calculate();
            return qlInterpolation_->secondDerivative(x, allowExtrapolation);
        }
        void performCalculations() const;
      protected:
        Interpolation(const boost::shared_ptr<ObjectHandler::ValueObject>&,
                      const std::vector<QuantLib::Real>& x,
                      const std::vector<QuantLib::Handle<QuantLib::Quote> >& yh,
                      bool permanent);
        QuantLib::Size n_;
        std::vector<QuantLib::Real> x_;
        mutable std::vector<QuantLib::Real> y_;
        std::vector<QuantLib::Handle<QuantLib::Quote> > yh_;
        boost::shared_ptr<QuantLib::Interpolation> qlInterpolation_;
    };

    class GenericInterp : public Interpolation {
      public:
        GenericInterp(const boost::shared_ptr<ObjectHandler::ValueObject>&,
                      const std::string& type,
                      const std::vector<QuantLib::Real>& x,
                      const std::vector<QuantLib::Handle<QuantLib::Quote> >& y,
                      bool permanent);
    };
    
    class MixedLinearCubicInterpolation : public Interpolation {
      public:
        MixedLinearCubicInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& y,
            QuantLib::Size n,
            QuantLib::CubicInterpolation::DerivativeApprox da,
            bool monotonic,
            QuantLib::CubicInterpolation::BoundaryCondition leftCondition,
            QuantLib::Real leftConditionValue,
            QuantLib::CubicInterpolation::BoundaryCondition rightCondition,
            QuantLib::Real rightConditionValue,
            bool permanent);
        //QuantLib::Size switchIndex() const {
        //    calculate();
        //    return qlMixedLinearCubicInterpolation_->switchIndex();
        //}
      protected:
        boost::shared_ptr<QuantLib::MixedLinearCubicInterpolation> qlMixedLinearCubicInterpolation_;
    };

    class CubicInterpolation : public Interpolation {
      public:
        CubicInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& y,
            QuantLib::CubicInterpolation::DerivativeApprox da,
            bool monotonic,
            QuantLib::CubicInterpolation::BoundaryCondition leftCondition,
            QuantLib::Real leftConditionValue,
            QuantLib::CubicInterpolation::BoundaryCondition rightCondition,
            QuantLib::Real rightConditionValue,
            bool permanent);
        const std::vector<QuantLib::Real>& primitiveConstants() const {
            calculate();
            return qlCubicInterpolation_->primitiveConstants();
        }
        const std::vector<QuantLib::Real>& aCoefficients() const {
            calculate();
            return qlCubicInterpolation_->aCoefficients();
        }
        const std::vector<QuantLib::Real>& bCoefficients() const {
            calculate();
            return qlCubicInterpolation_->bCoefficients();
        }
        const std::vector<QuantLib::Real>& cCoefficients() const {
            calculate();
            return qlCubicInterpolation_->cCoefficients();
        }
        const std::vector<bool>& monotonicityAdjustments() const {
            calculate();
            return qlCubicInterpolation_->monotonicityAdjustments();
        }
      protected:
        boost::shared_ptr<QuantLib::CubicInterpolation> qlCubicInterpolation_;
    };
    
    class AbcdInterpolation : public Interpolation {
      public:
        AbcdInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& y,
            QuantLib::Real a,
            QuantLib::Real b,
            QuantLib::Real c,
            QuantLib::Real d,
            bool aIsFixed,
            bool bIsFixed,
            bool cIsFixed,
            bool dIsFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria>& ec,
            const boost::shared_ptr<QuantLib::OptimizationMethod>& om,
            bool permanent);
        QuantLib::Real a() const {
            calculate();
            return qlAbcdInterpolation_->a();
        }
        QuantLib::Real b() const {
            calculate();
            return qlAbcdInterpolation_->b();
        }
        QuantLib::Real c() const {
            calculate();
            return qlAbcdInterpolation_->c();
        }
        QuantLib::Real d() const {
            calculate();
            return qlAbcdInterpolation_->d();
        }
        QuantLib::Real rmsError() const {
            calculate();
            return qlAbcdInterpolation_->rmsError();
        }
        QuantLib::Real maxError() const {
            calculate();
            return qlAbcdInterpolation_->maxError();
        }
        QuantLib::EndCriteria::Type endCriteria() const {
            calculate();
            return qlAbcdInterpolation_->endCriteria();
        }
      protected:
        boost::shared_ptr<QuantLib::AbcdInterpolation> qlAbcdInterpolation_;
    };

    class SABRInterpolation : public Interpolation {
      public:
        SABRInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& y,
            QuantLib::Time t,
            QuantLib::Handle<QuantLib::Quote> forward,
            QuantLib::Real alpha,
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool alphaIsFixed,
            bool betaIsFixed,
            bool nuIsFixed,
            bool rhoIsFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria>& ec,
            const boost::shared_ptr<QuantLib::OptimizationMethod>& om,
            bool permanent);
        QuantLib::Real alpha() const {
            calculate();
            return qlSABRInterpolation_->alpha();
        }
        QuantLib::Real beta() const {
            calculate();
            return qlSABRInterpolation_->beta();
        }
        QuantLib::Real nu() const {
            calculate();
            return qlSABRInterpolation_->nu();
        }
        QuantLib::Real rho() const {
            calculate();
            return qlSABRInterpolation_->rho();
        }
        const std::vector<QuantLib::Real>& interpolationWeights() const {
            calculate();
            return qlSABRInterpolation_->interpolationWeights();
        }
        QuantLib::Real rmsError() const {
            calculate();
            return qlSABRInterpolation_->rmsError();
        }
        QuantLib::Real maxError() const {
            calculate();
            return qlSABRInterpolation_->maxError();
        }
        QuantLib::EndCriteria::Type endCriteria() const {
            calculate();
            return qlSABRInterpolation_->endCriteria();
        }
        void performCalculations() const {
            forward_ = forwardh_->value();
            Interpolation::performCalculations();
        }
      protected:
        QuantLib::Handle<QuantLib::Quote> forwardh_;
        mutable QuantLib::Real forward_;
        boost::shared_ptr<QuantLib::SABRInterpolation> qlSABRInterpolation_;
    };

}

#endif
