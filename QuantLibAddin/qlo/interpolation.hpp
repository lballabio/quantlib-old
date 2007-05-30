
/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#include <oh/objecthandler.hpp>
#include <ql/math/optimization/method.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/interpolations/cubicspline.hpp>
#include <ql/math/matrix.hpp>

namespace QuantLibAddin {

    class Extrapolator :
        public ObjectHandler::LibraryObject<QuantLib::Extrapolator> {
    };

    class Interpolation : public Extrapolator {
      public:
        Interpolation(const std::vector<QuantLib::Real>& x,
                      const std::vector<QuantLib::Real>& y);
      protected:
        std::vector<QuantLib::Real> x_, y_;
    };

    class LinearInterpolation : public Interpolation {
      public:
          LinearInterpolation(const std::string &linearInterpolationType,
                              const std::vector<QuantLib::Real>& x,
                              const std::vector<QuantLib::Real>& y);
    };
    
    class CubicSplineInterpolation : public Interpolation {
      public:
        CubicSplineInterpolation(
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Real>& y,
            QuantLib::CubicSpline::BoundaryCondition leftCondition,
            double leftConditionValue,
            QuantLib::CubicSpline::BoundaryCondition rightCondition,
            double rightConditionValue,
            bool monotonicityConstraint);
    };
    
    class SABRInterpolation : public Interpolation {
      public:
        SABRInterpolation(
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Real>& y,
            QuantLib::Time t,
            QuantLib::Rate forward,
            QuantLib::Real alpha,
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria>& ec,
            const boost::shared_ptr<QuantLib::OptimizationMethod>& om);
      private:
        QuantLib::Real forward_;
    };

}

#endif
