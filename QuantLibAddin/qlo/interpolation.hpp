
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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

#include <oh/libraryobject.hpp>
#include <ql/math/interpolations/cubicspline.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class Extrapolator;
    class EndCriteria;
    class OptimizationMethod;
}

namespace QuantLibAddin {

    OH_LIB_CLASS(Extrapolator, QuantLib::Extrapolator);

    class Interpolation : public Extrapolator {
      public:
        Interpolation(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                      const std::vector<QuantLib::Real>& x,
                      const std::vector<QuantLib::Real>& y,
                      bool permanent);
    };

    class LinearInterpolation : public Interpolation {
      public:
          LinearInterpolation(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                              const std::string& linearInterpolationType,
                              const std::vector<QuantLib::Real>& x,
                              const std::vector<QuantLib::Real>& y,
                              bool permanent);
    };
    
    class CubicSplineInterpolation : public Interpolation {
      public:
        CubicSplineInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Real>& y,
            QuantLib::CubicSpline::BoundaryCondition leftCondition,
            QuantLib::Real leftConditionValue,
            QuantLib::CubicSpline::BoundaryCondition rightCondition,
            QuantLib::Real rightConditionValue,
            bool monotonicityConstraint,
            bool permanent);
    };
    
    class SABRInterpolation : public Interpolation {
      public:
        SABRInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
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
            const boost::shared_ptr<QuantLib::OptimizationMethod>& om,
            bool permanent);
      private:
        QuantLib::Real forward_;
    };

}

#endif

