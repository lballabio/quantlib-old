
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/interpolation.hpp>
#include <qlo/Enumerations/Factories/interpolationsfactory.hpp>

#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>

#include <ql/math/optimization/method.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/matrix.hpp>

namespace QuantLibAddin {

    Interpolation::Interpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        bool permanent)
    : Extrapolator(properties, permanent), x_(x), y_(y) {
        QL_REQUIRE(x.size()==y.size(),
                  "unmatched size between x (" << x.size() <<
                  ") and y(" << y.size() << ")");
    }

    LinearInterpolation::LinearInterpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::string &linearInterpolationType, 
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        bool permanent)
    : Interpolation(properties, x, y, permanent) {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::Interpolation> >()
            (linearInterpolationType, x_.begin(), x_.end(), y_.begin());
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    CubicSplineInterpolation::CubicSplineInterpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        QuantLib::CubicSpline::BoundaryCondition leftCondition,
        double leftConditionValue,
        QuantLib::CubicSpline::BoundaryCondition rightCondition,
        double rightConditionValue,
        bool monotonicityConstraint,
        bool permanent)
    : Interpolation(properties, x, y, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CubicSpline(x_.begin(), x_.end(), y_.begin(),
                                  leftCondition, leftConditionValue,
                                  rightCondition, rightConditionValue,
                                  monotonicityConstraint));
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    SABRInterpolation::SABRInterpolation(
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
        bool permanent)
    : Interpolation(properties, x, y, permanent), forward_(forward) {

        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SABRInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        t, forward_, alpha, beta, nu, rho,
                                        isAlphaFixed, isBetaFixed,
                                        isNuFixed, isRhoFixed, vegaWeighted,
                                        ec, om));
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }
   
}

