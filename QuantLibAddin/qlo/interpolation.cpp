
/*
 Copyright (C) 2006 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/interpolation.hpp>
#include <qlo/Factories/interpolationsfactory.hpp>

#include <ql/math/backwardflatinterpolation.hpp>
#include <ql/math/forwardflatinterpolation.hpp>
#include <ql/math/linearinterpolation.hpp>
#include <ql/math/sabrinterpolation.hpp>

namespace QuantLibAddin {

    Interpolation::Interpolation(
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y)
    : x_(x), y_(y) {
        QL_REQUIRE(x.size()==y.size(), "unmatched x/y");
    }

    LinearInterpolation::LinearInterpolation(
        const std::string &linearInterpolationType, 
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y)
    : Interpolation(x,y) {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::Interpolation> >()
            (linearInterpolationType, x_.begin(), x_.end(), y_.begin());
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    CubicSplineInterpolation::CubicSplineInterpolation(
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        QuantLib::CubicSpline::BoundaryCondition leftCondition,
        double leftConditionValue,
        QuantLib::CubicSpline::BoundaryCondition rightCondition,
        double rightConditionValue,
        bool monotonicityConstraint)
    : Interpolation(x,y) {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CubicSpline(x_.begin(), x_.end(), y_.begin(),
                                  leftCondition, leftConditionValue,
                                  rightCondition, rightConditionValue,
                                  monotonicityConstraint));
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    SABRInterpolation::SABRInterpolation(
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
        const boost::shared_ptr<QuantLib::OptimizationMethod>& om)
    : Interpolation(x,y), forward_(forward) {

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

