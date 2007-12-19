
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/interpolation.hpp>
#include <qlo/Enumerations/Factories/interpolationsfactory.hpp>

#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/cubicspline.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>
#include <ql/math/interpolations/abcdinterpolation.hpp>

using std::pair;
using std::vector;
using QuantLib::Real;
using QuantLib::Size;

namespace QuantLibAddin {

    Interpolation::Interpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const vector<Real>& x,
        const vector<Real>& y,
        bool permanent)
    : Extrapolator(properties, permanent) {
        QL_REQUIRE(!x.empty(), "empty x vector");
        Size n = x.size();
        QL_REQUIRE(n==y.size(),
                  "unmatched size between x (" << n <<
                  ") and y(" << y.size() << ")");
        vector<pair<Real, Real> > pairs(n);
        for (Size i=0; i<n; ++i)
            pairs[i]=std::make_pair<Real, Real>(x[i], y[i]);
        std::sort(pairs.begin(), pairs.end());
        vector<pair<Real, Real> >::iterator end = std::unique(pairs.begin(),
                                                              pairs.end());
        x_.push_back(pairs[0].first);
        y_.push_back(pairs[0].second);
        vector<pair<Real, Real> >::iterator j;
        for (j=pairs.begin()+1; j<end; ++j) {
            QL_REQUIRE(x_.back()!=j->first,
                       "duplicated x value: " << j->first);
            x_.push_back(j->first);
            y_.push_back(j->second);
        }
    }

    LinearInterpolation::LinearInterpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::string &linearInterpolationType, 
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        bool permanent)
    : Interpolation(properties, x, y, permanent)
    {
        //const std::vector<QuantLib::Real>& x_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("XARRAY"));
        //const std::vector<QuantLib::Real>& y_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("YARRAY"));
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::Interpolation> >()
            (linearInterpolationType, x_.begin(), x_.end(), y_.begin());
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    CubicSplineInterpolation::CubicSplineInterpolation(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Real>& x,
        const std::vector<QuantLib::Real>& y,
        QuantLib::CubicSplineInterpolation::BoundaryCondition leftCondition,
        QuantLib::Real leftConditionValue,
        QuantLib::CubicSplineInterpolation::BoundaryCondition rightCondition,
        QuantLib::Real rightConditionValue,
        bool monotonicityConstraint,
        bool permanent)
    : Interpolation(properties, x, y, permanent)
    {
        //const std::vector<QuantLib::Real>& x_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("XARRAY"));
        //const std::vector<QuantLib::Real>& y_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("YARRAY"));
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CubicSplineInterpolation(x_.begin(), x_.end(), y_.begin(),
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
    : Interpolation(properties, x, y, permanent), forward_(forward)
    {
        //const std::vector<QuantLib::Real>& x_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("XARRAY"));
        //const std::vector<QuantLib::Real>& y_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("YARRAY"));
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SABRInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        t, forward_, alpha, beta, nu, rho,
                                        isAlphaFixed, isBetaFixed,
                                        isNuFixed, isRhoFixed,
                                        vegaWeighted,
                                        ec, om));
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    AbcdInterpolation::AbcdInterpolation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& x,
            const std::vector<QuantLib::Real>& y,
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
            bool permanent)
    : Interpolation(properties, x, y, permanent)
    {
        //const std::vector<QuantLib::Real>& x_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("XARRAY"));
        //const std::vector<QuantLib::Real>& y_ =
        //    boost::any_cast<std::vector<QuantLib::Real> >(propertyValue("YARRAY"));
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::AbcdInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        a, b, c, d,
                                        aIsFixed, bIsFixed, cIsFixed, dIsFixed,
                                        vegaWeighted,
                                        ec, om));
        boost::dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

}
