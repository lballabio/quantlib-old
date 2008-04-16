/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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
#include <qlo/enumerations/factories/interpolationsfactory.hpp>

#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>
#include <ql/math/interpolations/abcdinterpolation.hpp>

using std::pair;
using std::vector;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using ObjectHandler::ValueObject;
using ObjectHandler::Create;
using QuantLib::Real;
using QuantLib::Size;
using QuantLib::EndCriteria;
using QuantLib::OptimizationMethod;

namespace QuantLibAddin {

    Interpolation::Interpolation(const shared_ptr<ValueObject>& prop,
                                 const vector<Real>& x,
                                 const vector<Real>& y,
                                 bool permanent)
    : Extrapolator(prop, permanent)
    {
        QL_REQUIRE(!x.empty(), "empty x vector");
        Size n = x.size();
        QL_REQUIRE(n==y.size(),
                   "unmatched size between x (" << n << ") and y(" <<
                   y.size() << ")");
        vector<pair<Real, Real> > pairs(n);
        for (Size i=0; i<n; ++i)
            pairs[i] = std::make_pair<Real, Real>(x[i], y[i]);
        std::sort(pairs.begin(), pairs.end());
        vector<pair<Real, Real> >::iterator end = std::unique(pairs.begin(),
                                                              pairs.end());
        x_.push_back(pairs[0].first);
        y_.push_back(pairs[0].second);
        vector<pair<Real, Real> >::iterator j;
        for (j=pairs.begin()+1; j<end; ++j) {
            if (x_.back() == j->first) {
                if (y_.back() == j->second) continue;
                QL_FAIL("duplicated x value: " << j->first <<
                        " with different y values: " << y_.back() <<
                        ", " << j->second);
            }
            x_.push_back(j->first);
            y_.push_back(j->second);
        }
    }

    GenericInterp::GenericInterp(const shared_ptr<ValueObject>& p,
                                 const std::string& type,
                                 const vector<Real>& x,
                                 const vector<Real>& y,
                                 bool permanent)
    : Interpolation(p, x, y, permanent)
    {
        libraryObject_ = Create<shared_ptr<QuantLib::Interpolation> >()
            (type, x_.begin(), x_.end(), y_.begin());
        dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    CubicInterpolation::CubicInterpolation(
        const shared_ptr<ValueObject>& properties,
        const vector<Real>& x,
        const vector<Real>& y,
        QuantLib::CubicInterpolation::DerivativeApprox da,
        bool monotonic,
        QuantLib::CubicInterpolation::BoundaryCondition leftCondition,
        Real leftValue,
        QuantLib::CubicInterpolation::BoundaryCondition rightCondition,
        Real rightValue,
        bool permanent)
    : Interpolation(properties, x, y, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CubicInterpolation(x_.begin(), x_.end(),
                                         y_.begin(),
                                         da, monotonic,
                                         leftCondition, leftValue,
                                         rightCondition, rightValue));
        dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    AbcdInterpolation::AbcdInterpolation(
            const shared_ptr<ValueObject>& properties,
            const vector<Real>& x,
            const vector<Real>& y,
            Real a,
            Real b,
            Real c,
            Real d,
            bool aIsFixed,
            bool bIsFixed,
            bool cIsFixed,
            bool dIsFixed,
            bool vegaWeighted,
            const shared_ptr<QuantLib::EndCriteria>& ec,
            const shared_ptr<QuantLib::OptimizationMethod>& om,
            bool permanent)
    : Interpolation(properties, x, y, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::AbcdInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        a, b, c, d,
                                        aIsFixed, bIsFixed, cIsFixed, dIsFixed,
                                        vegaWeighted,
                                        ec, om));
        dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

    SABRInterpolation::SABRInterpolation(
                                    const shared_ptr<ValueObject>& p,
                                    const vector<Real>& x,
                                    const vector<Real>& y,
                                    QuantLib::Time t,
                                    QuantLib::Rate forward,
                                    Real alpha,
                                    Real beta,
                                    Real nu,
                                    Real rho,
                                    bool isAlphaFixed,
                                    bool isBetaFixed,
                                    bool isNuFixed,
                                    bool isRhoFixed,
                                    bool vegaWeighted,
                                    const shared_ptr<EndCriteria>& ec,
                                    const shared_ptr<OptimizationMethod>& om,
                                    bool permanent)
    : Interpolation(p, x, y, permanent), forward_(forward)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SABRInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        t, forward_, alpha, beta, nu, rho,
                                        isAlphaFixed, isBetaFixed,
                                        isNuFixed, isRhoFixed,
                                        vegaWeighted,
                                        ec, om));
        dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_)->update();
    }

}
