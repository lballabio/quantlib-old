/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2009 Ferdinando Ametrano

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
#include <ql/quote.hpp>

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
using QuantLib::Handle;
using QuantLib::Quote;

namespace QuantLibAddin {

    namespace {
        class QuoteHandleSorter {
          public:
            bool operator()(const pair<Real, Handle<Quote> >& h1,
                            const pair<Real, Handle<Quote> >& h2) const {
                if (h1.first > h2.first)
                    return false;
                return true;
            }
        };
    }

    Interpolation::Interpolation(const shared_ptr<ValueObject>& prop,
                                 const vector<Real>& x,
                                 const vector<Handle<Quote> >& yh,
                                 bool permanent)
    : Extrapolator(prop, permanent), n_(x.size()), x_(n_), y_(n_), yh_(n_)
    {
        QL_REQUIRE(!x.empty(), "empty x vector");
        QL_REQUIRE(n_==yh.size(),
                   "unmatched size between x (" << n_ << ") and y(" <<
                   yh.size() << ")");
        vector<pair<Real, Handle<Quote> > > pairs(n_);
        for (Size i=0; i<n_; ++i)
            pairs[i] = std::make_pair<Real, Handle<Quote> >(x[i], yh[i]);
        std::sort(pairs.begin(), pairs.end(), QuoteHandleSorter());

        x_[0] = pairs[0].first;
        yh_[0] = pairs[0].second;
        registerWith(yh_[0]);
        vector<pair<Real, Handle<Quote> > >::iterator j = pairs.begin()+1;
        for (Size i=1; i<n_; ++i, ++j) {
            QL_REQUIRE(x_[i-1] != j->first,
                       "duplicated x value: " << j->first);
            x_[i] = j->first;
            yh_[i] = j->second;
            registerWith(yh_[i]);
        }
        // temporary patch
        for (Size i=0; i<n_; ++i) {
            if (yh_[i]->isValid())
                y_[i] = yh_[i]->value();
            else 
                y_[i] = 1.0;
        }
    }

    void Interpolation::performCalculations() const {
        for (Size i=0; i<n_; ++i)
            y_[i] = yh_[i]->value();
        qlInterpolation_->update();
    }

    GenericInterp::GenericInterp(const shared_ptr<ValueObject>& p,
                                 const std::string& type,
                                 const vector<Real>& x,
                                 const vector<Handle<Quote> >& yh,
                                 bool permanent)
    : Interpolation(p, x, yh, permanent)
    {
        libraryObject_ = Create<shared_ptr<QuantLib::Interpolation> >()
            (type, x_.begin(), x_.end(), y_.begin());
        qlInterpolation_ = dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_);
    }

    CubicInterpolation::CubicInterpolation(
        const shared_ptr<ValueObject>& properties,
        const vector<Real>& x,
        const vector<Handle<Quote> >& yh,
        QuantLib::CubicInterpolation::DerivativeApprox da,
        bool monotonic,
        QuantLib::CubicInterpolation::BoundaryCondition leftCondition,
        Real leftValue,
        QuantLib::CubicInterpolation::BoundaryCondition rightCondition,
        Real rightValue,
        bool permanent)
    : Interpolation(properties, x, yh, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CubicInterpolation(x_.begin(), x_.end(),
                                         y_.begin(),
                                         da, monotonic,
                                         leftCondition, leftValue,
                                         rightCondition, rightValue));
        qlInterpolation_ = dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_);
    }

    AbcdInterpolation::AbcdInterpolation(
            const shared_ptr<ValueObject>& properties,
            const vector<Real>& x,
            const vector<Handle<Quote> >& yh,
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
    : Interpolation(properties, x, yh, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::AbcdInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        a, b, c, d,
                                        aIsFixed, bIsFixed, cIsFixed, dIsFixed,
                                        vegaWeighted,
                                        ec, om));
        qlInterpolation_ = dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_);
    }

    SABRInterpolation::SABRInterpolation(
                                    const shared_ptr<ValueObject>& p,
                                    const vector<Real>& x,
                                    const vector<Handle<Quote> >& yh,
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
    : Interpolation(p, x, yh, permanent), forward_(forward)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SABRInterpolation(x_.begin(), x_.end(), y_.begin(),
                                        t, forward_, alpha, beta, nu, rho,
                                        isAlphaFixed, isBetaFixed,
                                        isNuFixed, isRhoFixed,
                                        vegaWeighted,
                                        ec, om));
        qlInterpolation_ = dynamic_pointer_cast<QuantLib::Interpolation>(
            libraryObject_);
    }

}
