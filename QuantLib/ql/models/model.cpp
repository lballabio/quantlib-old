/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2013, 2015 Peter Caspers
 Copyright (C) 2015 Ferdinando Ametrano

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

#include <ql/models/model.hpp>
#include <ql/math/optimization/problem.hpp>
#include <ql/math/optimization/projection.hpp>
#include <ql/math/optimization/projectedconstraint.hpp>
#include <algorithm>

using std::vector;
using boost::shared_ptr;
using boost::static_pointer_cast;

namespace QuantLib {

    namespace {
        void no_deletion(CalibratedModel*) {}
    }

    CalibratedModel::CalibratedModel(Size nArguments)
    : arguments_(nArguments),
      constraint_(new PrivateConstraint(arguments_)),
      endCriteria_(EndCriteria::None) {}

    class CalibratedModel::CalibrationFunction : public CostFunction {
      public:
        CalibrationFunction(
                          CalibratedModel* model,
                          const vector<shared_ptr<CalibrationHelperBase> >& h,
                          const vector<Real>& weights,
                          const Projection& projection)
        : model_(model, no_deletion), helpers_(h),
          weights_(weights), projection_(projection) {}

        virtual ~CalibrationFunction() {}

        virtual Real value(const Array& params) const {
            model_->setParams(projection_.include(params));
            Real value=0.0;
            for (Size i=0; i<helpers_.size(); ++i) {
                Real diff = helpers_[i]->calibrationError();
                value += diff*diff*weights_[i];
            }
            return std::sqrt(value);
        }

        virtual Disposable<Array> values(const Array& params) const {
            model_->setParams(projection_.include(params));
            Array values(helpers_.size());
            for (Size i=0; i<helpers_.size(); ++i) {
                values[i] = helpers_[i]->calibrationError() *
                                                        std::sqrt(weights_[i]);
            }
            return values;
        }

        virtual Real finiteDifferenceEpsilon() const { return 1e-6; }

      private:
        shared_ptr<CalibratedModel> model_;
        const vector<shared_ptr<CalibrationHelperBase> >& helpers_;
        const vector<Real>& weights_;
        const Projection& projection_;
    };

    void CalibratedModel::calibrate(
                            const vector<shared_ptr<CalibrationHelper> >& h,
                            OptimizationMethod& method,
                            const EndCriteria& endCriteria,
                            const Constraint& additionalConstraint,
                            const vector<Real>& weights,
                            const vector<bool>& fixParameters) {
        vector<shared_ptr<CalibrationHelperBase> > tmp(h.size());
        for (Size i=0; i<h.size(); ++i)
            tmp[i] = static_pointer_cast<CalibrationHelperBase>(h[i]);
        calibrate(tmp, method, endCriteria, additionalConstraint, weights,
                  fixParameters);
    }

    void CalibratedModel::calibrate(
                    const vector<shared_ptr<CalibrationHelperBase> >& h,
                    OptimizationMethod& method,
                    const EndCriteria& endCriteria,
                    const Constraint& additionalConstraint,
                    const vector<Real>& w,
                    const vector<bool>& fixParameters) {

        QL_REQUIRE(!h.empty(), "no helpers provided");

        Array prms = params();
        if (fixParameters.empty()) {
            fixedParameters_.resize(prms.size());
            std::fill(fixedParameters_.begin(), fixedParameters_.end(), false);
        } else {
            QL_REQUIRE(fixParameters.size() == prms.size(),
                       "mismatch between number of parametrs (" <<
                       h.size() << ") and fixed parameters booleans (" <<
                       fixParameters.size() << ")");
            fixedParameters_ = fixParameters;
        }
        Projection proj(prms, fixedParameters_);

        if (w.empty()) {
            weights_.resize(h.size());
            std::fill(weights_.begin(), weights_.end(), 1.0);
        } else {
            QL_REQUIRE(w.size() == h.size(),
                       "mismatch between number of helpers (" <<
                       h.size() << ") and weights (" << w.size() << ")");
            weights_ = w;
        }
        CalibrationFunction f(this, h, weights_, proj);

        Constraint c;
        if (additionalConstraint.empty())
            c = *constraint_;
        else
            c = CompositeConstraint(*constraint_, additionalConstraint);
        ProjectedConstraint pc(c, proj);

        Problem prob(f, pc, proj.project(prms));
        endCriteria_ = method.minimize(prob, endCriteria);
        setParams(proj.include(prob.currentValue()));

        notifyObservers();
    }

    Real CalibratedModel::value(
                            const Array& params,
                            const vector<shared_ptr<CalibrationHelper> >& h) {
        vector<shared_ptr<CalibrationHelperBase> > helpers(h.size());
        for (Size i=0; i<h.size(); ++i)
            helpers[i] = static_pointer_cast<CalibrationHelperBase>(h[i]);
        return value(params, helpers);
    }

    Real CalibratedModel::value(
                    const Array& params,
                    const vector<shared_ptr<CalibrationHelperBase> >& h) {
        vector<Real> w = vector<Real>(h.size(), 1.0);
        Projection p(params);
        CalibrationFunction f(this, h, w, p);
        return f.value(params);
    }

    Disposable<Array> CalibratedModel::params() const {
        Size size=0;
        for (Size i=0; i<arguments_.size(); ++i)
            size += arguments_[i].size();
        Array params(size);
        for (Size i=0, k=0; i<arguments_.size(); ++i) {
            for (Size j=0; j<arguments_[i].size(); ++j, ++k)
                params[k] = arguments_[i].params()[j];
        }
        return params;
    }

    void CalibratedModel::setParams(const Array& params) {
        Array::const_iterator p = params.begin();
        for (Size i=0; i<arguments_.size(); ++i) {
            for (Size j=0; j<arguments_[i].size(); ++j, ++p) {
                QL_REQUIRE(p!=params.end(),"parameter array too small");
                arguments_[i].setParam(j, *p);
            }
        }
        QL_REQUIRE(p==params.end(),"parameter array too big!");
        generateArguments();
        notifyObservers();
    }

}
