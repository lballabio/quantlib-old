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

/*! \file gsr.hpp
    \brief GSR 1 factor model
*/

#ifndef quantlib_gsr_hpp
#define quantlib_gsr_hpp

#include <ql/time/schedule.hpp>
#include <ql/math/integrals/simpsonintegral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

#include <ql/experimental/models/gaussian1dmodel.hpp>
#include <ql/experimental/models/gsrprocess.hpp>

#include <boost/math/special_functions.hpp>

namespace QuantLib {

//! One factor gsr model, formulation is in forward measure

class Gsr : public Gaussian1dModel, public CalibratedModel {

  public:
    // constant mean reversion
    Gsr(const Handle<YieldTermStructure> &termStructure,
        const std::vector<Date> &volstepdates,
        const std::vector<Real> &volatilities, const Real reversion,
        const Real T = 60.0,
        const std::vector<Real> &adjusters = std::vector<Real>());
    // piecewise mean reversion (with same step dates as volatilities)
    Gsr(const Handle<YieldTermStructure> &termStructure,
        const std::vector<Date> &volstepdates,
        const std::vector<Real> &volatilities,
        const std::vector<Real> &reversions, const Real T = 60.0,
        const std::vector<Real> &adjusters = std::vector<Real>());
    // constant mean reversion with floating model data
    Gsr(const Handle<YieldTermStructure> &termStructure,
        const std::vector<Date> &volstepdates,
        const std::vector<Handle<Quote> > &volatilities,
        const Handle<Quote> reversion, const Real T = 60.0,
        const std::vector<Handle<Quote> > &adjusters =
            std::vector<Handle<Quote> >());
    // piecewise mean reversion with floating model data
    Gsr(const Handle<YieldTermStructure> &termStructure,
        const std::vector<Date> &volstepdates,
        const std::vector<Handle<Quote> > &volatilities,
        const std::vector<Handle<Quote> > &reversions, const Real T = 60.0,
        const std::vector<Handle<Quote> > &adjusters =
            std::vector<Handle<Quote> >());

    const Real numeraireTime() const;
    const void numeraireTime(const Real T);

    const Array &reversion() const { return reversion_.params(); }
    const Array &volatility() const { return sigma_.params(); }

    // calibration constraints

    Disposable<std::vector<bool> > FixedReversions() {
        std::vector<bool> res(reversions_.size(), true);
        std::vector<bool> vol(volatilities_.size(), false);
        res.insert(res.end(), vol.begin(), vol.end());
        return res;
    }

    Disposable<std::vector<bool> > FixedVolatilities() {
        std::vector<bool> res(reversions_.size(), false);
        std::vector<bool> vol(volatilities_.size(), true);
        res.insert(res.end(), vol.begin(), vol.end());
        return res;
    }

    Disposable<std::vector<bool> > MoveVolatility(Size i) {
        QL_REQUIRE(i < volatilities_.size(),
                   "volatility with index " << i << " does not exist (0..."
                                            << volatilities_.size() - 1 << ")");
        std::vector<bool> res(reversions_.size() + volatilities_.size(), true);
        res[reversions_.size() + i] = false;
        return res;
    }

    Disposable<std::vector<bool> > MoveReversion(Size i) {
        QL_REQUIRE(i < reversions_.size(),
                   "reversion with index " << i << " does not exist (0..."
                                           << reversions_.size() - 1 << ")");
        std::vector<bool> res(reversions_.size() + volatilities_.size(), true);
        res[i] = false;
        return res;
    }

    // With fixed reversion calibrate the volatilities one by one
    // to the given helpers. It is assumed that that volatility step
    // dates are suitable for this, i.e. they should be identical to
    // the fixing dates of the helpers (except for the last one where
    // we do not need a step). Also note that the endcritera reflect
    // only the status of the last calibration when using this method.
    void calibrateVolatilitiesIterative(
        const std::vector<boost::shared_ptr<CalibrationHelper> > &helpers,
        OptimizationMethod &method, const EndCriteria &endCriteria,
        const Constraint &constraint = Constraint(),
        const std::vector<Real> &weights = std::vector<Real>()) {

        for (Size i = 0; i < helpers.size(); i++) {
            std::vector<boost::shared_ptr<CalibrationHelper> > h(1, helpers[i]);
            calibrate(h, method, endCriteria, constraint, weights,
                      MoveVolatility(i));
        }
    }

    // With fixed volatility calibrate the reversions one by one
    // to the given helpers. In this case the step dates must be chosen
    // according to the maturities of the calibration instruments.
    void calibrateReversionsIterative(
        const std::vector<boost::shared_ptr<CalibrationHelper> > &helpers,
        OptimizationMethod &method, const EndCriteria &endCriteria,
        const Constraint &constraint = Constraint(),
        const std::vector<Real> &weights = std::vector<Real>()) {

        for (Size i = 0; i < helpers.size(); i++) {
            std::vector<boost::shared_ptr<CalibrationHelper> > h(1, helpers[i]);
            calibrate(h, method, endCriteria, constraint, weights,
                      MoveReversion(i));
        }
    }

  protected:
    const Real numeraireImpl(const Time t, const Real y,
                             const Handle<YieldTermStructure> &yts) const;

    const Real zerobondImpl(const Time T, const Time t, const Real y,
                            const Handle<YieldTermStructure> &yts) const;

    void generateArguments() {
        boost::static_pointer_cast<GsrProcess>(stateProcess_)->flushCache();
        notifyObservers();
    }

    void update() { LazyObject::update(); }

    void performCalculations() const {
        Gaussian1dModel::performCalculations();
        updateTimes();
        updateState();
    }

  private:
    void updateTimes() const;
    void updateState() const;
    void initialize(Real);

    Parameter &reversion_, &sigma_, &adjuster_;

    std::vector<Handle<Quote> > volatilities_;
    std::vector<Handle<Quote> > reversions_;
    std::vector<Handle<Quote> > adjusters_;
    std::vector<Date> volstepdates_; // this is shared between vols,
                                     // adjusters and reverisons in
                                     // case of piecewise reversions
    mutable std::vector<Time> volsteptimes_;
    mutable Array volsteptimesArray_; // FIXME this is redundant (just a copy of
                                      // volsteptimes_)
};

inline const Real Gsr::numeraireTime() const {
    return boost::dynamic_pointer_cast<GsrProcess>(stateProcess_)
        ->getForwardMeasureTime();
}

inline const void Gsr::numeraireTime(const Real T) {
    boost::dynamic_pointer_cast<GsrProcess>(stateProcess_)
        ->setForwardMeasureTime(T);
}
}

#endif
