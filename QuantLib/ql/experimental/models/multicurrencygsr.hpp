/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file multicurrencygsr.hpp
    \brief multicurrency gsr model with Garman Kohlhagen fx processes
    !!! this class does not work !!!
*/

#ifndef quantlib_multicurrency_gsr_hpp
#define quantlib_multicurrency_gsr_hpp

#include <ql/models/shortrate/onefactormodels/gsr.hpp>

namespace QuantLib {

/*! model parameters considered on this level are the stepwise
    fx volatilities only */

class MultiCurrencyGsr : public CalibratedModel {

  public:
    // fixed fx spots (understood as fx spots as of today), fx volatilities
    MultiCurrencyGsr(
        const std::vector<boost::shared_ptr<Gsr> > &currencyModels,
        const std::vector<Real> &fxSpots,
        const std::vector<Date> &fxVolStepDates,
        const std::vector<std::vector<Real> > &fxVolatilities,
        const Matrix &correlation,
        const std::vector<Handle<YieldTermStructure> > &fxDiscountCurves =
            std::vector<Handle<YieldTermStructure> >());

    const boost::shared_ptr<Gsr> currencyModel(Size i);

    // calibration constraints
    Disposable<std::vector<bool> > MoveCurrencyFXVolatility(Size i);

    // calibrate the stepwise fx volatilities dom - currency(i)
    void calibrateFxVolatilitiesIterative(
        Size i,
        const std::vector<boost::shared_ptr<CalibrationHelper> > &helpers,
        OptimizationMethod &method, const EndCriteria &endCriteria,
        const Constraint &constraint = Constraint(),
        const std::vector<Real> &weights = std::vector<Real>());

    boost::shared_ptr<StochasticProcess> stateProcess() const {
        return process_;
    }

  protected:
    void generateArguments() { notifyObservers(); }
    void update() {
        updateTimes();
        notifyObservers();
    }

  private:
    void updateTimes() const;

    std::vector<Parameter> &fxVols_;
    const std::vector<boost::shared_ptr<Gsr> > currencyModels_;
    const std::vector<Real> fxSpots_;
    const std::vector<Date> fxVolStepDates_;
    const std::vector<std::vector<Real> > fxVolatilities_;
    std::vector<Handle<YieldTermStructure> > fxDiscountCurves_;

    std::vector<boost::shared_ptr<StochasticProcess1D> > processes_;
    boost::shared_ptr<StochasticProcess> process_;

    mutable std::vector<Time> fxVolStepTimes_;
    mutable Array fxVolStepTimesArray_;

    Size n_;
};

} // namespace QuantLib

#endif
