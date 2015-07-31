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

#include <ql/experimental/models/multicurrencygsr.hpp>
#include <ql/experimental/models/multicurrencygsrprocess.hpp>
#include <ql/experimental/models/gsrprocess_riskneutral.hpp>
#include <ql/termstructures/volatility/equityfx/blackvariancecurve.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/quotes/simplequote.hpp>

#include <boost/make_shared.hpp>

namespace QuantLib {

MultiCurrencyGsr::MultiCurrencyGsr(
    const std::vector<boost::shared_ptr<Gsr> > &currencyModels,
    const std::vector<Real> &fxSpots, const std::vector<Date> &fxVolStepDates,
    const std::vector<std::vector<Real> > &fxVolatilities,
    const Matrix &correlation,
    const std::vector<Handle<YieldTermStructure> > &fxDiscountCurves)
    : CalibratedModel(fxSpots.size()), fxVols_(arguments_),
      currencyModels_(currencyModels), fxSpots_(fxSpots),
      fxVolStepDates_(fxVolStepDates), fxVolatilities_(fxVolatilities),
      fxDiscountCurves_(fxDiscountCurves) {

    n_ = fxSpots.size();

    // consistency checks

    QL_REQUIRE(currencyModels_.size() == n_ + 1,
               "for n (" << n_
                         << ") fx spots there must be n+1 currency models ("
                         << currencyModels_.size());

    QL_REQUIRE(fxVolatilities_.size() == n_,
               "inconsistent number of fx spots ("
                   << n_ << ") and fx volatilities (" << fxVolatilities.size()
                   << ") given");

    for (Size i = 0; i < n_; ++i) {
        QL_REQUIRE(fxVolatilities_[i].size() == fxVolStepDates.size() + 1,
                   "for " << fxVolStepDates.size()
                          << " fx vol step dates there must be "
                          << fxVolStepDates.size() + 1
                          << " volatilities, but for currency #" << i
                          << " there are " << fxVolatilities_[i].size());
    }

    // default curves are the model curves

    if (fxDiscountCurves_.size() == 0) {
        for (Size i = 0; i < n_ + 1; ++i) {
            fxDiscountCurves_.push_back(currencyModels_[i]->termStructure());
        }
    }

    QL_REQUIRE(fxDiscountCurves_.size() == n_ + 1,
               "number of currency models ("
                   << n_ + 1
                   << ") is inconsistent with number of fx discount curves ("
                   << fxDiscountCurves_.size() << ")");

    QL_REQUIRE(correlation.rows() == correlation.columns() &&
                   correlation.rows() == 2 * n_ + 1,
               "correlation matrix (" << correlation.rows() << "x"
                                      << correlation.columns() << ") must be "
                                      << 2 * n_ + 1 << " x " << 2 * n_ + 1);

    // TODO check that all currency models have the same reference date (and day
    // counter), check that the fx discount curves have this reference date too

    // link the model parameters
    for (Size i = 0; i < n_; ++i) {
        fxVols_[i] =
            PiecewiseConstantParameter(fxVolStepTimes_, PositiveConstraint());
        for (Size j = 0; j < fxVols_.size(); ++j)
            fxVols_[i].setParam(j, fxVolatilities_[i][j]);
    }

    // construct the state process

    fxVolStepTimesArray_ = Array(fxVolStepDates.size());
    updateTimes();

    // fx processes (we have to convert back the given bs model volatilities to
    // a full termstructure unfortunately to use the existing g-k-process.

    for (Size i = 0; i < n_; ++i) {
        Handle<YieldTermStructure> yts =
            currencyModels_[i]->termStructure();
        std::vector<Date> blackVolDates(fxVolStepDates);
        if (fxVolStepDates.size() == 0)
            blackVolDates.push_back(yts->referenceDate() + 365);
        else
            blackVolDates.push_back(fxVolStepDates.back() + 365);
        std::vector<Real> blackVolCurve;
        Real totalVariance = 0.0;
        Real t, t0 = 0.0;
        for (Size j = 0; j < blackVolDates.size(); ++j) {
            t = currencyModels_[i]->termStructure()->timeFromReference(
                blackVolDates[j]);
            totalVariance +=
                (t - t0) * fxVolatilities_[i][j] * fxVolatilities_[i][j];
            blackVolCurve.push_back(std::sqrt(totalVariance / t));
        }
        Handle<BlackVolTermStructure> blackVolTs(
            boost::make_shared<BlackVarianceCurve>(
                currencyModels_[i]->termStructure()->referenceDate(),
                blackVolDates, blackVolCurve,
                currencyModels_[i]->termStructure()->dayCounter(), false));
        blackVolTs->enableExtrapolation();
        boost::shared_ptr<GarmanKohlagenProcess> tmp =
            boost::make_shared<GarmanKohlagenProcess>(
                Handle<Quote>(boost::make_shared<SimpleQuote>(fxSpots[i])),
                fxDiscountCurves_[i + 1], fxDiscountCurves_[0], blackVolTs);
        processes_.push_back(tmp);
    }

    // currency models' processes are converted to risk neutral measure (still
    // foreign at this stage)

    for (Size i = 0; i < n_ + 1; ++i) {
        boost::shared_ptr<GsrProcess> p =
            boost::dynamic_pointer_cast<GsrProcess>(
                currencyModels_[i]->stateProcess());
        processes_.push_back(boost::make_shared<GsrProcessRiskNeutral>(
            p->times(), p->volatility(), p->reversion(), p->adjuster(),
            p->referenceDate(), p->dayCounter()));
    }

    process_ =
        boost::make_shared<MultiCurrencyGsrProcess>(processes_, correlation);

    // register with observables

    for (Size i = 0; i < n_ + 1; ++i) {
        registerWith(currencyModels_[i]);
        registerWith(fxDiscountCurves_[i]);
    }
}

void MultiCurrencyGsr::updateTimes() const {
    fxVolStepTimes_.clear();
    for (Size i = 0; i < fxVolStepDates_.size(); ++i) {
        Time tmp = currencyModels_[0]->termStructure()->timeFromReference(
            fxVolStepDates_[i]);
        fxVolStepTimesArray_[i] = tmp;
        fxVolStepTimes_.push_back(tmp);
    }
}

} // namespace QuantLib
