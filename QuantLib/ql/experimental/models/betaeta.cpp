/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers, Roland Lichters

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

#include <ql/experimental/models/betaeta.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/time/schedule.hpp>

#include <boost/make_shared.hpp>

namespace QuantLib {

BetaEta::BetaEta(const Handle<YieldTermStructure> &termStructure,
                 const std::vector<Date> &volstepdates,
                 const std::vector<Real> &volatilities, const Real reversion,
                 const Real beta, const Real eta)
    : TermStructureConsistentModel(termStructure), CalibratedModel(4),
      reversion_(arguments_[0]), sigma_(arguments_[1]), pBeta_(arguments_[2]),
      pEta_(arguments_[3]), volstepdates_(volstepdates) {
    QL_REQUIRE(!termStructure.empty(),
               "no yield term structure given (empty handle)");
    volatilities_.resize(volatilities.size());
    for (Size i = 0; i < volatilities.size(); ++i)
        volatilities_[i] =
            Handle<Quote>(boost::make_shared<SimpleQuote>(volatilities[i]));
    reversions_.resize(1);
    reversions_[0] = Handle<Quote>(boost::make_shared<SimpleQuote>(reversion));
    beta_ = Handle<Quote>(boost::make_shared<SimpleQuote>(beta));
    eta_ = Handle<Quote>(boost::make_shared<SimpleQuote>(eta));
    initialize();
}

BetaEta::BetaEta(const Handle<YieldTermStructure> &termStructure,
                 const std::vector<Date> &volstepdates,
                 const std::vector<Real> &volatilities,
                 const std::vector<Real> &reversions, const Real beta,
                 const Real eta)
    : TermStructureConsistentModel(termStructure), CalibratedModel(4),
      reversion_(arguments_[0]), sigma_(arguments_[1]), pBeta_(arguments_[2]),
      pEta_(arguments_[3]), volstepdates_(volstepdates) {
    QL_REQUIRE(!termStructure.empty(),
               "no yield term structure given (empty handle)");
    volatilities_.resize(volatilities.size());
    for (Size i = 0; i < volatilities.size(); ++i)
        volatilities_[i] =
            Handle<Quote>(boost::make_shared<SimpleQuote>(volatilities[i]));
    reversions_.resize(reversions_.size());
    for (Size i = 0; i < reversions_.size(); ++i)
        reversions_[i] =
            Handle<Quote>(boost::make_shared<SimpleQuote>(volatilities[i]));
    beta_ = Handle<Quote>(boost::make_shared<SimpleQuote>(beta));
    eta_ = Handle<Quote>(boost::make_shared<SimpleQuote>(eta));
    initialize();
}

BetaEta::BetaEta(const Handle<YieldTermStructure> &termStructure,
                 const std::vector<Date> &volstepdates,
                 const std::vector<Handle<Quote> > &volatilities,
                 const Handle<Quote> reversion, const Handle<Quote> beta,
                 const Handle<Quote> eta)
    : TermStructureConsistentModel(termStructure), CalibratedModel(4),
      reversion_(arguments_[0]), sigma_(arguments_[1]), pBeta_(arguments_[2]),
      pEta_(arguments_[3]), volatilities_(volatilities),
      reversions_(std::vector<Handle<Quote> >(1, reversion)),
      volstepdates_(volstepdates) {

    QL_REQUIRE(!termStructure.empty(),
               "no yield term structure given (empty handle)");
    initialize();
}

BetaEta::BetaEta(const Handle<YieldTermStructure> &termStructure,
                 const std::vector<Date> &volstepdates,
                 const std::vector<Handle<Quote> > &volatilities,
                 const std::vector<Handle<Quote> > &reversions,
                 const Handle<Quote> beta, const Handle<Quote> eta)
    : TermStructureConsistentModel(termStructure), CalibratedModel(4),
      reversion_(arguments_[0]), sigma_(arguments_[1]), pBeta_(arguments_[2]),
      pEta_(arguments_[3]), volatilities_(volatilities),
      reversions_(reversions), volstepdates_(volstepdates) {

    QL_REQUIRE(!termStructure.empty(),
               "no yield term structure given (empty handle)");
    initialize();
}

void BetaEta::updateTimes() const {
    volsteptimes_.clear();
    int j = 0;
    for (std::vector<Date>::const_iterator i = volstepdates_.begin();
         i != volstepdates_.end(); ++i, ++j) {
        volsteptimes_.push_back(termStructure()->timeFromReference(*i));
        volsteptimesArray_[j] = volsteptimes_[j];
        if (j == 0)
            QL_REQUIRE(volsteptimes_[0] > 0.0, "volsteptimes must be positive ("
                                                   << volsteptimes_[0] << ")");
        else
            QL_REQUIRE(volsteptimes_[j] > volsteptimes_[j - 1],
                       "volsteptimes must be strictly increasing ("
                           << volsteptimes_[j - 1] << "@" << (j - 1) << ", "
                           << volsteptimes_[j] << "@" << j << ")");
    }
}

void BetaEta::updateState() const {
    for (Size i = 0; i < reversion_.size(); i++) {
        reversion_.setParam(i, reversions_[i]->value());
    }
    for (Size i = 0; i < sigma_.size(); i++) {
        sigma_.setParam(i, volatilities_[i]->value());
    }
    pBeta_.setParam(0, beta_->value());
    pEta_.setParam(0, eta_->value());
    betaLink_ = beta_->value();
    etaLink_ = eta_->value();
}

void BetaEta::initialize() {
    volsteptimesArray_ = Array(volstepdates_.size());
    updateTimes();
    QL_REQUIRE(volatilities_.size() == volsteptimes_.size() + 1,
               "there must be n+1 volatilities ("
                   << volatilities_.size() << ") for n volatility step times ("
                   << volsteptimes_.size() << ")");
    sigma_ = PiecewiseConstantParameter(volsteptimes_, NoConstraint());

    QL_REQUIRE(reversions_.size() == 1 ||
                   reversions_.size() == volsteptimes_.size() + 1,
               "there must be 1 or n+1 reversions ("
                   << reversions_.size() << ") for n volatility step times ("
                   << volsteptimes_.size() << ")");
    if (reversions_.size() == 1) {
        reversion_ = ConstantParameter(reversions_[0]->value(), NoConstraint());
    } else {
        reversion_ = PiecewiseConstantParameter(volsteptimes_, NoConstraint());
    }

    pBeta_ = ConstantParameter(beta_->value(), NoConstraint());
    pEta_ = ConstantParameter(eta_->value(), NoConstraint());
    betaLink_ = beta_->value();
    etaLink_ = eta_->value();

    for (Size i = 0; i < sigma_.size(); i++) {
        sigma_.setParam(i, volatilities_[i]->value());
    }
    for (Size i = 0; i < reversion_.size(); i++) {
        reversion_.setParam(i, reversions_[i]->value());
    }

    registerWith(termStructure());

    for (Size i = 0; i < reversions_.size(); ++i)
        registerWith(reversions_[i]);
    for (Size i = 0; i < volatilities_.size(); ++i)
        registerWith(volatilities_[i]);

    registerWith(beta_);
    registerWith(eta_);

    core_ = boost::make_shared<BetaEtaCore>(volsteptimesArray_, sigma_.params(),
                                            reversion_.params(), betaLink_,
                                            etaLink_);
}

const Real BetaEta::numeraire(const Time t, const Real x,
                              const Handle<YieldTermStructure> &yts) const {
    Real d =
        yts.empty() ? this->termStructure()->discount(t) : yts->discount(t);
    return d * std::exp(core_->lambda(t) * x + core_->M(0, 0, t));
}

const Real BetaEta::zerobond(const Time T, const Time t, const Real x,
                             const Handle<YieldTermStructure> &yts) const {

    Real d = yts.empty()
                 ? this->termStructure()->discount(T) /
                       this->termStructure()->discount(t)
                 : yts->discount(T) / yts->discount(t);

    return d * std::exp(-(core_->lambda(T) - core_->lambda(t)) * x -
                        (core_->M(0, 0, T) - core_->M(0, 0, t)) +
                        core_->M(t, x, T));
}

const Real BetaEta::forwardRate(const Date &fixing, const Date &referenceDate,
                                const Real x,
                                boost::shared_ptr<IborIndex> iborIdx) const {

    QL_REQUIRE(iborIdx != NULL, "no ibor index given");

    calculate();

    if (fixing <= (evaluationDate_ + (enforcesTodaysHistoricFixings_ ? 0 : -1)))
        return iborIdx->fixing(fixing);

    Handle<YieldTermStructure> yts =
        iborIdx->forwardingTermStructure(); // might be empty, then use
                                            // model curve

    Date valueDate = iborIdx->valueDate(fixing);
    Date endDate = iborIdx->fixingCalendar().advance(
        valueDate, iborIdx->tenor(), iborIdx->businessDayConvention(),
        iborIdx->endOfMonth());
    // FIXME Here we should use the calculation date calendar ?
    Real dcf = iborIdx->dayCounter().yearFraction(valueDate, endDate);

    return (zerobond(valueDate, referenceDate, x, yts) -
            zerobond(endDate, referenceDate, x, yts)) /
           (dcf * zerobond(endDate, referenceDate, x, yts));
}

const Real BetaEta::swapRate(const Date &fixing, const Period &tenor,
                             const Date &referenceDate, const Real x,
                             boost::shared_ptr<SwapIndex> swapIdx) const {

    QL_REQUIRE(swapIdx != NULL, "no swap index given");

    calculate();

    if (fixing <= (evaluationDate_ + (enforcesTodaysHistoricFixings_ ? 0 : -1)))
        return swapIdx->fixing(fixing);

    Handle<YieldTermStructure> ytsf =
        swapIdx->iborIndex()->forwardingTermStructure();
    Handle<YieldTermStructure> ytsd =
        swapIdx->discountingTermStructure(); // either might be empty, then
                                             // use model curve

    Schedule sched, floatSched;

    boost::shared_ptr<VanillaSwap> underlying =
        underlyingSwap(swapIdx, fixing, tenor);

    sched = underlying->fixedSchedule();

    boost::shared_ptr<OvernightIndexedSwapIndex> oisIdx =
        boost::dynamic_pointer_cast<OvernightIndexedSwapIndex>(swapIdx);
    if (oisIdx != NULL) {
        floatSched = sched;
    } else {
        floatSched = underlying->floatingSchedule();
    }

    // should be fine for overnightindexed swap indices as well
    Real annuity = swapAnnuity(fixing, tenor, referenceDate, x, swapIdx);
    Rate floatleg = 0.0;
    if (ytsf.empty() && ytsd.empty()) { // simple 100-formula can be used
                                        // only in one curve setup
        floatleg =
            (zerobond(sched.dates().front(), referenceDate, x,
                      Handle<YieldTermStructure>()) -
             zerobond(sched.calendar().adjust(sched.dates().back(),
                                              underlying->paymentConvention()),
                      referenceDate, x, Handle<YieldTermStructure>()));
    } else {
        for (Size i = 1; i < floatSched.size(); i++) {
            floatleg +=
                (zerobond(floatSched[i - 1], referenceDate, x, ytsf) /
                     zerobond(floatSched[i], referenceDate, x, ytsf) -
                 1.0) *
                zerobond(floatSched.calendar().adjust(
                             floatSched[i], underlying->paymentConvention()),
                         referenceDate, x, ytsd);
        }
    }
    return floatleg / annuity;
}

const Real BetaEta::swapAnnuity(const Date &fixing, const Period &tenor,
                                const Date &referenceDate, const Real x,
                                boost::shared_ptr<SwapIndex> swapIdx) const {

    QL_REQUIRE(swapIdx != NULL, "no swap index given");

    calculate();

    Handle<YieldTermStructure> ytsd =
        swapIdx->discountingTermStructure(); // might be empty, then use
                                             // model curve

    boost::shared_ptr<VanillaSwap> underlying =
        underlyingSwap(swapIdx, fixing, tenor);

    Schedule sched = underlying->fixedSchedule();

    Real annuity = 0.0;
    for (unsigned int j = 1; j < sched.size(); j++) {
        annuity += zerobond(sched.calendar().adjust(
                                sched.date(j), underlying->paymentConvention()),
                            referenceDate, x, ytsd) *
                   swapIdx->dayCounter().yearFraction(sched.date(j - 1),
                                                      sched.date(j));
    }
    return annuity;
}

const Disposable<Array> BetaEta::xGrid(const Real stdDevs, const int gridPoints,
                                       const Real T, const Real t,
                                       const Real x) const {

    Array result(2 * gridPoints + 1, 0.0);

    // approximate standard deviation for x
    Real s = std::sqrt(core_->tau(t, T));

    Real h = stdDevs * s / (static_cast<Real>(gridPoints));

    for (int j = -gridPoints; j <= gridPoints; ++j) {
        result[j + gridPoints] = x + h * (static_cast<Real>(j));
    }

    return result;
}

} // namespace QuantLib
