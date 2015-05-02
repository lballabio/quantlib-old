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

/*! \file mcfxtarfengine.hpp
    \brief Monte Carlo engine for FX Tarf
*/

#ifndef quantlib_pricingengines_mc_fxtarf_hpp
#define quantlib_pricingengines_mc_fxtarf_hpp

#include <ql/event.hpp>

#include <ql/pricingengines/mcsimulation.hpp>

#include <ql/processes/blackscholesprocess.hpp>

#include <ql/experimental/fx/fxtarf.hpp>
#include <ql/experimental/fx/proxyinstrument.hpp>

#include <boost/make_shared.hpp>

#include <iostream>

namespace QuantLib {

template <class RNG = PseudoRandom, class S = Statistics>
class McFxTarfEngine : public FxTarf::engine,
                       public McSimulation<SingleVariate, RNG, S> {
  public:
    typedef typename McSimulation<SingleVariate, RNG, S>::path_generator_type
        path_generator_type;
    typedef typename McSimulation<SingleVariate, RNG, S>::path_pricer_type
        path_pricer_type;
    typedef typename McSimulation<SingleVariate, RNG, S>::stats_type stats_type;
    //! constructor
    McFxTarfEngine(
        const boost::shared_ptr<GeneralizedBlackScholesProcess> &process,
        Size timeSteps, Size timeStepsPerYears, bool brownianBridge,
        bool antitheticVariate, Size requiredSamples, Real requiredTolerance,
        Size maxSamples, BigNatural seed,
        const Handle<YieldTermStructure> discount);

    void calculate() const;

  protected:
    // McSimulation Interface
    TimeGrid timeGrid() const;
    boost::shared_ptr<path_generator_type> pathGenerator() const;
    boost::shared_ptr<path_pricer_type> pathPricer() const;
    // data members
    boost::shared_ptr<GeneralizedBlackScholesProcess> process_;
    Size timeSteps_, timeStepsPerYear_;
    Size requiredSamples_, maxSamples_;
    Real requiredTolerance_;
    bool brownianBridge_;
    BigNatural seed_;
    const Handle<YieldTermStructure> discount_;
    mutable std::vector<Real> fixingTimes_, discounts_;
};

//! Monte Carlo fx-tarf engine factory
template <class RNG = PseudoRandom, class S = Statistics>
class MakeMcFxTarfEngine {
  public:
    MakeMcFxTarfEngine(
        const boost::shared_ptr<GeneralizedBlackScholesProcess> &);
    // named parameters
    MakeMcFxTarfEngine &withSteps(Size steps);
    MakeMcFxTarfEngine &withStepsPerYear(Size steps);
    MakeMcFxTarfEngine &withBrownianBridge(bool b = true);
    MakeMcFxTarfEngine &withAntitheticVariate(bool b = true);
    MakeMcFxTarfEngine &withSamples(Size samples);
    MakeMcFxTarfEngine &withAbsoluteTolerance(Real tolerance);
    MakeMcFxTarfEngine &withMaxSamples(Size samples);
    MakeMcFxTarfEngine &withSeed(BigNatural seed);
    MakeMcFxTarfEngine &
    withDiscount(const Handle<YieldTermStructure> &discount);
    // conversion to pricing engine
    operator boost::shared_ptr<PricingEngine>() const;

  private:
    boost::shared_ptr<GeneralizedBlackScholesProcess> process_;
    bool brownianBridge_, antithetic_;
    Size steps_, stepsPerYear_, samples_, maxSamples_;
    Real tolerance_;
    BigNatural seed_;
    Handle<YieldTermStructure> discount_;
};

//! Path Pricer
class FxTarfPathPricer : public PathPricer<Path> {
  public:
    FxTarfPathPricer(const std::vector<Real> &fixingTimes,
                     const std::vector<Real> &discounts,
                     const Real accumulatedAmount, const Real sourceNominal,
                     const FxTarf *instrument);
    Real operator()(const Path &path) const;

  private:
    const std::vector<Real> &fixingTimes_, &discounts_;
    const Real accumulatedAmount_, sourceNominal_;
    const FxTarf *instrument_;
    mutable std::vector<Size> fixingIndices_;
};

// Implementation

template <class RNG, class S>
McFxTarfEngine<RNG, S>::McFxTarfEngine(
    const boost::shared_ptr<GeneralizedBlackScholesProcess> &process,
    Size timeSteps, Size timeStepsPerYear, bool brownianBridge,
    bool antitheticVariate, Size requiredSamples, Real requiredTolerance,
    Size maxSamples, BigNatural seed, const Handle<YieldTermStructure> discount)
    : McSimulation<SingleVariate, RNG, S>(antitheticVariate, false),
      process_(process), timeSteps_(timeSteps),
      timeStepsPerYear_(timeStepsPerYear), requiredSamples_(requiredSamples),
      maxSamples_(maxSamples), requiredTolerance_(requiredTolerance),
    brownianBridge_(brownianBridge), seed_(seed), discount_(discount) {
    QL_REQUIRE(timeSteps != Null<Size>() || timeStepsPerYear != Null<Size>(),
               "no time steps provided");
    QL_REQUIRE(timeSteps == Null<Size>() || timeStepsPerYear == Null<Size>(),
               "both time steps and time steps per year were provided");
    QL_REQUIRE(timeSteps != 0, "timeSteps must be positive, "
                                   << timeSteps << " not allowed");
    QL_REQUIRE(timeStepsPerYear != 0, "timeStepsPerYear must be positive, "
                                          << timeStepsPerYear
                                          << " not allowed");
    QL_REQUIRE(!discount_.empty(),"no discount curve given");
    registerWith(process_);
}

template <class RNG, class S> void McFxTarfEngine<RNG, S>::calculate() const {

    // case where target is triggered, but not settled (if settled, the
    // instrument is expired)
    if (arguments_.accumulatedAmount >= arguments_.target) {
        int i = 1;
        while (detail::simple_event(arguments_.schedule.date(i)).hasOccurred())
            ++i;
        results_.value = arguments_.lastAmount *
                         discount_->discount(arguments_.schedule.date(i));
        return;
    }
    // case where only one fixing is left which is today
    if (arguments_.openFixingDates.back() ==
        Settings::instance().evaluationDate()) {
        Real fixing =
            arguments_.index->fixing(arguments_.openFixingDates.back());
        results_.value = arguments_.instrument->payout(fixing) *
                         arguments_.sourceNominal; // discount today = 1
        return;
    }
    // we know that we have at least one future fixing left, which
    // is not today (this case is handled in the calculate method)
    for (Size i = 0; i < arguments_.openFixingDates.size(); ++i) {
        fixingTimes_.push_back(process_->time(arguments_.openFixingDates[i]));
        discounts_.push_back(
            discount_->discount(arguments_.openPaymentDates[i]));
//        std::cout << "open fixing;" << arguments_.openFixingDates[i] << ";" << fixingTimes_[i] << std::endl;
    }
    McSimulation<SingleVariate, RNG, S>::calculate(
        requiredTolerance_, requiredSamples_, maxSamples_);
    results_.value = this->mcModel_->sampleAccumulator().mean();
    if (RNG::allowsErrorEstimate)
        results_.errorEstimate =
            this->mcModel_->sampleAccumulator().errorEstimate();

    // proxy
    results_.proxy = boost::make_shared<FxTarf::Proxy>();
}

template <class RNG, class S>
TimeGrid McFxTarfEngine<RNG, S>::timeGrid() const {
    if (timeSteps_ != Null<Size>()) {
        return TimeGrid(fixingTimes_.begin(), fixingTimes_.end(), timeSteps_);
    } else if (timeStepsPerYear_ != Null<Size>()) {
        Size steps = static_cast<Size>(timeStepsPerYear_ * fixingTimes_.back());
        return TimeGrid(fixingTimes_.begin(), fixingTimes_.end(),
                        std::max<Size>(steps, 1));
    } else {
        QL_FAIL("time steps not specified");
    }
}

template <class RNG, class S>
boost::shared_ptr<typename McFxTarfEngine<RNG, S>::path_generator_type>
McFxTarfEngine<RNG, S>::pathGenerator() const {
    TimeGrid grid = timeGrid();
    typename RNG::rsg_type gen =
        RNG::make_sequence_generator(grid.size() - 1, seed_);
    return boost::make_shared<path_generator_type>(process_, grid, gen,
                                                   brownianBridge_);
}

template <class RNG, class S>
boost::shared_ptr<typename McFxTarfEngine<RNG, S>::path_pricer_type>
McFxTarfEngine<RNG, S>::pathPricer() const {
    return boost::make_shared<FxTarfPathPricer>(
        fixingTimes_, discounts_, arguments_.accumulatedAmount,
        arguments_.sourceNominal, arguments_.instrument);
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S>::MakeMcFxTarfEngine(
    const boost::shared_ptr<GeneralizedBlackScholesProcess> &process)
    : process_(process), brownianBridge_(false), antithetic_(false),
      steps_(Null<Size>()), stepsPerYear_(Null<Size>()), samples_(Null<Size>()),
      maxSamples_(Null<Size>()), tolerance_(Null<Real>()), seed_(0),
      discount_(process->riskFreeRate()) {}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withSteps(Size steps) {
    steps_ = steps;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withStepsPerYear(Size steps) {
    stepsPerYear_ = steps;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withBrownianBridge(bool brownianBridge) {
    brownianBridge_ = brownianBridge;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withAntitheticVariate(bool b) {
    antithetic_ = b;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withSamples(Size samples) {
    QL_REQUIRE(tolerance_ == Null<Real>(), "tolerance already set");
    samples_ = samples;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withAbsoluteTolerance(Real tolerance) {
    QL_REQUIRE(samples_ == Null<Size>(), "number of samples already set");
    QL_REQUIRE(RNG::allowsErrorEstimate, "chosen random generator policy "
                                         "does not allow an error estimate");
    tolerance_ = tolerance;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withMaxSamples(Size samples) {
    maxSamples_ = samples;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &
MakeMcFxTarfEngine<RNG, S>::withSeed(BigNatural seed) {
    seed_ = seed;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S> &MakeMcFxTarfEngine<RNG, S>::withDiscount(
    const Handle<YieldTermStructure> &discount) {
    discount_ = discount;
    return *this;
}

template <class RNG, class S>
inline MakeMcFxTarfEngine<RNG, S>::
operator boost::shared_ptr<PricingEngine>() const {
    QL_REQUIRE(steps_ != Null<Size>() || stepsPerYear_ != Null<Size>(),
               "number of steps not given");
    QL_REQUIRE(steps_ == Null<Size>() || stepsPerYear_ == Null<Size>(),
               "number of steps overspecified");
    return boost::shared_ptr<PricingEngine>(new McFxTarfEngine<RNG, S>(
        process_, steps_, stepsPerYear_, brownianBridge_, antithetic_, samples_,
        tolerance_, maxSamples_, seed_, discount_));
}

} // namespace QuantLib
#endif
