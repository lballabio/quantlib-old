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

/*! \file betaeta.hpp
    \brief Hagan / Woodward beta-eta model
*/

#ifndef quantlib_model_betaeta_hpp
#define quantlib_model_betaeta_hpp

#include <ql/experimental/models/betaetacore.hpp>
#include <ql/models/model.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/instruments/vanillaswap.hpp>

#include <boost/unordered_map.hpp>

namespace QuantLib {

// there is a big overlap with the Gaussian1d model interface
// refactor this, i.e. create a common base class and build
// the engines on top of this

class BetaEta : public TermStructureConsistentModel,
                public CalibratedModel,
                public LazyObject {
  public:
    // constant mean reversion
    BetaEta(const Handle<YieldTermStructure> &termStructure,
            const std::vector<Date> &volstepdates,
            const std::vector<Real> &volatilities, const Real reversion,
            const Real beta, const Real eta);
    // piecewise mean reversion (with same step dates as volatilities)
    BetaEta(const Handle<YieldTermStructure> &termStructure,
            const std::vector<Date> &volstepdates,
            const std::vector<Real> &volatilities,
            const std::vector<Real> &reversions, const Real beta,
            const Real eta);
    // constant mean reversion with floating model data
    BetaEta(const Handle<YieldTermStructure> &termStructure,
            const std::vector<Date> &volstepdates,
            const std::vector<Handle<Quote> > &volatilities,
            const Handle<Quote> reversion, const Handle<Quote> beta,
            const Handle<Quote> eta);
    // piecewise mean reversion with floating model data
    BetaEta(const Handle<YieldTermStructure> &termStructure,
            const std::vector<Date> &volstepdates,
            const std::vector<Handle<Quote> > &volatilities,
            const std::vector<Handle<Quote> > &reversions,
            const Handle<Quote> beta, const Handle<Quote> eta);

    const Real numeraire(const Time t, const Real x = 0.0,
                         const Handle<YieldTermStructure> &yts =
                             Handle<YieldTermStructure>()) const;

    const Real zerobond(const Time T, const Time t = 0.0, const Real x = 0.0,
                        const Handle<YieldTermStructure> &yts =
                            Handle<YieldTermStructure>()) const;

    const Real numeraire(const Date &referenceDate, const Real y = 0.0,
                         const Handle<YieldTermStructure> &yts =
                             Handle<YieldTermStructure>()) const;
    const Real zerobond(const Date &maturity,
                        const Date &referenceDate = Null<Date>(),
                        const Real x = 0.0,
                        const Handle<YieldTermStructure> &yts =
                            Handle<YieldTermStructure>()) const;

    const Real forwardRate(const Date &fixing,
                           const Date &referenceDate = Null<Date>(),
                           const Real x = 0.0,
                           boost::shared_ptr<IborIndex> iborIdx =
                               boost::shared_ptr<IborIndex>()) const;

    const Real swapRate(const Date &fixing, const Period &tenor,
                        const Date &referenceDate = Null<Date>(),
                        const Real x = 0.0,
                        boost::shared_ptr<SwapIndex> swapIdx =
                            boost::shared_ptr<SwapIndex>()) const;

    const Real swapAnnuity(const Date &fixing, const Period &tenor,
                           const Date &referenceDate = Null<Date>(),
                           const Real x = 0.0,
                           boost::shared_ptr<SwapIndex> swapIdx =
                               boost::shared_ptr<SwapIndex>()) const;

    /*! Generates a grid of values for the state variable $x$
       at time $T$ conditional on $x(t)=x$, covering stdDevs
       standard deviations assuming an approximate variance
       $\int_t^T \alpha^2$ for $x$. The grid consists of
       2*gridPoints+1 points */
    const Disposable<Array> xGrid(const Real stdDevs, const int gridPoints,
                                  const Real T = 1.0, const Real t = 0,
                                  const Real x = 0) const;

    /*! integrates f against the conditional density $x(t)|x(t0)=x0$
        covering stdDevs approximate standard deviations in the same
        sense as in the xGrid implementation */
    const Real integrate(const Real stdDevs,
                         const boost::function<Real(Real)> &f, const Real t0,
                         const Real x0, const Real t) const;

  private:
    void generateArguments() { notifyObservers(); }

    // It is of great importance for performance reasons to cache underlying
    // swaps generated from indexes. In addition the indexes may only be given
    // as templates for the conventions with the tenor replaced by the actual
    // one later on.

    struct CachedSwapKey {
        const boost::shared_ptr<SwapIndex> index;
        const Date fixing;
        const Period tenor;
        const bool operator==(const CachedSwapKey &o) const {
            return index->name() == o.index->name() && fixing == o.fixing &&
                   tenor == o.tenor;
        }
    };

    struct CachedSwapKeyHasher
        : std::unary_function<CachedSwapKey, std::size_t> {
        std::size_t operator()(CachedSwapKey const &x) const {
            std::size_t seed = 0;
            boost::hash_combine(seed, x.index->name());
            boost::hash_combine(seed, x.fixing.serialNumber());
            boost::hash_combine(seed, x.tenor.length());
            boost::hash_combine(seed, x.tenor.units());
            return seed;
        }
    };

    typedef boost::unordered_map<CachedSwapKey, boost::shared_ptr<VanillaSwap>,
                                 CachedSwapKeyHasher> CacheType;

    mutable CacheType swapCache_;

    // retrieve underlying swap from cache if possible, otherwise
    // create it and store it in the cache
    boost::shared_ptr<VanillaSwap>
    underlyingSwap(const boost::shared_ptr<SwapIndex> &index,
                   const Date &expiry, const Period &tenor) const {

        CachedSwapKey k = {index, expiry, tenor};
        CacheType::iterator i = swapCache_.find(k);
        if (i == swapCache_.end()) {
            boost::shared_ptr<VanillaSwap> underlying =
                index->clone(tenor)->underlyingSwap(expiry);
            swapCache_.insert(std::make_pair(k, underlying));
            return underlying;
        }
        return i->second;
    }

    void update() { LazyObject::update(); }

    void performCalculations() const {
        evaluationDate_ = Settings::instance().evaluationDate();
        enforcesTodaysHistoricFixings_ =
            Settings::instance().enforcesTodaysHistoricFixings();
        updateTimes();
        updateState();
    }

    void updateTimes() const;
    void updateState() const;
    void initialize();

    Parameter &reversion_, &sigma_, &pBeta_, &pEta_;
    std::vector<Handle<Quote> > volatilities_;
    std::vector<Handle<Quote> > reversions_;
    Handle<Quote> beta_;
    Handle<Quote> eta_;
    mutable Real betaLink_, etaLink_; // redundant, just used as a link to
                                      // the core computation class

    std::vector<Date> volstepdates_; // these are shared between volatilities
                                     // and reversions in case of piecewise
                                     // reversions
    mutable std::vector<Time> volsteptimes_;
    mutable Array volsteptimesArray_; // redundant, just used as a link to
                                      // the core computation class

    mutable Date evaluationDate_;
    mutable bool enforcesTodaysHistoricFixings_;

    boost::shared_ptr<BetaEtaCore> core_;
};

// implementation

inline const Real
BetaEta::numeraire(const Date &referenceDate, const Real x,
                   const Handle<YieldTermStructure> &yts) const {

    return numeraire(termStructure()->timeFromReference(referenceDate), x, yts);
}

inline const Real
BetaEta::zerobond(const Date &maturity, const Date &referenceDate, const Real y,
                  const Handle<YieldTermStructure> &yts) const {

    return zerobond(termStructure()->timeFromReference(maturity),
                    referenceDate != Null<Date>()
                        ? termStructure()->timeFromReference(referenceDate)
                        : 0.0,
                    y, yts);
}

inline const Real BetaEta::integrate(const Real stdDevs,
                                     const boost::function<Real(Real)> &f,
                                     const Real t0, const Real x0,
                                     const Real t) const {
    Real s = core_->tau(t0, t);
    boost::shared_ptr<Integrator> i = core_->integrator();
    return (*i)(f, x0 - stdDevs * s, x0 + stdDevs * s);
}

} // namespace QuantLib

#endif
