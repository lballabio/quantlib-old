/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers
 Copyright (C) 2015 Roland Lichters

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

/*! cf. Hagan, Woodward: Markov interest rate models,
    Applied Mathematical Finance 6, 233–260 (1999)

    We assume a reflecting barrier at x = -1/beta (cf.
    the last paragraph on p. 241).

    We assume a piecewise constant reversion \kappa and
    set \lambda(t) := (1-exp(-\kappa*t))/\kappa
    note that these are effective (integrated) rather
    than forward-forward reversions though
*/

// TODO there is a big overlap with the Gaussian1d model interface
// refactor this, i.e. create a common base class and build the
// engines on top of this

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

    const Array &reversion() const { return reversion_.params(); }
    const Array &volatility() const { return sigma_.params(); }
    const Real beta() const { return pBeta_.params()[0]; }
    const Real eta() const { return pEta_.params()[0]; }

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

    /*! for testing purposes we can switch off the usage of tabulated values */
    const void useTabulation(const bool useTabulation) {
        if (useTabulation_ != useTabulation) {
            useTabulation_ = useTabulation;
            update();
        }
    }

    // calibration constraints
    // TODO what is of practical use here ?

    Disposable<std::vector<bool> > MoveVolatility(Size i) {
        QL_REQUIRE(i < volatilities_.size(),
                   "volatility with index " << i << " does not exist (0..."
                                            << volatilities_.size() - 1 << ")");
        std::vector<bool> res(reversions_.size() + volatilities_.size() + 2,
                              true);
        res[reversions_.size() + i] = false;
        return res;
    }

    // With fixed reversion, beta and eta calibrate the volatilities
    // one by one to the given helpers. The same comments as in the
    // corresonding method in Gsr hold.
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

  private:
    void generateArguments() { notifyObservers(); }

    // see Gaussian1dModel

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
    }

    void updateTimes() const;
    void updateVolatility();
    void updateReversion();
    void updateBeta();
    void updateEta();

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
    boost::shared_ptr<Integrator> integrator_, integrator2_;

    class integrand {
      public:
        integrand(const Real t0, const Real x0, const Real t,
                  const boost::function<Real(Real)> &f, const BetaEtaCore &core)
            : t0_(t0), x0_(x0), t_(t), f_(f), core_(core) {}
        Real operator()(Real x) const {
            return f_(x) * core_.p(t0_, x0_, t_, x);
        }

      private:
        const Real t0_, x0_, t_;
        const boost::function<Real(Real)> &f_;
        const BetaEtaCore &core_;
    };
    friend class integrand;

    bool useTabulation_; // for testing, normally it should be true

    struct VolatilityObserver : public Observer {
        VolatilityObserver(BetaEta *p) : p_(p) {}
        void update() { p_->updateVolatility(); }
        BetaEta *p_;
    };
    struct ReversionObserver : public Observer {
        ReversionObserver(BetaEta *p) : p_(p) {}
        void update() { p_->updateReversion(); }
        BetaEta *p_;
    };
    struct BetaObserver : public Observer {
        BetaObserver(BetaEta *p) : p_(p) {}
        void update() { p_->updateBeta(); }
        BetaEta *p_;
    };
    struct EtaObserver : public Observer {
        EtaObserver(BetaEta *p) : p_(p) {}
        void update() { p_->updateEta(); }
        BetaEta *p_;
    };

    boost::shared_ptr<VolatilityObserver> volatilityObserver_;
    boost::shared_ptr<ReversionObserver> reversionObserver_;
    boost::shared_ptr<BetaObserver> betaObserver_;
    boost::shared_ptr<EtaObserver> etaObserver_;
};

// implementation

inline const Real
BetaEta::numeraire(const Date &referenceDate, const Real x,
                   const Handle<YieldTermStructure> &yts) const {

    return numeraire(termStructure()->timeFromReference(referenceDate), x, yts);
}

inline const Real
BetaEta::zerobond(const Date &maturity, const Date &referenceDate, const Real x,
                  const Handle<YieldTermStructure> &yts) const {

    return zerobond(termStructure()->timeFromReference(maturity),
                    referenceDate != Null<Date>()
                        ? termStructure()->timeFromReference(referenceDate)
                        : 0.0,
                    x, yts);
}

inline const Real BetaEta::integrate(const Real stdDevs,
                                     const boost::function<Real(Real)> &f,
                                     const Real t0, const Real x0,
                                     const Real t) const {
    Real s = std::sqrt(core_->tau(t0, t));
    integrand phi(t0, x0, t, f, *core_);
    Real result;
    // left integration bound should be greater or equal to barrier
    Real a = std::max(x0 - stdDevs * s, -1.0 / beta_->value());
    Real b = x0 + stdDevs * s;
    try {
        result = (*integrator_)(phi, a, b);
    } catch (QuantLib::Error) {
        result = (*integrator2_)(phi, a, b);
    }
    return result;
}

} // namespace QuantLib

#endif
