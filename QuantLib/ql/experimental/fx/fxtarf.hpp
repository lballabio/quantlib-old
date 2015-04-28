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

/*! \file fxtarf.hpp
    \brief FX TARF instrument
*/

#ifndef quantlib_fxtarf_hpp
#define quantlib_fxtarf_hpp

#include <ql/experimental/fx/fxindex.hpp>
#include <ql/experimental/fx/proxyengine.hpp>
#include <ql/instrument.hpp>
#include <ql/time/schedule.hpp>
#include <ql/option.hpp>

namespace QuantLib {

class FxTarf : public Instrument {
  public:
    class arguments;
    class results;
    class engine;
    //! proxy description
    struct Proxy : ProxyDescription {
        bool dummy; // stub implementation
    };
    //! \name Constructors
    //@{
    /*! If the accumulatedAmount is not null, no past fixings are
        used to calculate the accumulated amount, but exactly this
        number is assumed to represent this amount. */
    FxTarf(const Schedule schedule, const boost::shared_ptr<FxIndex> &index,
           const Real sourceNominal, const Real strike,
           const Option::Type longPositionType, const Real shortPositionFactor,
           const Real target,
           const Handle<Quote> accumulatedAmount = Handle<Quote>());
    //@}
    //! \name Instrument interface
    //@{
    // the tarf is expired iff accumulated amount >= target
    // and this amount is settled
    bool isExpired() const;
    void setupArguments(PricingEngine::arguments *) const;
    void fetchResults(const PricingEngine::results *) const;
    //@}
    //! \name Additional interface
    //@{
    Date startDate() const;
    Date maturityDate() const;
    Real accumulatedAmount() const {
        return accumulatedAmountAndSettlement().first;
    }
    bool accumulatedAmountSettled() const {
        return accumulatedAmountAndSettlement().second;
    }
    boost::shared_ptr<ProxyDescription> proxy() const;

  protected:
    //! \name Instrument interface
    //@{
    void setupExpired() const;
    //@}

  private:
    std::pair<Real, bool> accumulatedAmountAndSettlement() const;
    const Schedule schedule_;
    const boost::shared_ptr<FxIndex> index_;
    const Real sourceNominal_, strike_;
    const Option::Type longPositionType_;
    const Real shortPositionFactor_;
    const Real target_;
    const Handle<Quote> accumulatedAmount_;
    mutable boost::shared_ptr<FxTarf::Proxy> proxy_;
};

class FxTarf::arguments : public virtual PricingEngine::arguments {
  public:
    Schedule schedule;
    boost::shared_ptr<FxIndex> index;
    Real sourceNominal;
    Real strike;
    Option::Type longPositionType;
    Real shortPositionFactor;
    Real target;
    Real accumulatedAmount;
    void validate() const;
};

class FxTarf::results : public Instrument::results {
  public:
    void reset();
    boost::shared_ptr<FxTarf::Proxy> proxy;
};

class FxTarf::engine
    : public GenericEngine<FxTarf::arguments, FxTarf::results> {};

} // namespace QuantLib

#endif
