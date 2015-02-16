/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2006, 2011 Ferdinando Ametrano
 Copyright (C) 2007, 2008 StatPro Italia srl
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

/*! \file swap.hpp
    \brief Interest rate swap
*/

#ifndef quantlib_swap_hpp
#define quantlib_swap_hpp

#include <ql/instrument.hpp>
#include <ql/cashflow.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/coupon.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

//! Interest rate swap
/*! The cash flows belonging to the first leg are paid;
    the ones belonging to the second leg are received.

    \ingroup instruments
*/

template <class T> class Swap_t : public Instrument_t<T> {
  public:
    class arguments;
    class results;
    class engine;
    //! \name Constructors
    //@{
    /*! The cash flows belonging to the first leg are paid;
        the ones belonging to the second leg are received.
    */
    Swap_t(const typename Leg_t<T>::Type &firstLeg,
           const typename Leg_t<T>::Type &secondLeg);
    /*! Multi leg constructor. */
    Swap_t(const std::vector<typename Leg_t<T>::Type> &legs,
           const std::vector<bool> &payer);
    //@}
    //! \name Instrument interface
    //@{
    bool isExpired() const;
    void setupArguments(PricingEngine::arguments *) const;
    void fetchResults(const PricingEngine::results *) const;
    //@}
    //! \name Additional interface
    //@{
    Date startDate() const;
    Date maturityDate() const;
    T legBPS(Size j) const {
        QL_REQUIRE(j < legs_.size(), "leg# " << j << " doesn't exist!");
        this->calculate();
        return legBPS_[j];
    }
    T legNPV(Size j) const {
        QL_REQUIRE(j < legs_.size(), "leg #" << j << " doesn't exist!");
        this->calculate();
        return legNPV_[j];
    }
    T startDiscounts(Size j) const {
        QL_REQUIRE(j < legs_.size(), "leg #" << j << " doesn't exist!");
        this->calculate();
        return startDiscounts_[j];
    }
    T endDiscounts(Size j) const {
        QL_REQUIRE(j < legs_.size(), "leg #" << j << " doesn't exist!");
        this->calculate();
        return endDiscounts_[j];
    }
    T npvDateDiscount() const {
        this->calculate();
        return npvDateDiscount_;
    }
    const typename Leg_t<T>::Type &leg(Size j) const {
        QL_REQUIRE(j < legs_.size(), "leg #" << j << " doesn't exist!");
        return legs_[j];
    }
    //@}
  protected:
    //! \name Constructors
    //@{
    /*! This constructor can be used by derived classes that will
        build their legs themselves.
    */
    Swap_t(Size legs);
    //@}
    //! \name Instrument interface
    //@{
    void setupExpired() const;
    //@}
    // data members
    std::vector<typename Leg_t<T>::Type> legs_;
    std::vector<T> payer_;
    mutable std::vector<T> legNPV_;
    mutable std::vector<T> legBPS_;
    mutable std::vector<T> startDiscounts_, endDiscounts_;
    mutable T npvDateDiscount_;
};

template <class T>
class Swap_t<T>::arguments : public virtual PricingEngine::arguments {
  public:
    std::vector<typename Leg_t<T>::Type> legs;
    std::vector<T> payer;
    void validate() const;
};

template <class T> class Swap_t<T>::results : public Instrument_t<T>::results {
  public:
    std::vector<T> legNPV;
    std::vector<T> legBPS;
    std::vector<T> startDiscounts, endDiscounts;
    T npvDateDiscount;
    void reset();
};

template <class T>
class Swap_t<T>::engine : public GenericEngine<typename Swap_t<T>::arguments,
                                               typename Swap_t<T>::results> {};

typedef Swap_t<Real> Swap;

// implementation

template <class T>
Swap_t<T>::Swap_t(const typename Leg_t<T>::Type &firstLeg,
                  const typename Leg_t<T>::Type &secondLeg)
    : legs_(2), payer_(2), legNPV_(2, 0.0), legBPS_(2, 0.0),
      startDiscounts_(2, 0.0), endDiscounts_(2, 0.0), npvDateDiscount_(0.0) {
    legs_[0] = firstLeg;
    legs_[1] = secondLeg;
    payer_[0] = -1.0;
    payer_[1] = 1.0;
    for (typename Leg_t<T>::Type::iterator i = legs_[0].begin();
         i != legs_[0].end(); ++i)
        this->registerWith(*i);
    for (typename Leg_t<T>::Type::iterator i = legs_[1].begin();
         i != legs_[1].end(); ++i)
        this->registerWith(*i);
}

template <class T>
Swap_t<T>::Swap_t(const std::vector<typename Leg_t<T>::Type> &legs,
                  const std::vector<bool> &payer)
    : legs_(legs), payer_(legs.size(), 1.0), legNPV_(legs.size(), 0.0),
      legBPS_(legs.size(), 0.0), startDiscounts_(legs.size(), 0.0),
      endDiscounts_(legs.size(), 0.0), npvDateDiscount_(0.0) {
    QL_REQUIRE(payer.size() == legs_.size(),
               "size mismatch between payer (" << payer.size() << ") and legs ("
                                               << legs_.size() << ")");
    for (Size j = 0; j < legs_.size(); ++j) {
        if (payer[j])
            payer_[j] = -1.0;
        for (typename Leg_t<T>::Type::iterator i = legs_[j].begin();
             i != legs_[j].end(); ++i)
            this->registerWith(*i);
    }
}

template <class T>
Swap_t<T>::Swap_t(Size legs)
    : legs_(legs), payer_(legs), legNPV_(legs, 0.0), legBPS_(legs, 0.0),
      startDiscounts_(legs, 0.0), endDiscounts_(legs, 0.0),
      npvDateDiscount_(0.0) {}

template <class T> bool Swap_t<T>::isExpired() const {
    for (Size j = 0; j < legs_.size(); ++j) {
        typename Leg_t<T>::Type::const_iterator i;
        for (i = legs_[j].begin(); i != legs_[j].end(); ++i)
            if (!(*i)->hasOccurred())
                return false;
    }
    return true;
}

template <class T> void Swap_t<T>::setupExpired() const {
    Instrument_t<T>::setupExpired();
    std::fill(legBPS_.begin(), legBPS_.end(), 0.0);
    std::fill(legNPV_.begin(), legNPV_.end(), 0.0);
    std::fill(startDiscounts_.begin(), startDiscounts_.end(), 0.0);
    std::fill(endDiscounts_.begin(), endDiscounts_.end(), 0.0);
    npvDateDiscount_ = 0.0;
}

template <class T>
void Swap_t<T>::setupArguments(PricingEngine::arguments *args) const {
    Swap_t<T>::arguments *arguments =
        dynamic_cast<Swap_t<T>::arguments *>(args);
    QL_REQUIRE(arguments != 0, "wrong argument type");

    arguments->legs = legs_;
    arguments->payer = payer_;
}

template <class T>
void Swap_t<T>::fetchResults(const PricingEngine::results *r) const {
    Instrument_t<T>::fetchResults(r);

    const Swap_t<T>::results *results =
        dynamic_cast<const Swap_t<T>::results *>(r);
    QL_REQUIRE(results != 0, "wrong result type");

    if (!results->legNPV.empty()) {
        QL_REQUIRE(results->legNPV.size() == legNPV_.size(),
                   "wrong number of leg NPV returned");
        legNPV_ = results->legNPV;
    } else {
        std::fill(legNPV_.begin(), legNPV_.end(), Null<Real>());
    }

    if (!results->legBPS.empty()) {
        QL_REQUIRE(results->legBPS.size() == legBPS_.size(),
                   "wrong number of leg BPS returned");
        legBPS_ = results->legBPS;
    } else {
        std::fill(legBPS_.begin(), legBPS_.end(), Null<Real>());
    }

    if (!results->startDiscounts.empty()) {
        QL_REQUIRE(results->startDiscounts.size() == startDiscounts_.size(),
                   "wrong number of leg start discounts returned");
        startDiscounts_ = results->startDiscounts;
    } else {
        std::fill(startDiscounts_.begin(), startDiscounts_.end(),
                  Null<DiscountFactor>());
    }

    if (!results->endDiscounts.empty()) {
        QL_REQUIRE(results->endDiscounts.size() == endDiscounts_.size(),
                   "wrong number of leg end discounts returned");
        endDiscounts_ = results->endDiscounts;
    } else {
        std::fill(endDiscounts_.begin(), endDiscounts_.end(),
                  Null<DiscountFactor>());
    }

    if (results->npvDateDiscount != Null<DiscountFactor>()) {
        npvDateDiscount_ = results->npvDateDiscount;
    } else {
        npvDateDiscount_ = Null<DiscountFactor>();
    }
}

template <class T> Date Swap_t<T>::startDate() const {
    QL_REQUIRE(!legs_.empty(), "no legs given");
    Date d = CashFlows::startDate<T>(legs_[0]);
    for (Size j = 1; j < legs_.size(); ++j)
        d = std::min(d, CashFlows::startDate<T>(legs_[j]));
    return d;
}

template <class T> Date Swap_t<T>::maturityDate() const {
    QL_REQUIRE(!legs_.empty(), "no legs given");
    Date d = CashFlows::maturityDate<T>(legs_[0]);
    for (Size j = 1; j < legs_.size(); ++j)
        d = std::max(d, CashFlows::maturityDate<T>(legs_[j]));
    return d;
}

template <class T> void Swap_t<T>::arguments::validate() const {
    QL_REQUIRE(legs.size() == payer.size(),
               "number of legs and multipliers differ");
}

template <class T> void Swap_t<T>::results::reset() {
    Instrument_t<T>::results::reset();
    legNPV.clear();
    legBPS.clear();
    startDiscounts.clear();
    endDiscounts.clear();
    npvDateDiscount = Null<DiscountFactor>();
}

} // namespace QuantLib
#endif
