/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Giorgio Facchinetti
 Copyright (C) 2009 StatPro Italia srl
 Copyright (C) 2015 Cheng Li

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

/*! \file discountingbondengine.hpp
    \brief discounting bond engine
*/

#ifndef quantlib_discounting_bond_engine_hpp
#define quantlib_discounting_bond_engine_hpp

#include <ql/handle.hpp>
#include <ql/instruments/bond.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/cashflows/cashflows.hpp>

namespace QuantLib {

template <class T> class DiscountingBondEngine_t : public Bond_t<T>::engine {
  public:
    DiscountingBondEngine_t(
        const Handle<YieldTermStructure_t<T> > &discountCurve =
            Handle<YieldTermStructure_t<T> >(),
        boost::optional<bool> includeSettlementDateFlows = boost::none);
    void calculate() const;
    Handle<YieldTermStructure_t<T> > discountCurve() const {
        return discountCurve_;
    }

  private:
    Handle<YieldTermStructure_t<T> > discountCurve_;
    boost::optional<bool> includeSettlementDateFlows_;
    bool allowExtrop_;
};

typedef DiscountingBondEngine_t<Real> DiscountingBondEngine;

// implementation

template <class T>
DiscountingBondEngine_t<T>::DiscountingBondEngine_t(
    const Handle<YieldTermStructure_t<T> > &discountCurve,
    boost::optional<bool> includeSettlementDateFlows)
    : discountCurve_(discountCurve),
      includeSettlementDateFlows_(includeSettlementDateFlows) {
    this->registerWith(discountCurve_);
}

template <class T> void DiscountingBondEngine_t<T>::calculate() const {
    QL_REQUIRE(!discountCurve_.empty(),
               "discounting term structure handle is empty");

    this->results_.valuationDate = (*discountCurve_)->referenceDate();

    bool includeRefDateFlows =
        this->includeSettlementDateFlows_
            ? *(this->includeSettlementDateFlows_)
            : Settings::instance().includeReferenceDateEvents();

    this->results_.value = CashFlows::npv<T>(
        this->arguments_.cashflows, **discountCurve_, includeRefDateFlows,
        this->results_.valuationDate, this->results_.valuationDate);

    // a bond's cashflow on settlement date is never taken into
    // account, so we might have to play it safe and recalculate
    if (!includeRefDateFlows &&
        this->results_.valuationDate == this->arguments_.settlementDate) {
        // same parameters as above, we can avoid another call
        this->results_.settlementValue = this->results_.value;
    } else {
        // no such luck
        this->results_.settlementValue = CashFlows::npv<T>(
            this->arguments_.cashflows, **(this->discountCurve_), false,
            this->arguments_.settlementDate, this->arguments_.settlementDate);
    }
}
}

#endif
