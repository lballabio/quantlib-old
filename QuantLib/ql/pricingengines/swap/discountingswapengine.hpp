/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2009 StatPro Italia srl
 Copyright (C) 2011 Ferdinando Ametrano
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

/*! \file discountingswapengine.hpp
    \brief discounting swap engine
*/

#ifndef quantlib_discounting_swap_engine_hpp
#define quantlib_discounting_swap_engine_hpp

#include <ql/instruments/swap.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/handle.hpp>

namespace QuantLib {

template <class T> class DiscountingSwapEngine_t : public Swap_t<T>::engine {
  public:
    DiscountingSwapEngine_t(
        const Handle<YieldTermStructure_t<T> > &discountCurve =
            Handle<YieldTermStructure_t<T> >(),
        boost::optional<bool> includeSettlementDateFlows = boost::none,
        Date settlementDate = Date(), Date npvDate = Date());
    void calculate() const;
    Handle<YieldTermStructure_t<T> > discountCurve() const {
        return discountCurve_;
    }

  private:
    Handle<YieldTermStructure_t<T> > discountCurve_;
    boost::optional<bool> includeSettlementDateFlows_;
    Date settlementDate_, npvDate_;
};

typedef DiscountingSwapEngine_t<Real> DiscountingSwapEngine;

// implementation

template <class T>
DiscountingSwapEngine_t<T>::DiscountingSwapEngine_t(
    const Handle<YieldTermStructure_t<T> > &discountCurve,
    boost::optional<bool> includeSettlementDateFlows, Date settlementDate,
    Date npvDate)
    : discountCurve_(discountCurve),
      includeSettlementDateFlows_(includeSettlementDateFlows),
      settlementDate_(settlementDate), npvDate_(npvDate) {
    this->registerWith(discountCurve_);
}

template <class T> void DiscountingSwapEngine_t<T>::calculate() const {
    QL_REQUIRE(!discountCurve_.empty(),
               "discounting term structure handle is empty");

    this->results_.value = 0.0;
    this->results_.errorEstimate = Null<Real>();

    Date refDate = discountCurve_->referenceDate();

    Date settlementDate = settlementDate_;
    if (settlementDate_ == Date()) {
        settlementDate = refDate;
    } else {
        QL_REQUIRE(settlementDate >= refDate,
                   "settlement date (" << settlementDate
                                       << ") before "
                                          "discount curve reference date ("
                                       << refDate << ")");
    }

    this->results_.valuationDate = npvDate_;
    if (npvDate_ == Date()) {
        this->results_.valuationDate = refDate;
    } else {
        QL_REQUIRE(npvDate_ >= refDate,
                   "npv date (" << npvDate_ << ") before "
                                               "discount curve reference date ("
                                << refDate << ")");
    }
    this->results_.npvDateDiscount =
        discountCurve_->discount(this->results_.valuationDate);

    Size n = this->arguments_.legs.size();
    this->results_.legNPV.resize(n);
    this->results_.legBPS.resize(n);
    this->results_.startDiscounts.resize(n);
    this->results_.endDiscounts.resize(n);

    bool includeRefDateFlows =
        includeSettlementDateFlows_
            ? *includeSettlementDateFlows_
            : Settings::instance().includeReferenceDateEvents();

    for (Size i = 0; i < n; ++i) {
        try {
            const YieldTermStructure_t<T> &discount_ref = **discountCurve_;
            CashFlows::npvbps(
                this->arguments_.legs[i], discount_ref, includeRefDateFlows,
                settlementDate, this->results_.valuationDate,
                this->results_.legNPV[i], this->results_.legBPS[i]);
            this->results_.legNPV[i] *= this->arguments_.payer[i];
            this->results_.legBPS[i] *= this->arguments_.payer[i];

            if (!this->arguments_.legs[i].empty()) {
                Date d1 = CashFlows::startDate<T>(this->arguments_.legs[i]);
                if (d1 >= refDate)
                    this->results_.startDiscounts[i] =
                        discountCurve_->discount(d1);
                else
                    this->results_.startDiscounts[i] = Null<DiscountFactor>();

                Date d2 = CashFlows::maturityDate<T>(this->arguments_.legs[i]);
                if (d2 >= refDate)
                    this->results_.endDiscounts[i] =
                        discountCurve_->discount(d2);
                else
                    this->results_.endDiscounts[i] = Null<DiscountFactor>();
            } else {
                this->results_.startDiscounts[i] = Null<DiscountFactor>();
                this->results_.endDiscounts[i] = Null<DiscountFactor>();
            }

        } catch (std::exception &e) {
            QL_FAIL(io::ordinal(i + 1) << " leg: " << e.what());
        }
        this->results_.value += this->results_.legNPV[i];
    }
}
}

#endif
