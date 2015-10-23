/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

/*! \file zcinflationcapfloor.hpp
 \brief Zero coupon cap / floor
 */

#ifndef quantlib_inflcf_hpp
#define quantlib_inflcf_hpp

#include <ql/cashflow.hpp>
#include <ql/index.hpp>
#include <ql/indexes/inflationindex.hpp>
#include <ql/handle.hpp>
#include <ql/option.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/inflationtermstructure.hpp>
#include <ql/termstructures/volatility/equityfx/blackvoltermstructure.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

namespace QuantLib {

//! Cash flow dependent on an index ratio with cap floor payoff.

/*! This cash flow is not a coupon, i.e., there's no accrual.  The
    amount is E( max (i(T)/i(0) - (1 + k)^T , 0) ), i.e. the strike
    is annualized.

    WARNING the volatility is taken from the black vol term structure
    naively using strikeand lastFixingDate. If the firstFixingDate is
    not the same as for quoted instruments (e.g. evalDate - 3m), this
    leads to wrong results. Todo - adjust strike in this case before
    reading off the vol from the ts
*/

class ZCInflationCapFloor : public CashFlow, public Observer {
  public:
    ZCInflationCapFloor(Real notional,
                        const boost::shared_ptr<ZeroInflationIndex> &index,
                        const Handle<YieldTermStructure> &nominalYts,
                        const Handle<ZeroInflationTermStructure> &inflationYts,
                        const Handle<BlackVolTermStructure> &inflationVol,
                        const Date &firstFixingDate, const Date &lastFixingDate,
                        const Date &paymentDate, const Real strike,
                        const Option::Type type,
                        const DayCounter &dc = Actual365Fixed(),
                        const Date &lastKnownFixingDate = Null<Date>()) // if not set, use base date from inflation ts
    : notional_(notional), index_(index), firstFixingDate_(firstFixingDate),
          lastFixingDate_(lastFixingDate), paymentDate_(paymentDate),
          nominalYts_(nominalYts), inflationYts_(inflationYts),
        inflationVol_(inflationVol), strike_(strike), type_(type),
        dc_(dc) {

        // compute actual fixing dates out of first and last fixing dates
        if (index_->interpolated()) {
            actualFirstFixingDate_ = inflationYts_->calendar().advance(
                firstFixingDate_, -inflationYts_->observationLag());
            actualLastFixingDate_ = inflationYts_->calendar().advance(
                lastFixingDate_, -inflationYts_->observationLag());
            
        } else {
            std::pair<Date, Date> dd =
                inflationPeriod(firstFixingDate_ - inflationYts_->observationLag(),
                                index_->frequency());
            actualFirstFixingDate_ = dd.first;
            dd = inflationPeriod(lastFixingDate_ -
                                     inflationYts_->observationLag(),
                                 index_->frequency());
            actualLastFixingDate_ = dd.first;
        }
        fixingTime_ = inflationVol_->dayCounter().yearFraction(
            lastKnownFixingDate == Null<Date>() ? inflationYts_->baseDate()
                                                : lastKnownFixingDate,
            actualLastFixingDate_);

        // registerWith(index);
        registerWith(nominalYts);
        registerWith(inflationYts);

    }

    //! \name Event interface
    //@{
    Date date() const { return paymentDate_; }
    //@}
    virtual Real notional() const { return notional_; }
    virtual Date actualFirstFixingDate() const { return actualFirstFixingDate_; }
    virtual Date actualLastFixingDate() const { return actualLastFixingDate_; }
    // virtual boost::shared_ptr<Index> index() const { return index_; }
    //! \name CashFlow interface
    //@{
    Real amount() const; // already virtual
    //@}
    // return implied unit displaced vol from undiscounted option price
    Real impliedTotalVariance(Real undeflatedPrice) const;
    // return implied vol
    Real impliedVolatility(Real undeflatedPrice) const;
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
    //! \name Observer interface
    //@{
    void update() { notifyObservers(); }
    //@}
  private:
    Real amount(Real totalVariance) const;
    Real notional_;
    boost::shared_ptr<ZeroInflationIndex> index_;
    Date firstFixingDate_, lastFixingDate_, paymentDate_;
    Date actualFirstFixingDate_, actualLastFixingDate_;
    Real fixingTime_;
    Handle<YieldTermStructure> nominalYts_;
    Handle<ZeroInflationTermStructure> inflationYts_;
    Handle<BlackVolTermStructure> inflationVol_;
    Real strike_;
    Option::Type type_;
    DayCounter dc_;

    class ImpliedVarianceHelper {
      public:
        ImpliedVarianceHelper(const ZCInflationCapFloor *h, Real target)
            : h_(h), target_(target) {}
        Real operator()(Real v) const { return target_ - h_->amount(v); }

      private:
        const ZCInflationCapFloor *h_;
        Real target_;
    };
};

// inline definitions

inline void ZCInflationCapFloor::accept(AcyclicVisitor &v) {
    Visitor<ZCInflationCapFloor> *v1 =
        dynamic_cast<Visitor<ZCInflationCapFloor> *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        CashFlow::accept(v);
}
}

#endif
