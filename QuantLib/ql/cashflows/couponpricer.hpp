/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Giorgio Facchinetti
 Copyright (C) 2007 Cristina Duminuco
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

/*! \file couponpricer.hpp
    \brief Coupon pricers
*/

#ifndef quantlib_coupon_pricer_hpp
#define quantlib_coupon_pricer_hpp

#include <ql/termstructures/volatility/optionlet/optionletvolatilitystructure.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/cashflow.hpp>
#include <ql/option.hpp>
#include <ql/cashflows/couponpricerbase2.hpp>
#include <ql/cashflows/capflooredcoupon.hpp>
#include <ql/cashflows/iborcouponbase.hpp>
#include <ql/cashflows/cmscouponbase.hpp>
#include <ql/cashflows/digitalcoupon.hpp>
#include <ql/cashflows/digitalcmscoupon.hpp>
#include <ql/cashflows/digitaliborcoupon.hpp>
#include <ql/cashflows/rangeaccrual.hpp>
#include <ql/experimental/coupons/subperiodcoupons.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

#include <boost/type_traits.hpp>

namespace QuantLib {

using boost::dynamic_pointer_cast;

//! base pricer for vanilla CMS coupons
template <class T>
class CmsCouponPricer_t : public FloatingRateCouponPricer_t<T> {
  public:
    CmsCouponPricer_t(const Handle<SwaptionVolatilityStructure> &v =
                          Handle<SwaptionVolatilityStructure>())
        : swaptionVol_(v) {
        this->registerWith(swaptionVol_);
    }

    Handle<SwaptionVolatilityStructure> swaptionVolatility() const {
        return swaptionVol_;
    }
    void setSwaptionVolatility(const Handle<SwaptionVolatilityStructure> &v =
                                   Handle<SwaptionVolatilityStructure>()) {
        this->unregisterWith(swaptionVol_);
        swaptionVol_ = v;
        this->registerWith(swaptionVol_);
        this->update();
    }

  private:
    Handle<SwaptionVolatilityStructure> swaptionVol_;
};

typedef CmsCouponPricer_t<Real> CmsCouponPricer;

/*! (CMS) coupon pricer that has a mean reversion parameter which can be
  used to calibrate to cms market quotes */
template <class T> class MeanRevertingPricer_t {
  public:
    virtual T meanReversion() const = 0;
    virtual void setMeanReversion(const Handle<Quote_t<T> > &) = 0;
};

typedef MeanRevertingPricer_t<Real> MeanRevertingPricer;

class SubPeriodsPricer : public FloatingRateCouponPricer {
  public:
    virtual Rate swapletRate() const;
    virtual Real capletPrice(Rate effectiveCap) const;
    virtual Rate capletRate(Rate effectiveCap) const;
    virtual Real floorletPrice(Rate effectiveFloor) const;
    virtual Rate floorletRate(Rate effectiveFloor) const;
    void initialize(const FloatingRateCoupon &coupon);

  protected:
    const SubPeriodsCoupon *coupon_;
    Real startTime_;
    Real endTime_;
    Real accrualFactor_;
    std::vector<Real> observationTimes_;
    std::vector<Real> observationCvg_;
    std::vector<Real> initialValues_;

    std::vector<Date> observationIndexStartDates_;
    std::vector<Date> observationIndexEndDates_;

    Size observations_;
    Real discount_;
    Real gearing_;
    Spread spread_;
    Real spreadLegValue_;
};

class AveragingRatePricer : public SubPeriodsPricer {
  public:
    Real swapletPrice() const;
};

class CompoundingRatePricer : public SubPeriodsPricer {
  public:
    Real swapletPrice() const;
};

template <template <class> class S, class T>
void setCouponPricer(
    const typename Leg_t<T>::Type &leg, const boost::shared_ptr<S<T> > &pricer,
    typename boost::enable_if<boost::is_base_of<FloatingRateCouponPricer_t<T>,
                                                S<T> > >::type *dummy = 0);

template <template <class> class S, class T>
void setCouponPricers(
    const typename Leg_t<T>::Type &leg,
    const std::vector<boost::shared_ptr<S<T> > > &pricers,
    typename boost::enable_if<boost::is_base_of<FloatingRateCouponPricer_t<T>,
                                                S<T> > >::type *dummy = 0);
//===========================================================================//
//                         CouponSelectorToSetPricer                         //
//===========================================================================//

namespace {

template <class T>
class PricerSetter : public AcyclicVisitor,
                     public Visitor<CashFlow_t<T> >,
                     public Visitor<Coupon_t<T> >,
                     public Visitor<IborCoupon_t<T> >,
                     public Visitor<CmsCoupon_t<T> >,
                     public Visitor<CappedFlooredIborCoupon_t<T> >,
                     public Visitor<CappedFlooredCmsCoupon_t<T> >,
                     public Visitor<DigitalIborCoupon_t<T> >,
                     public Visitor<DigitalCmsCoupon_t<T> >,
                     public Visitor<RangeAccrualFloatersCoupon>,
                     public Visitor<SubPeriodsCoupon> {
  private:
    const boost::shared_ptr<FloatingRateCouponPricer_t<T> > pricer_;

  public:
    PricerSetter(
        const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &pricer)
        : pricer_(pricer) {}

    void visit(CashFlow_t<T> &c);
    void visit(Coupon_t<T> &c);
    void visit(IborCoupon_t<T> &c);
    void visit(CappedFlooredIborCoupon_t<T> &c);
    void visit(DigitalIborCoupon_t<T> &c);
    void visit(CmsCoupon_t<T> &c);
    void visit(CappedFlooredCmsCoupon_t<T> &c);
    void visit(DigitalCmsCoupon_t<T> &c);
    void visit(RangeAccrualFloatersCoupon &c);
    void visit(SubPeriodsCoupon &c);
};

template <class T> void PricerSetter<T>::visit(CashFlow_t<T> &) {
    // nothing to do
}

template <class T> void PricerSetter<T>::visit(Coupon_t<T> &) {
    // nothing to do
}

template <class T> void PricerSetter<T>::visit(IborCoupon_t<T> &c) {
    const boost::shared_ptr<IborCouponPricer_t<T> > iborCouponPricer =
        boost::dynamic_pointer_cast<IborCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(iborCouponPricer, "pricer not compatible with Ibor coupon");
    c.setPricer(iborCouponPricer);
}

template <class T> void PricerSetter<T>::visit(DigitalIborCoupon_t<T> &c) {
    const boost::shared_ptr<IborCouponPricer_t<T> > iborCouponPricer =
        boost::dynamic_pointer_cast<IborCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(iborCouponPricer, "pricer not compatible with Ibor coupon");
    c.setPricer(iborCouponPricer);
}

template <class T>
void PricerSetter<T>::visit(CappedFlooredIborCoupon_t<T> &c) {
    const boost::shared_ptr<IborCouponPricer_t<T> > iborCouponPricer =
        boost::dynamic_pointer_cast<IborCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(iborCouponPricer, "pricer not compatible with Ibor coupon");
    c.setPricer(iborCouponPricer);
}

template <class T> void PricerSetter<T>::visit(CmsCoupon_t<T> &c) {
    const boost::shared_ptr<CmsCouponPricer_t<T> > cmsCouponPricer =
        boost::dynamic_pointer_cast<CmsCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(cmsCouponPricer, "pricer not compatible with CMS coupon");
    c.setPricer(cmsCouponPricer);
}

template <class T> void PricerSetter<T>::visit(CappedFlooredCmsCoupon_t<T> &c) {
    const boost::shared_ptr<CmsCouponPricer_t<T> > cmsCouponPricer =
        boost::dynamic_pointer_cast<CmsCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(cmsCouponPricer, "pricer not compatible with CMS coupon");
    c.setPricer(cmsCouponPricer);
}

template <class T> void PricerSetter<T>::visit(DigitalCmsCoupon_t<T> &c) {
    const boost::shared_ptr<CmsCouponPricer_t<T> > cmsCouponPricer =
        boost::dynamic_pointer_cast<CmsCouponPricer_t<T> >(pricer_);
    QL_REQUIRE(cmsCouponPricer, "pricer not compatible with CMS coupon");
    c.setPricer(cmsCouponPricer);
}

template <class T> void PricerSetter<T>::visit(RangeAccrualFloatersCoupon &c) {
    const boost::shared_ptr<RangeAccrualPricer> rangeAccrualPricer =
        boost::dynamic_pointer_cast<RangeAccrualPricer>(pricer_);
    QL_REQUIRE(rangeAccrualPricer,
               "pricer not compatible with range-accrual coupon");
    c.setPricer(rangeAccrualPricer);
}

template <class T> void PricerSetter<T>::visit(SubPeriodsCoupon &c) {
    const boost::shared_ptr<SubPeriodsPricer> subPeriodsPricer =
        boost::dynamic_pointer_cast<SubPeriodsPricer>(pricer_);
    QL_REQUIRE(subPeriodsPricer,
               "pricer not compatible with sub-period coupon");
    c.setPricer(subPeriodsPricer);
}
} // namespace {}

template <template <class> class S, class T>
void setCouponPricer(
    const typename Leg_t<T>::Type &leg, const boost::shared_ptr<S<T> > &pricer,
    typename boost::enable_if<boost::is_base_of<FloatingRateCouponPricer_t<T>,
                                                S<T> > >::type *dummy) {
    PricerSetter<T> setter(pricer);
    for (Size i = 0; i < leg.size(); ++i) {
        leg[i]->accept(setter);
    }
}

template <template <class> class S, class T>
void setCouponPricers(
    const typename Leg_t<T>::Type &leg,
    const std::vector<boost::shared_ptr<S<T> > > &pricers,
    typename boost::enable_if<boost::is_base_of<FloatingRateCouponPricer_t<T>,
                                                S<T> > >::type *dummy) {
    Size nCashFlows = leg.size();
    QL_REQUIRE(nCashFlows > 0, "no cashflows");

    Size nPricers = pricers.size();
    QL_REQUIRE(nCashFlows >= nPricers, "mismatch between leg size ("
                                           << nCashFlows
                                           << ") and number of pricers ("
                                           << nPricers << ")");

    for (Size i = 0; i < nCashFlows; ++i) {
        PricerSetter<T> setter(i < nPricers ? pricers[i]
                                            : pricers[nPricers - 1]);
        leg[i]->accept(setter);
    }
}

} // namespace QuantLib

#endif
