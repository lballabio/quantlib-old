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
#include <ql/cashflows/couponpricerbase.hpp>
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

namespace QuantLib {

using std::max;
using std::sqrt;

using boost::dynamic_pointer_cast;

//! base pricer for capped/floored Ibor coupons
template <class T>
class IborCouponPricer_t : public FloatingRateCouponPricer_t<T> {
  public:
    IborCouponPricer_t(const Handle<OptionletVolatilityStructure> &v =
                           Handle<OptionletVolatilityStructure>())
        : capletVol_(v) {
        this->registerWith(capletVol_);
    }

    Handle<OptionletVolatilityStructure> capletVolatility() const {
        return capletVol_;
    }
    void setCapletVolatility(const Handle<OptionletVolatilityStructure> &v =
                                 Handle<OptionletVolatilityStructure>()) {
        this->unregisterWith(capletVol_);
        capletVol_ = v;
        this->registerWith(capletVol_);
        this->update();
    }

  private:
    Handle<OptionletVolatilityStructure> capletVol_;
};

typedef IborCouponPricer_t<Real> IborCouponPricer;

//! Black-formula pricer for capped/floored Ibor coupons
template <class T>
class BlackIborCouponPricer_t : public IborCouponPricer_t<T> {
  public:
    BlackIborCouponPricer_t(const Handle<OptionletVolatilityStructure> &v =
                                Handle<OptionletVolatilityStructure>())
        : IborCouponPricer_t<T>(v){};
    virtual void initialize(const FloatingRateCoupon_t<T> &coupon);
    /* */
    T swapletPrice() const;
    T swapletRate() const;
    T capletPrice(T effectiveCap) const;
    T capletRate(T effectiveCap) const;
    T floorletPrice(T effectiveFloor) const;
    T floorletRate(T effectiveFloor) const;

  protected:
    T optionletPrice(Option::Type optionType, T effStrike) const;

    virtual T adjustedFixing(T fixing = Null<Rate>()) const;

    T gearing_;
    T spread_;
    Time accrualPeriod_;
    boost::shared_ptr<IborIndex_t<T> > index_;
    T discount_;
    T spreadLegValue_;

    const FloatingRateCoupon_t<T> *coupon_;
};

typedef BlackIborCouponPricer_t<Real> BlackIborCouponPricer;

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

template <class T>
void setCouponPricer(const typename Leg_t<T>::Type &leg,
                     const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &);

template <class T>
void setCouponPricers(
    const typename Leg_t<T>::Type &leg,
    const std::vector<boost::shared_ptr<FloatingRateCouponPricer_t<T> > > &);

// inline

template <class T> inline T BlackIborCouponPricer_t<T>::swapletPrice() const {
    // past or future fixing is managed in InterestRateIndex::fixing()

    T swapletPrice = adjustedFixing() * accrualPeriod_ * discount_;
    return gearing_ * swapletPrice + spreadLegValue_;
}

template <class T> inline T BlackIborCouponPricer_t<T>::swapletRate() const {
    return swapletPrice() / (accrualPeriod_ * discount_);
}

template <class T>
inline T BlackIborCouponPricer_t<T>::capletPrice(T effectiveCap) const {
    T capletPrice = optionletPrice(Option::Call, effectiveCap);
    return gearing_ * capletPrice;
}

template <class T>
inline T BlackIborCouponPricer_t<T>::capletRate(T effectiveCap) const {
    return capletPrice(effectiveCap) / (accrualPeriod_ * discount_);
}

template <class T>
inline T BlackIborCouponPricer_t<T>::floorletPrice(T effectiveFloor) const {
    T floorletPrice = optionletPrice(Option::Put, effectiveFloor);
    return gearing_ * floorletPrice;
}

template <class T>
inline T BlackIborCouponPricer_t<T>::floorletRate(T effectiveFloor) const {
    return floorletPrice(effectiveFloor) / (accrualPeriod_ * discount_);
}

// implementation

//===========================================================================//
//                              BlackIborCouponPricer                        //
//===========================================================================//

template <class T>
void BlackIborCouponPricer_t<T>::initialize(
    const FloatingRateCoupon_t<T> &coupon) {

    gearing_ = coupon.gearing();
    spread_ = coupon.spread();
    accrualPeriod_ = coupon.accrualPeriod();
    QL_REQUIRE(accrualPeriod_ != 0.0, "null accrual period");

    index_ = dynamic_pointer_cast<IborIndex>(coupon.index());
    if (!index_) {
        // check if the coupon was right
        const IborCoupon_t<T> *c =
            dynamic_cast<const IborCoupon_t<T> *>(&coupon);
        QL_REQUIRE(c, "IborCoupon required");
        // coupon was right, index is not
        QL_FAIL("IborIndex required");
    }
    Handle<YieldTermStructure_t<T> > rateCurve =
        index_->forwardingTermStructure();

    Date paymentDate = coupon.date();
    if (paymentDate > rateCurve->referenceDate())
        discount_ = rateCurve->discount(paymentDate);
    else
        discount_ = 1.0;

    spreadLegValue_ = spread_ * accrualPeriod_ * discount_;

    coupon_ = &coupon;
}

template <class T>
T BlackIborCouponPricer_t<T>::optionletPrice(Option::Type optionType,
                                             T effStrike) const {
    Date fixingDate = coupon_->fixingDate();
    if (fixingDate <= Settings::instance().evaluationDate()) {
        // the amount is determined
        T a, b;
        if (optionType == Option::Call) {
            a = coupon_->indexFixing();
            b = effStrike;
        } else {
            a = effStrike;
            b = coupon_->indexFixing();
        }
        return max(a - b, 0.0) * accrualPeriod_ * discount_;
    } else {
        // not yet determined, use Black model
        QL_REQUIRE(!this->capletVolatility().empty(),
                   "missing optionlet volatility");
        T stdDev = sqrt(
            this->capletVolatility()->blackVariance(fixingDate, effStrike));
        T fixing =
            blackFormula<T>(optionType, effStrike, adjustedFixing(), stdDev);
        return fixing * accrualPeriod_ * discount_;
    }
}

template <class T>
T BlackIborCouponPricer_t<T>::adjustedFixing(T fixing) const {

    if (fixing == Null<Rate>())
        fixing = coupon_->indexFixing();

    if (!coupon_->isInArrears())
        return fixing;

    QL_REQUIRE(!this->capletVolatility().empty(),
               "missing optionlet volatility");
    Date d1 = coupon_->fixingDate();
    Date referenceDate = this->capletVolatility()->referenceDate();
    if (d1 <= referenceDate)
        return fixing;

    // see Hull, 4th ed., page 550
    Date d2 = index_->valueDate(d1);
    Date d3 = index_->maturityDate(d2);
    Time tau = index_->dayCounter().yearFraction(d2, d3);
    T variance = this->capletVolatility()->blackVariance(d1, fixing);
    T adjustement = fixing * fixing * variance * tau / (1.0 + fixing * tau);
    return fixing + adjustement;
}

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

template <class T = Real>
void setCouponPricer(
    const typename Leg_t<T>::Type &leg,
    const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &pricer) {
    PricerSetter<T> setter(pricer);
    for (Size i = 0; i < leg.size(); ++i) {
        leg[i]->accept(setter);
    }
}

template <class T = Real>
void setCouponPricers(
    const typename Leg_t<T>::Type &leg,
    const std::vector<boost::shared_ptr<FloatingRateCouponPricer_t<T> > > &
        pricers) {
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
