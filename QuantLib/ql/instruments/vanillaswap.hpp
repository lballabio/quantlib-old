/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2006, 2008 Ferdinando Ametrano
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

/*! \file vanillaswap.hpp
    \brief Simple fixed-rate vs Libor swap
*/

#ifndef quantlib_vanilla_swap_hpp
#define quantlib_vanilla_swap_hpp

#include <ql/instruments/swap.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/time/schedule.hpp>
#include <ql/indexes/iborindex.hpp>
#include <boost/optional.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/couponpricerbase.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

//! Plain-vanilla swap: fix vs floating leg
/*! \ingroup instruments

    If no payment convention is passed, the convention of the
    floating-rate schedule is used.

    \warning if <tt>Settings::includeReferenceDateCashFlows()</tt>
             is set to <tt>true</tt>, payments occurring at the
             settlement date of the swap might be included in the
             NPV and therefore affect the fair-rate and
             fair-spread calculation. This might not be what you
             want.

    \test
    - the correctness of the returned value is tested by checking
      that the price of a swap paying the fair fixed rate is null.
    - the correctness of the returned value is tested by checking
      that the price of a swap receiving the fair floating-rate
      spread is null.
    - the correctness of the returned value is tested by checking
      that the price of a swap decreases with the paid fixed rate.
    - the correctness of the returned value is tested by checking
      that the price of a swap increases with the received
      floating-rate spread.
    - the correctness of the returned value is tested by checking
      it against a known good value.
*/
template <class T> class VanillaSwap_t : public Swap_t<T> {
  public:
    enum Type { Receiver = -1, Payer = 1 };
    class arguments;
    class results;
    class engine;
    VanillaSwap_t(
        Type type, T nominal, const Schedule &fixedSchedule, T fixedRate,
        const DayCounter &fixedDayCount, const Schedule &floatSchedule,
        const boost::shared_ptr<IborIndex_t<T> > &iborIndex, T spread,
        const DayCounter &floatingDayCount,
        boost::optional<BusinessDayConvention> paymentConvention = boost::none);
    //! \name Inspectors
    //@{
    Type type() const;
    T nominal() const;

    const Schedule &fixedSchedule() const;
    T fixedRate() const;
    const DayCounter &fixedDayCount() const;

    const Schedule &floatingSchedule() const;
    const boost::shared_ptr<IborIndex_t<T> > &iborIndex() const;
    T spread() const;
    const DayCounter &floatingDayCount() const;

    BusinessDayConvention paymentConvention() const;

    const typename Leg_t<T>::Type &fixedLeg() const;
    const typename Leg_t<T>::Type &floatingLeg() const;
    //@}

    //! \name Results
    //@{
    T fixedLegBPS() const;
    T fixedLegNPV() const;
    T fairRate() const;

    T floatingLegBPS() const;
    T floatingLegNPV() const;
    T fairSpread() const;
    //@}
    // other
    void setupArguments(PricingEngine::arguments *args) const;
    void fetchResults(const PricingEngine::results *) const;

  private:
    void setupExpired() const;
    Type type_;
    T nominal_;
    Schedule fixedSchedule_;
    T fixedRate_;
    DayCounter fixedDayCount_;
    Schedule floatingSchedule_;
    boost::shared_ptr<IborIndex_t<T> > iborIndex_;
    T spread_;
    DayCounter floatingDayCount_;
    BusinessDayConvention paymentConvention_;
    // results
    mutable T fairRate_;
    mutable T fairSpread_;
};

typedef VanillaSwap_t<Real> VanillaSwap;

//! %Arguments for simple swap calculation
template <class T>
class VanillaSwap_t<T>::arguments : public Swap_t<T>::arguments {
  public:
    arguments() : type(Receiver), nominal(Null<Real>()) {}
    Type type;
    T nominal;

    std::vector<Date> fixedResetDates;
    std::vector<Date> fixedPayDates;
    std::vector<Time> floatingAccrualTimes;
    std::vector<Date> floatingResetDates;
    std::vector<Date> floatingFixingDates;
    std::vector<Date> floatingPayDates;

    std::vector<T> fixedCoupons;
    std::vector<T> floatingSpreads;
    std::vector<T> floatingCoupons;
    void validate() const;
};

//! %Results from simple swap calculation
template <class T> class VanillaSwap_t<T>::results : public Swap_t<T>::results {
  public:
    T fairRate;
    T fairSpread;
    void reset();
};

template <class T>
class VanillaSwap_t<T>::engine
    : public GenericEngine<VanillaSwap_t<T>::arguments,
                           VanillaSwap_t<T>::results> {};

// inline definitions

template <class T>
inline typename VanillaSwap_t<T>::Type VanillaSwap_t<T>::type() const {
    return type_;
}

template <class T> inline T VanillaSwap_t<T>::nominal() const {
    return nominal_;
}

template <class T>
inline const Schedule &VanillaSwap_t<T>::fixedSchedule() const {
    return fixedSchedule_;
}

template <class T> inline T VanillaSwap_t<T>::fixedRate() const {
    return fixedRate_;
}

template <class T>
inline const DayCounter &VanillaSwap_t<T>::fixedDayCount() const {
    return fixedDayCount_;
}

template <class T>
inline const Schedule &VanillaSwap_t<T>::floatingSchedule() const {
    return floatingSchedule_;
}

template <class T>
inline const boost::shared_ptr<IborIndex_t<T> > &
VanillaSwap_t<T>::iborIndex() const {
    return iborIndex_;
}

template <class T> inline T VanillaSwap_t<T>::spread() const { return spread_; }

template <class T>
inline const DayCounter &VanillaSwap_t<T>::floatingDayCount() const {
    return floatingDayCount_;
}

template <class T>
inline BusinessDayConvention VanillaSwap_t<T>::paymentConvention() const {
    return paymentConvention_;
}

template <class T>
inline const typename Leg_t<T>::Type &VanillaSwap_t<T>::fixedLeg() const {
    return this->legs_[0];
}

template <class T>
inline const typename Leg_t<T>::Type &VanillaSwap_t<T>::floatingLeg() const {
    return this->legs_[1];
}

template <class T>
std::ostream &operator<<(std::ostream &out, typename VanillaSwap_t<T>::Type t);

// implementation

template <class T>
VanillaSwap_t<T>::VanillaSwap_t(
    Type type, T nominal, const Schedule &fixedSchedule, T fixedRate,
    const DayCounter &fixedDayCount, const Schedule &floatSchedule,
    const boost::shared_ptr<IborIndex_t<T> > &iborIndex, T spread,
    const DayCounter &floatingDayCount,
    boost::optional<BusinessDayConvention> paymentConvention)
    : Swap_t<T>(2), type_(type), nominal_(nominal),
      fixedSchedule_(fixedSchedule), fixedRate_(fixedRate),
      fixedDayCount_(fixedDayCount), floatingSchedule_(floatSchedule),
      iborIndex_(iborIndex), spread_(spread),
      floatingDayCount_(floatingDayCount) {

    if (paymentConvention)
        paymentConvention_ = *paymentConvention;
    else
        paymentConvention_ = floatingSchedule_.businessDayConvention();

    this->legs_[0] = FixedRateLeg_t<T>(fixedSchedule_)
                         .withNotionals(nominal_)
                         .withCouponRates(fixedRate_, fixedDayCount_)
                         .withPaymentAdjustment(paymentConvention_);

    this->legs_[1] = IborLeg_t<T>(floatingSchedule_, iborIndex_)
                         .withNotionals(nominal_)
                         .withPaymentDayCounter(floatingDayCount_)
                         .withPaymentAdjustment(paymentConvention_)
                         .withSpreads(spread_);

    for (typename Leg_t<T>::Type::const_iterator i = this->legs_[1].begin();
         i < this->legs_[1].end(); ++i)
        this->registerWith(*i);

    switch (type_) {
    case Payer:
        this->payer_[0] = -1.0;
        this->payer_[1] = +1.0;
        break;
    case Receiver:
        this->payer_[0] = +1.0;
        this->payer_[1] = -1.0;
        break;
    default:
        QL_FAIL("Unknown vanilla-swap type");
    }
}

template <class T>
void VanillaSwap_t<T>::setupArguments(PricingEngine::arguments *args) const {

    Swap_t<T>::setupArguments(args);

    VanillaSwap_t<T>::arguments *arguments =
        dynamic_cast<VanillaSwap_t<T>::arguments *>(args);

    if (!arguments) // it's a swap engine...
        return;

    arguments->type = type_;
    arguments->nominal = nominal_;

    const typename Leg_t<T>::Type &fixedCoupons = fixedLeg();

    arguments->fixedResetDates = arguments->fixedPayDates =
        std::vector<Date>(fixedCoupons.size());
    arguments->fixedCoupons = std::vector<T>(fixedCoupons.size());

    for (Size i = 0; i < fixedCoupons.size(); ++i) {
        boost::shared_ptr<FixedRateCoupon_t<T> > coupon =
            boost::dynamic_pointer_cast<FixedRateCoupon_t<T> >(fixedCoupons[i]);

        arguments->fixedPayDates[i] = coupon->date();
        arguments->fixedResetDates[i] = coupon->accrualStartDate();
        arguments->fixedCoupons[i] = coupon->amount();
    }

    const typename Leg_t<T>::Type &floatingCoupons = floatingLeg();

    arguments->floatingResetDates = arguments->floatingPayDates =
        arguments->floatingFixingDates =
            std::vector<Date>(floatingCoupons.size());
    arguments->floatingAccrualTimes = std::vector<Time>(floatingCoupons.size());
    arguments->floatingSpreads = std::vector<T>(floatingCoupons.size());
    arguments->floatingCoupons = std::vector<T>(floatingCoupons.size());
    for (Size i = 0; i < floatingCoupons.size(); ++i) {
        boost::shared_ptr<IborCoupon_t<T> > coupon =
            boost::dynamic_pointer_cast<IborCoupon_t<T> >(floatingCoupons[i]);

        arguments->floatingResetDates[i] = coupon->accrualStartDate();
        arguments->floatingPayDates[i] = coupon->date();

        arguments->floatingFixingDates[i] = coupon->fixingDate();
        arguments->floatingAccrualTimes[i] = coupon->accrualPeriod();
        arguments->floatingSpreads[i] = coupon->spread();
        try {
            arguments->floatingCoupons[i] = coupon->amount();
        } catch (Error &) {
            arguments->floatingCoupons[i] = Null<Real>();
        }
    }
}

template <class T> T VanillaSwap_t<T>::fairRate() const {
    this->calculate();
    QL_REQUIRE(fairRate_ != Null<Rate>(), "result not available");
    return fairRate_;
}

template <class T> T VanillaSwap_t<T>::fairSpread() const {
    this->calculate();
    QL_REQUIRE(fairSpread_ != Null<Spread>(), "result not available");
    return fairSpread_;
}

template <class T> T VanillaSwap_t<T>::fixedLegBPS() const {
    this->calculate();
    QL_REQUIRE(this->legBPS_[0] != Null<Real>(), "result not available");
    return this->legBPS_[0];
}

template <class T> T VanillaSwap_t<T>::floatingLegBPS() const {
    this->calculate();
    QL_REQUIRE(this->legBPS_[1] != Null<Real>(), "result not available");
    return this->legBPS_[1];
}

template <class T> T VanillaSwap_t<T>::fixedLegNPV() const {
    this->calculate();
    QL_REQUIRE(this->legNPV_[0] != Null<Real>(), "result not available");
    return this->legNPV_[0];
}

template <class T> T VanillaSwap_t<T>::floatingLegNPV() const {
    this->calculate();
    QL_REQUIRE(this->legNPV_[1] != Null<Real>(), "result not available");
    return this->legNPV_[1];
}

template <class T> void VanillaSwap_t<T>::setupExpired() const {
    Swap_t<T>::setupExpired();
    this->legBPS_[0] = this->legBPS_[1] = 0.0;
    fairRate_ = Null<Real>();
    fairSpread_ = Null<Real>();
}

template <class T>
void VanillaSwap_t<T>::fetchResults(const PricingEngine::results *r) const {
    static const Spread basisPoint = 1.0e-4;

    Swap_t<T>::fetchResults(r);

    const VanillaSwap_t<T>::results *results =
        dynamic_cast<const VanillaSwap_t<T>::results *>(r);
    if (results) { // might be a swap engine, so no error is thrown
        fairRate_ = results->fairRate;
        fairSpread_ = results->fairSpread;
    } else {
        fairRate_ = Null<Rate>();
        fairSpread_ = Null<Spread>();
    }

    if (fairRate_ == Null<Rate>()) {
        // calculate it from other results
        if (this->legBPS_[0] != Null<Real>())
            fairRate_ =
                fixedRate_ - this->NPV_ / (this->legBPS_[0] / basisPoint);
    }
    if (fairSpread_ == Null<Spread>()) {
        // ditto
        if (this->legBPS_[1] != Null<Real>())
            fairSpread_ =
                spread_ - this->NPV_ / (this->legBPS_[1] / basisPoint);
    }
}

template <class T> void VanillaSwap_t<T>::arguments::validate() const {
    Swap_t<T>::arguments::validate();
    QL_REQUIRE(nominal != Null<Real>(), "nominal null or not set");
    QL_REQUIRE(fixedResetDates.size() == fixedPayDates.size(),
               "number of fixed start dates different from "
               "number of fixed payment dates");
    QL_REQUIRE(fixedPayDates.size() == fixedCoupons.size(),
               "number of fixed payment dates different from "
               "number of fixed coupon amounts");
    QL_REQUIRE(floatingResetDates.size() == floatingPayDates.size(),
               "number of floating start dates different from "
               "number of floating payment dates");
    QL_REQUIRE(floatingFixingDates.size() == floatingPayDates.size(),
               "number of floating fixing dates different from "
               "number of floating payment dates");
    QL_REQUIRE(floatingAccrualTimes.size() == floatingPayDates.size(),
               "number of floating accrual Times different from "
               "number of floating payment dates");
    QL_REQUIRE(floatingSpreads.size() == floatingPayDates.size(),
               "number of floating spreads different from "
               "number of floating payment dates");
    QL_REQUIRE(floatingPayDates.size() == floatingCoupons.size(),
               "number of floating payment dates different from "
               "number of floating coupon amounts");
}

template <class T> void VanillaSwap_t<T>::results::reset() {
    Swap_t<T>::results::reset();
    fairRate = Null<Rate>();
    fairSpread = Null<Spread>();
}

template <class T>
std::ostream &operator<<(std::ostream &out, typename VanillaSwap_t<T>::Type t) {
    switch (t) {
    case VanillaSwap_t<T>::Payer:
        return out << "Payer";
    case VanillaSwap_t<T>::Receiver:
        return out << "Receiver";
    default:
        QL_FAIL("unknown VanillaSwap Type (" << Integer(t) << ")");
    }
}
}

#endif
