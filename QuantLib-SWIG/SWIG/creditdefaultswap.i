/*
 Copyright (C) 2008, 2009 StatPro Italia srl

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

#ifndef quantlib_credit_default_swap_i
#define quantlib_credit_default_swap_i

%include instruments.i
%include credit.i
%include termstructures.i
%include null.i

%{
using QuantLib::CreditDefaultSwap;
using QuantLib::MidPointCdsEngine;

typedef boost::shared_ptr<Instrument> CreditDefaultSwapPtr;
typedef boost::shared_ptr<PricingEngine> MidPointCdsEnginePtr;
%}

%rename(CreditDefaultSwap) CreditDefaultSwapPtr;
class CreditDefaultSwapPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        CreditDefaultSwapPtr(Protection::Side side,
                             Real notional,
                             Rate spread,
                             const Schedule& schedule,
                             BusinessDayConvention paymentConvention,
                             const DayCounter& dayCounter,
                             bool settlesAccrual = true,
                             bool paysAtDefaultTime = true) {
            return new CreditDefaultSwapPtr(
                    new CreditDefaultSwap(side, notional, spread, schedule,
                                          paymentConvention, dayCounter,
                                          settlesAccrual, paysAtDefaultTime));
        }
        CreditDefaultSwapPtr(Protection::Side side,
                             Real notional,
                             Rate upfront,
                             Rate spread,
                             const Schedule& schedule,
                             BusinessDayConvention paymentConvention,
                             const DayCounter& dayCounter,
                             bool settlesAccrual = true,
                             bool paysAtDefaultTime = true) {
            return new CreditDefaultSwapPtr(
                    new CreditDefaultSwap(side, notional, upfront, spread,
                                          schedule, paymentConvention,
                                          dayCounter, settlesAccrual,
                                          paysAtDefaultTime));
        }
        Protection::Side side() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->side();
        }
        Real notional() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->notional();
        }
        Rate runningSpread() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->runningSpread();
        }
        doubleOrNull upfront() const {
            boost::optional<Rate> result =
                boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->upfront();
            if (result)
                return *result;
            else
                return Null<double>();
        }
        bool settlesAccrual() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->settlesAccrual();
        }
        bool paysAtDefaultTime() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->paysAtDefaultTime();
        }
        Rate fairSpread() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->fairSpread();
        }
        Rate fairUpfront() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->fairUpfront();
        }
        Real couponLegBPS() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->couponLegBPS();
        }
        Real couponLegNPV() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->couponLegNPV();
        }
        Real defaultLegNPV() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->defaultLegNPV();
        }
        Real upfrontBPS() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->upfrontBPS();
        }
        Real upfrontNPV() const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->upfrontNPV();
        }
        Rate impliedHazardRate(Real targetNPV,
                               const Handle<YieldTermStructure>& discountCurve,
                               const DayCounter& dayCounter,
                               Real recoveryRate = 0.4,
                               Real accuracy = 1.0e-6) const {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->impliedHazardRate(targetNPV, discountCurve, dayCounter,
                                    recoveryRate, accuracy);
        }
        std::vector<boost::shared_ptr<CashFlow> > coupons() {
            return boost::dynamic_pointer_cast<CreditDefaultSwap>(*self)
                ->coupons();
        }
    }
};


%rename(MidPointCdsEngine) MidPointCdsEnginePtr;
class MidPointCdsEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        MidPointCdsEnginePtr(
                   const Handle<DefaultProbabilityTermStructure>& probability,
                   Real recoveryRate,
                   const Handle<YieldTermStructure>& discountCurve) {
            return new MidPointCdsEnginePtr(
                              new MidPointCdsEngine(probability, recoveryRate,
                                                    discountCurve));
        }
    }
};


#endif
