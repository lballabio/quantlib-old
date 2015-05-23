/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
  Copyright (C) 2013 Peter Caspers

  This file is part of QuantLib, a free-software/open-source library
  for financial quantitative analysts and developers - http://quantlib.org/

  QuantLib is free software: you can redistribute it and/or modify it
  under the terms of the QuantLib license.  You should have received a
  copy of the license along with this program; if not, please email
  <quantlib-dev@lists.sf.net>. The license is also available online at
  <http://quantlib.org/license.shtml>.


  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details.
*/

/*! \file cmsreplicationpricer.cpp
  \brief
*/

#include <ql/cashflows/cmsreplicationpricer.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/time/schedule.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/math/solvers1d/brent.hpp>
#include <ql/pricingengines/blackformula.hpp>

#include <boost/make_shared.hpp>
#include <ql/termstructures/volatility/atmsmilesection.hpp>

namespace QuantLib {

    CmsReplicationPricer::CmsReplicationPricer(const Handle<SwaptionVolatilityStructure>& swaptionVol,
                                               const Handle<Quote>& meanReversion,
                                               const Handle<YieldTermStructure>& couponDiscountCurve,
                                               const Settings& settings) : 
        CmsCouponPricer(swaptionVol), meanReversion_(meanReversion), 
        couponDiscountCurve_(couponDiscountCurve), settings_(settings) { 
        
        registerWith(meanReversion_);
        if(!couponDiscountCurve_.empty())
            registerWith(couponDiscountCurve_);
    }

    void CmsReplicationPricer::initialize(const FloatingRateCoupon& coupon){

        coupon_ =  dynamic_cast<const CmsCoupon*>(&coupon);
        QL_REQUIRE(coupon_, "CMS coupon needed");
        gearing_ = coupon_->gearing();
        spread_ = coupon_->spread();
        Time accrualPeriod = coupon_->accrualPeriod();
        QL_REQUIRE(accrualPeriod != 0.0, "null accrual period");

        fixingDate_ = coupon_->fixingDate();
        paymentDate_ = coupon_->date();
        swapIndex_ = coupon_->swapIndex();

        forwardCurve_ = swapIndex_->forwardingTermStructure();
        if(swapIndex_->exogenousDiscount())
            discountCurve_ = swapIndex_->discountingTermStructure();
        else
            discountCurve_ = forwardCurve_;

    
        // if no coupon discount curve is given just use the discounting curve from the swap index.
        // for rate calculation this curve cancels out in the computation, so e.g. the discounting
        // swap engine will produce correct results, even if the couponDiscountCurve is not set here.
        // only the price member function in this class will be dependent on the coupon discount curve.

        if(couponDiscountCurve_.empty())
            couponDiscountCurve_ = discountCurve_;

        today_ = QuantLib::Settings::instance().evaluationDate();

        if(paymentDate_ > today_)
            discount_ = couponDiscountCurve_->discount(paymentDate_);
        else discount_= 1.;

        spreadLegValue_ = spread_ * accrualPeriod * discount_;

        if (fixingDate_ > today_) {

            swapTenor_ = swapIndex_->tenor();
            swap_ = swapIndex_->underlyingSwap(fixingDate_);

            swapRateValue_ = swap_->fairRate();
            annuity_ = 1.0E4 * std::fabs(swap_->fixedLegBPS());

            boost::shared_ptr<SmileSection> sectionTmp =
                swaptionVolatility()->smileSection(fixingDate_, swapTenor_);

            // if the section does not provide an atm level, we enhance it to
            // have one, no need to exit with an exception ...

            if (sectionTmp->atmLevel() == Null<Real>())
                smileSection_ = boost::make_shared<AtmSmileSection>(
                    sectionTmp, swapRateValue_);
            else
                smileSection_ = sectionTmp;

            const Leg& fixedCoupons = swap_->fixedLeg();
            fixedLegPaymentDates_ = std::vector<Date>(fixedCoupons.size());
            fixedLegYearFractions_ = std::vector<Real>(fixedCoupons.size());
            for(Size i=0; i<fixedCoupons.size(); i++) {
                boost::shared_ptr<FixedRateCoupon> coupon = 
                    boost::dynamic_pointer_cast<FixedRateCoupon>(fixedCoupons[i]);
                fixedLegPaymentDates_[i] = coupon->date();
                fixedLegYearFractions_[i] = coupon->accrualPeriod();
            }

            const Leg& floatingCoupons = swap_->floatingLeg();
            floatingLegStartDates_ = floatingLegEndDates_ = floatingLegPaymentDates_ = 
                std::vector<Date>(floatingCoupons.size());
            for(Size i=0; i<floatingCoupons.size(); i++) {
                boost::shared_ptr<IborCoupon> coupon = 
                    boost::dynamic_pointer_cast<IborCoupon>(floatingCoupons[i]);
                floatingLegStartDates_[i] = coupon->accrualStartDate();
                floatingLegEndDates_[i] = coupon->accrualEndDate();
                floatingLegPaymentDates_[i] = coupon->date();
            }
            
        }
    }

    Real CmsReplicationPricer::hullWhiteScenario(Real dt, Real h) const {
    
        Real e;

        if(close(meanReversion_->value(),0.0))
            e = dt;
        else
            e = (1.0 - std::exp( -dt * meanReversion_->value() ) ) / meanReversion_->value();

        return std::exp( -h * e );

    }

    Real CmsReplicationPricer::annuity(Real h) const {
    
        Real annuity=0.0;
        for(Size i=0; i<fixedLegYearFractions_.size(); i++) {
            Real dt = discountCurve_->dayCounter().yearFraction(fixingDate_,fixedLegPaymentDates_[i]);
            annuity += fixedLegYearFractions_[i] * discountCurve_->discount(fixedLegPaymentDates_[i])
                / discountCurve_->discount(fixingDate_) * hullWhiteScenario(dt,h);
        }

        return annuity;

    }

    Real CmsReplicationPricer::floatingLegNpv(Real h) const {

        if(settings_.simplifiedFloatingLeg_) {
            Real dt1 = discountCurve_->dayCounter().yearFraction(fixingDate_,floatingLegStartDates_.front());
            Real dt2 = discountCurve_->dayCounter().yearFraction(fixingDate_,floatingLegPaymentDates_.back());
            return (discountCurve_->discount(floatingLegStartDates_.front()) * hullWhiteScenario(dt1,h) -
                    discountCurve_->discount(floatingLegPaymentDates_.back()) * hullWhiteScenario(dt2,h)) /
                discountCurve_->discount(fixingDate_);
        }
        else {
            Real npv=0.0;
            for(Size i=0; i<floatingLegStartDates_.size();i++) {
                Real dt1 = forwardCurve_->dayCounter().yearFraction(fixingDate_,floatingLegStartDates_[i]);
                Real dt2 = forwardCurve_->dayCounter().yearFraction(fixingDate_,floatingLegEndDates_[i]);
                Real dt3 = discountCurve_->dayCounter().yearFraction(fixingDate_,floatingLegPaymentDates_[i]);
                // we use that the day counter of the floating leg is equal to that of the float index always for
                // swapIndex underlying swaps
                npv += (forwardCurve_->discount(floatingLegStartDates_[i]) *
                            hullWhiteScenario(dt1, h) /
                            (forwardCurve_->discount(floatingLegEndDates_[i]) *
                             hullWhiteScenario(dt2, h)) -
                        1.0) *
                       discountCurve_->discount(floatingLegPaymentDates_[i]) /
                       discountCurve_->discount(fixingDate_) *
                       hullWhiteScenario(dt3, h);
            }
            return npv;
        }

    }

    Real CmsReplicationPricer::swapRate(Real h) const {

        return floatingLegNpv(h) / annuity(h);

    }

    Real CmsReplicationPricer::h(Real rate) const {

        Real a = -10.0, b = 10.0;
        HHelper h(this,rate);
        Brent solver;

        Real c;
    
        try {
            c = solver.solve(h,1.0E-6,0.0,a,b);
        } catch(QuantLib::Error e) {
            QL_FAIL("can not imply h from rate (" << rate << "):" << e.what()); 
        }

        return c;

    }


    Real CmsReplicationPricer::strikeFromVegaRatio(Real ratio, Option::Type optionType, Real referenceStrike) const {
    
        Real a,b,min,max,k;
        if(optionType == Option::Call) {
            a = swapRateValue_;
            min = referenceStrike;
            b = max = k = std::min(smileSection_->maxStrike(), settings_.upperRateBound_);
        }
        else {
            a = min = k = std::max(smileSection_->minStrike(), settings_.lowerRateBound_);
            b = swapRateValue_;
            max = referenceStrike;
        }

        VegaRatioHelper h(&*smileSection_,smileSection_->vega(swapRateValue_)*ratio);
        Brent solver;

        try {
            k = solver.solve(h,1.0E-5,(a+b)/2.0,a,b);
        } catch(...) {
            // use default value set above      
        }

        return std::min(std::max(k,min),max);

    }


    Real CmsReplicationPricer::strikeFromPrice(Real price, Option::Type optionType, Real referenceStrike) const {

        Real a,b,min,max,k;
        if(optionType == Option::Call) {
            a = swapRateValue_;
            min = referenceStrike;
            b = max = k = std::min(smileSection_->maxStrike(), settings_.upperRateBound_);
        }
        else {
            a = min = k = std::max(smileSection_->minStrike(), settings_.lowerRateBound_);
            b = swapRateValue_;
            max = referenceStrike;
        }

        PriceHelper h(&*smileSection_, optionType, price);
        Brent solver;

        try {
            k = solver.solve(h,1.0E-5,swapRateValue_,a,b);
        } catch(...) {
            // use default value set above      
        }

        return std::min(std::max(k,min),max);

    }

    Real CmsReplicationPricer::optionletPrice(Option::Type optionType, Real strike) const {
    
        Real phi = optionType == Option::Call ? 1.0 : -1.0;

        if(optionType == Option::Call && strike >= settings_.upperRateBound_) return 0.0;
        if(optionType == Option::Put && strike <= settings_.lowerRateBound_) return 0.0;
    
        // determine vector of hs corresponding to scenarios;

        std::vector<Real> hs;
        switch(settings_.strategy_) {
    
        case Settings::DiscreteStrikeSpreads : {
            for(Size i=0;i<settings_.n_;i++) {
                Real k = strike + phi*settings_.discreteStrikeSpreads_[i];
                if(k>=smileSection_->minStrike() && k<=smileSection_->maxStrike())
                    hs.push_back( h(k) );
            }
            break;
        }

        case Settings::RateBound : {
            Real h1 = h(strike);
            Real h2 = h(optionType == Option::Call ? settings_.upperRateBound_ : settings_.lowerRateBound_);
            for(Size i=0;i<settings_.n_;i++) {
                hs.push_back(h1+((Real)(i+1)/(settings_.n_-1))*(h2-h1));
            }
            break;
        }

        case Settings::VegaRatio : {
            // strikeFromVegaRatio ensures that returned strike is on the expected side of strike
            Real effectiveBound = optionType == Option::Call ?
                std::min(settings_.upperRateBound_ , 
                         strikeFromVegaRatio(settings_.vegaRatio_,optionType,strike)) :
                std::max(settings_.lowerRateBound_, 
                         strikeFromVegaRatio(settings_.vegaRatio_,optionType,strike));
            if(close(fabs(effectiveBound-strike),0.0)) return 0.0;
            Real h1 = h(strike);
            Real h2 = h(effectiveBound);
            for(Size i=0;i<settings_.n_;i++) {
                hs.push_back(h1+((Real)(i+1)/(settings_.n_-1))*(h2-h1));
            }
            break;
        }

        case Settings::PriceThreshold : {
            // strikeFromPrice ensures that returned strike is on the expected side of strike
            Real effectiveBound = optionType == Option::Call ?
                std::min(settings_.upperRateBound_,
                         strikeFromPrice(settings_.priceThreshold_,optionType,strike)) :
                std::max(settings_.lowerRateBound_,
                         strikeFromPrice(settings_.priceThreshold_,optionType,strike));
            if(close(fabs(effectiveBound-strike),0.0)) return 0.0;
            Real h1 = h(strike);
            Real h2 = h(effectiveBound);
            for(Size i=0;i<settings_.n_;i++) {
                hs.push_back(h1+((Real)(i+1)/(settings_.n_-1))*(h2-h1));
            }
            break;
        }

        default:
            QL_FAIL("Unknown strategy (" << settings_.strategy_ << ")");

        }

        // compute the hedge basket and price it
        std::vector<Real> weights, strikes;

        Real rate = strike, lastRate;
        Real price = QL_MAX_REAL, lastPrice, basketPrice = 0.0;

        for(Size i=0;i<hs.size();i++) {
            lastPrice = price;
            lastRate = rate;
            rate = swapRate(hs[i]);
            strikes.push_back(lastRate);
            Real ann = annuity(hs[i]);
            Real npv=0.0;
            for(Size j=0;j<i;j++) { // this can be done _much_ more efficiently ...
                npv += weights[j] * phi * (rate - strikes[j]) * ann;
            }
            Real dt = discountCurve_->dayCounter().yearFraction(fixingDate_,paymentDate_);
            weights.push_back( ( phi * discountCurve_->discount(paymentDate_) / 
                                 discountCurve_->discount(fixingDate_) *
                                 hullWhiteScenario(dt,hs[i]) * (rate - strike) - npv ) /
                               ( phi * ann * (rate - lastRate) ) );
            price = blackFormula(optionType,lastRate,swapRateValue_,
                                 std::sqrt(smileSection_->variance(lastRate)),annuity_);
            if(settings_.enforceMonotonicPrices_)
                price = std::min( lastPrice, price );
            basketPrice += weights.back()*price;
        }

        // note that fixedLegBPS() is computed w.r.t. discountCurve_, but the coupon discount curve
        // may be different. We have to take this into account here.

        basketPrice *=  coupon_->accrualPeriod() * discount_ / discountCurve_->discount(paymentDate_);

        return basketPrice;

    }

    Real CmsReplicationPricer::meanReversion() const { return meanReversion_->value();}

    Rate CmsReplicationPricer::swapletRate() const {
        return swapletPrice()/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::capletPrice(Rate effectiveCap) const {
        // caplet is equivalent to call option on fixing
        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs =
                std::max(coupon_->swapIndex()->fixing(fixingDate_)-effectiveCap, 0.);
            Rate price = (gearing_*Rs)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real capletPrice = optionletPrice(Option::Call, effectiveCap);
            return gearing_ * capletPrice;
        }
    }

    Rate CmsReplicationPricer::capletRate(Rate effectiveCap) const {
        return capletPrice(effectiveCap)/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::floorletPrice(Rate effectiveFloor) const {
        // floorlet is equivalent to put option on fixing
        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs =
                std::max(effectiveFloor-coupon_->swapIndex()->fixing(fixingDate_),0.);
            Rate price = (gearing_*Rs)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real floorletPrice = optionletPrice(Option::Put, effectiveFloor);
            return gearing_ * floorletPrice;
        }
    }

    Rate CmsReplicationPricer::floorletRate(Rate effectiveFloor) const {
        return floorletPrice(effectiveFloor)/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::swapletPrice() const {

        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs = coupon_->swapIndex()->fixing(fixingDate_);
            Rate price = (gearing_*Rs + spread_)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real atmCapletPrice = optionletPrice(Option::Call, swapRateValue_);
            Real atmFloorletPrice = optionletPrice(Option::Put, swapRateValue_);
            return gearing_ *(coupon_->accrualPeriod()* discount_ * swapRateValue_
                              + atmCapletPrice - atmFloorletPrice)
                + spreadLegValue_;
        }
    }

}
