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
  or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details. */

/*! \file lineartsrpricer.hpp
  \brief linear terminal swap rate model for cms coupon pricing
*/

#ifndef quantlib_lineartsr_pricer_hpp
#define quantlib_lineartsr_pricer_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/math/integrals/integral.hpp>

namespace QuantLib {

    class CmsCoupon;
    class YieldTermStructure;

    //! CMS-coupon pricer
    /*! Prices a cms coupon using a linear swap rate model where the slope
        parameter is linked to a gaussian short rate model.
    */

    class LinearTsrPricer : public CmsCouponPricer {

      public:
        struct Settings {

            enum Strategy {
                RateBound,
                VegaRatio,
                PriceThreshold
            };

            Settings()
                : strategy_(RateBound), vegaRatio_(0.01),
                  priceThreshold_(1.0E-8), lowerRateBound_(0.0001),
                  upperRateBound_(2.0000), cashSettledSwaptions_(false) {}

            // hedge with given lower and upper bound for swap rate, e.g. 0.0001
            // and 2.0000
            Settings &withRateBound(const Real lowerRateBound,
                                    const Real upperRateBound) {
                strategy_ = RateBound;
                lowerRateBound_ = lowerRateBound;
                upperRateBound_ = upperRateBound;
                return *this;
            }

            // hedge with an lower / upper rate bound such that the swaption
            // vega is a given ratio of the
            // atm vega but not outside the given bounds.
            Settings &withVegaRatio(const Real vegaRatio,
                                    const Real lowerRateBound = 0.0001,
                                    const Real upperRateBound = 2.0000) {
                strategy_ = VegaRatio;
                vegaRatio_ = vegaRatio;
                lowerRateBound_ = lowerRateBound;
                upperRateBound_ = upperRateBound;
                return *this;
            }

            // hedge with an upper rate bound such that a (undeflated) price of
            // the replicating swaption is
            // below a given threshold but not outside the given bounds. n
            // discrete scenarios are used on
            // each side of the strike
            Settings &withPriceThreshold(const Size n,
                                         const Real priceThreshold,
                                         const Real lowerRateBound = 0.0001,
                                         const Real upperRateBound = 2.0000) {
                strategy_ = PriceThreshold;
                priceThreshold_ = priceThreshold;
                lowerRateBound_ = lowerRateBound;
                upperRateBound_ = upperRateBound;
                return *this;
            }

            // use cash settlement pricing formula for replicating swaptions
            Settings &
            withCashSettledSwaptions(const bool cashSettledSwaptions) {
                QL_REQUIRE(!cashSettledSwaptions,
                           "cash settled swaptions basket not yet implemented");
                cashSettledSwaptions_ = cashSettledSwaptions;
                return *this;
            }

            Strategy strategy_;
            Real vegaRatio_;
            Real priceThreshold_;
            Real lowerRateBound_, upperRateBound_;
            bool cashSettledSwaptions_;
        };

        LinearTsrPricer(const Handle<SwaptionVolatilityStructure> &swaptionVol,
                     const Handle<Quote> &meanReversion,
                     const Handle<YieldTermStructure> &couponDiscountCurve =
                         Handle<YieldTermStructure>(),
                        const Settings &settings = Settings(),
                        const boost::shared_ptr<Integrator> &integrator = boost::shared_ptr<Integrator>());

        /* */
        virtual Real swapletPrice() const;
        virtual Rate swapletRate() const;
        virtual Real capletPrice(Rate effectiveCap) const;
        virtual Rate capletRate(Rate effectiveCap) const;
        virtual Real floorletPrice(Rate effectiveFloor) const;
        virtual Rate floorletRate(Rate effectiveFloor) const;
        /* */
        Real meanReversion() const;
        void setMeanReversion(const Handle<Quote> &meanReversion) {
            unregisterWith(meanReversion_);
            meanReversion_ = meanReversion;
            registerWith(meanReversion_);
            update();
        }

      private:
        const Real GsrG(const Date &d) const;
        const Real singularTerms(const Option::Type type, const Real strike) const;
        const Real integrand(const Real strike) const;
        Real a_, b_;

        class VegaRatioHelper;
        friend class VegaRatioelper;
        class VegaRatioHelper {
          public:
            VegaRatioHelper(const SmileSection *section, const Real targetVega)
                : section_(section), targetVega_(targetVega) {}
            double operator()(double strike) const {
                return section_->vega(strike) - targetVega_;
            };
            const SmileSection *section_;
            const Real targetVega_;
        };

        class PriceHelper;
        friend class PriceHelper;
        class PriceHelper {
          public:
            PriceHelper(const SmileSection *section, const Option::Type type,
                        const Real targetPrice)
                : section_(section), targetPrice_(targetPrice), type_(type) {}
            double operator()(double strike) const {
                return section_->optionPrice(strike, type_) - targetPrice_;
            };
            const SmileSection *section_;
            const Real targetPrice_;
            const Option::Type type_;
        };

        void initialize(const FloatingRateCoupon &coupon);

        Real optionletPrice(Option::Type optionType, Real strike) const;

        Real strikeFromVegaRatio(Real ratio, Option::Type optionType,
                                 Real referenceStrike) const;
        Real strikeFromPrice(Real price, Option::Type optionType,
                             Real referenceStrike) const;

        Handle<Quote> meanReversion_;

        Handle<YieldTermStructure> forwardCurve_, discountCurve_;
        Handle<YieldTermStructure> couponDiscountCurve_;

        const CmsCoupon *coupon_;

        Date today_, paymentDate_, fixingDate_;

        Real gearing_, spread_;

        Period swapTenor_;
        Real spreadLegValue_, swapRateValue_, couponDiscountRatio_, annuity_;

        boost::shared_ptr<SwapIndex> swapIndex_;
        boost::shared_ptr<VanillaSwap> swap_;
        boost::shared_ptr<SmileSection> smileSection_;

        Settings settings_;
        DayCounter volDayCounter_;
        boost::shared_ptr<Integrator> integrator_;
    };
}

#endif
