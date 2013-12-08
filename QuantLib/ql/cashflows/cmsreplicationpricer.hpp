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

/*! \file cmsreplicationpricer.hpp
  \brief CMS-coupon pricer
*/

#ifndef quantlib_cmsreplication_pricer_hpp
#define quantlib_cmsreplication_pricer_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLib {

    class CmsCoupon;
    class YieldTermStructure;
    
    //! CMS-coupon pricer
    /*! Prices a cms coupon via static replication
      The replication portfolio is discrete. This implementation is only for test purposes,
      it is highly inefficient.
    */

    class CmsReplicationPricer: public CmsCouponPricer {
      
    public:

        struct Settings {

            enum Strategy { DiscreteStrikeSpreads, RateBound, VegaRatio, PriceThreshold };
     
            Settings() : strategy_(RateBound), vegaRatio_(0.01), priceThreshold_(1.0E-8),
                         lowerRateBound_(0.0001), upperRateBound_(2.0000),
                         cashSettledSwaptions_(false),
                         enforceMonotonicPrices_(true),
                         simplifiedFloatingLeg_(false),
                         n_(100) {}

            // hedge with given discrete strikes relative to the strike, e.g.
            // 0.0025, 0.0050, 0.0100, 0.0150, 0.0200, 0.0250 means that swaptions 
            // strike, strike+0.0025, ... , 0.0200 are used to hedge scenarios strike+0.0025, ... , 0.0250
            // and similar for the left side of the strike
            // always stay between lowerRateBound and upperRateBound
            Settings& withDiscreteStrikeSpreads(const std::vector<Real>& discreteStrikeSpreads,
                                                const Real lowerRateBound = 0.0001, const Real upperRateBound = 2.0000) { 
                strategy_ = DiscreteStrikeSpreads; discreteStrikeSpreads_ = discreteStrikeSpreads; 
                n_ = discreteStrikeSpreads_.size();
                lowerRateBound_ = lowerRateBound; upperRateBound_ = upperRateBound; return *this; 
            }

            // hedge with given lower and upper bound for swap rate, e.g. 0.0001 and 2.0000
            // n discrete scenarios are used on each side of the strike
            Settings& withRateBound(const Size n, const Real lowerRateBound, const Real upperRateBound) {
                strategy_ = RateBound; 
                n_= n;
                lowerRateBound_ = lowerRateBound; upperRateBound_ = upperRateBound; return *this; 
            }

            // hedge with an lower / upper rate bound such that the swaption vega is a given ratio of the
            // atm vega but not outside the given bounds. Again n discrete scenarios are used on each
            // side of the strike
            Settings& withVegaRatio(const Size n, const Real vegaRatio,
                                    const Real lowerRateBound = 0.0001, const Real upperRateBound = 2.0000) { 
                strategy_ = VegaRatio; 
                n_=n; vegaRatio_ = vegaRatio; 
                lowerRateBound_=lowerRateBound; upperRateBound_=upperRateBound; return *this; 
            }

            // hedge with an upper rate bound such that a (undeflated) price of the replicating swaption is
            // below a given threshold but not outside the given bounds. n discrete scenarios are used on
            // each side of the strike
            Settings& withPriceThreshold(const Size n, const Real priceThreshold,
                                         const Real lowerRateBound = 0.0001, const Real upperRateBound = 2.0000) { 
                strategy_ = PriceThreshold; 
                n_=n; priceThreshold_ = priceThreshold;  
                lowerRateBound_=lowerRateBound; upperRateBound_=upperRateBound; return *this; 
            }

            // use cash settlement pricing formula for replicating swaptions
            Settings& withCashSettledSwaptions(const bool cashSettledSwaptions) {
                QL_REQUIRE(!cashSettledSwaptions,"cash settled swaptions basket not yet implemented"); 
                cashSettledSwaptions_ = cashSettledSwaptions; return *this; 
            }

            // enforce monotonic decreasing call swaptions and increasing put swaptions
            Settings& withMonotonicPrices(const bool enforceMonotonicPrices) {
                enforceMonotonicPrices_ = enforceMonotonicPrices; return *this;
            }

            // use simplified formula for floating leg swap evaluation
            Settings& withSimplifiedFloatingLeg(const bool simplifiedFloatingLeg) {
                simplifiedFloatingLeg_ = simplifiedFloatingLeg; return *this;
            }

            Strategy strategy_;
            std::vector<Real> discreteStrikeSpreads_;
            Real vegaRatio_;
            Real priceThreshold_;
            Real lowerRateBound_, upperRateBound_;
            bool cashSettledSwaptions_, enforceMonotonicPrices_, simplifiedFloatingLeg_;
            Size n_;

        };


        CmsReplicationPricer(const Handle<SwaptionVolatilityStructure>& swaptionVol,
                             const Handle<Quote>& meanReversion,
                             const Handle<YieldTermStructure>& couponDiscountCurve = 
                             Handle<YieldTermStructure>(),
                             const Settings& settings = Settings());
        
        /* */
        virtual Real swapletPrice() const;
        virtual Rate swapletRate() const;
        virtual Real capletPrice(Rate effectiveCap) const;
        virtual Rate capletRate(Rate effectiveCap) const;
        virtual Real floorletPrice(Rate effectiveFloor) const;
        virtual Rate floorletRate(Rate effectiveFloor) const;
        /* */
        Real meanReversion() const;
        void setMeanReversion(const Handle<Quote>& meanReversion) {
            unregisterWith(meanReversion_);
            meanReversion_ = meanReversion;
            registerWith(meanReversion_);
            update();
        }

    private:

        class VegaRatioHelper;
        friend class VegaRatioelper;
        class VegaRatioHelper {
        public:
            VegaRatioHelper(const SmileSection* section,
                            const Real targetVega)
                : section_(section), targetVega_(targetVega) {}
            double operator()(double strike) const {
                return section_->vega(strike) - targetVega_;
            };
            const SmileSection* section_;
            const Real targetVega_;
        };

        class PriceHelper;
        friend class PriceHelper;
        class PriceHelper {
        public:
            PriceHelper(const SmileSection* section,
                        const Option::Type type,
                        const Real targetPrice)
                : section_(section), targetPrice_(targetPrice), type_(type) {}
            double operator()(double strike) const {
                return section_->optionPrice(strike,type_) - targetPrice_;
            };
            const SmileSection* section_;
            const Real targetPrice_;
            const Option::Type type_;
        };

        class HHelper;
        friend class HHelper;
        class HHelper {
        public:
            HHelper(const CmsReplicationPricer* pricer, const Real targetSwapRate)
                : pricer_(pricer), targetSwapRate_(targetSwapRate) {}
            double operator()(double h) const {
                return pricer_->swapRate(h) - targetSwapRate_;
            }
            const CmsReplicationPricer* pricer_;
            const Real targetSwapRate_;
        };

        void initialize(const FloatingRateCoupon& coupon);

        Real optionletPrice(Option::Type optionType, Real strike) const;

        Real hullWhiteScenario(Real dt, Real h) const;
        Real annuity(Real h) const;
        Real floatingLegNpv(Real h) const;
        Real swapRate(Real h) const;
    
        Real h(Real rate) const;
        Real strikeFromVegaRatio(Real ratio, Option::Type optionType, Real referenceStrike) const;
        Real strikeFromPrice(Real price, Option::Type optionType, Real referenceStrike) const;
      
        Handle<Quote> meanReversion_;
        Handle<YieldTermStructure> forwardCurve_, discountCurve_;
        Handle<YieldTermStructure> couponDiscountCurve_;

        const CmsCoupon* coupon_;

        Date today_;

        Date paymentDate_, fixingDate_;

        std::vector<Real> fixedLegYearFractions_;
        std::vector<Date> fixedLegPaymentDates_, floatingLegStartDates_, 
            floatingLegEndDates_, floatingLegPaymentDates_;

        Real gearing_, spread_;

        Period swapTenor_;
        Real spreadLegValue_, swapRateValue_, discount_, annuity_;
    
        boost::shared_ptr<SwapIndex> swapIndex_;
        boost::shared_ptr<VanillaSwap> swap_;
        boost::shared_ptr<SmileSection> smileSection_;

        Settings settings_;

    };

}


#endif
