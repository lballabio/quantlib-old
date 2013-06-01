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

#include <ql/cashflows/couponpricer.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLib {

    class CmsCoupon;
    class YieldTermStructure;
    
    //! CMS-coupon pricer
    /*! Prices a cms coupon via static replication
        The replication portfolio is discrete
    */

    class CmsReplicationPricer: public CmsCouponPricer {
      
    public:

	struct Settings {

	    enum Strategy { DiscreteStrikes, RateBound, VegaRatio, PriceThreshold };
     
	    Settings() : strategy_(PriceThreshold), priceThreshold_(1.0E-6), cashSettledSwaptions_(false) {}

	    // hedge with given discrete strikes relative to atm, e.g.
	    // 0.0025, 0.0050, 0.0100, 0.0150, 0.0200, 0.0250 means that swaptions 
	    // atm, 0.0025, ... , 0.0200 are used to hedge scenarios 0.0025, ... , 0.0250
	    Settings& withDiscreteStrikeSpreads(const std::vector<Real>& discreteStrikeSpreads) { 
		strategy_ = DiscreteStrikes; discreteStrikeSpreads_ = discreteStrikeSpreads; return *this;
	    }

	    // hedge with given lower and upper bound for swap rate, e.g. 0.0001 and 2.0000
	    Settings& withRateBound(const Real lowerRateBound, const Real upperRateBound) {
		strategy_ = RateBound; 
		lowerRateBound_ = lowerRateBound; upperRateBound_ = upperRateBound; return *this; 
	    }

	    // hedge with an lower / upper rate bound such that the swaption vega is a given ratio of the
	    // atm vega but not outside the given bounds
	    Settings& withVegaRatio(const Real vegaRatio, 
				    const Real lowerBound = 0.0001, const Real upperBound = 2.0000) { 
		strategy_ = VegaRatio; vegaRatio_ = vegaRatio; 
		lowerRateBound_=lowerBound; upperRateBound_=upperBound; return *this; 
	    }

	    // hedge with an upper rate bound such that a price of the replicating swaption is below
	    // a given threshold but not outside the given bounds
	    Settings& withPriceThreshold(const Real priceThreshold, 
					 const Real lowerBound = 0.0001, const Real upperBound = 2.0000) { 
		strategy_ = PriceThreshold; priceThreshold_ = priceThreshold;  
		lowerRateBound_=lowerBound; upperRateBound_=upperBound; return *this; 
	    }

	    // use cash settlement pricing formula for replicating swaptions
	    Settings& withCashSettledSwaptions(const bool cashSettledSwaptions) { 
		cashSettledSwaptions_ = cashSettledSwaptions; return *this; 
	    }

	    Strategy strategy_;
	    std::vector<Real> discreteStrikeSpreads_;
	    Real lowerRateBound_, upperRateBound_;
	    Real priceThreshold_;
	    Real vegaRatio_;
	    bool cashSettledSwaptions_;

	};


	CmsReplicationPricer(const Handle<SwaptionVolatilityStructure>& swaptionVol,
			     const Handle<Quote>& meanReversion,
			     const Handle<YieldTermStructure>& couponDiscountCurve = 
			       Handle<YieldTermStructure>(),
			     const Settings settings = Settings());
        
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

        void initialize(const FloatingRateCoupon& coupon);

        Real optionletPrice(Option::Type optionType, Real strike) const;

	Real hullWhiteScenario(Real dt, Real h);
	Real annuity(Real h);
	Real floatingLegNpv(Real h);
	Real swapRate(Real h);
      
        Handle<YieldTermStructure> forwardCurve_, discountCurve_;
        Handle<YieldTermStructure> couponDiscountCurve_;

        const CmsCoupon* coupon_;

	Date today_;

        Date paymentDate_, fixingDate_;

	std::vector<Real> fixedLegYearFractions_;
	std::vector<Date> fixedLegPaymentDates_, floatingLegStartDates_, 
	    floatingLegEndDates_, floatingLegPaymentDates_;

        Real gearing_, spread_;

        Handle<Quote> meanReversion_;

        Period swapTenor_;
        Real spreadLegValue_, swapRateValue_, discount_;
	
	boost::shared_ptr<SwapIndex> swapIndex_;
	boost::shared_ptr<VanillaSwap> swap_;

	Settings settings_;

    };

}


#endif
