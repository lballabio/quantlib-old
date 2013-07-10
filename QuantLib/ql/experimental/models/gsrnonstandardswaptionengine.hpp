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

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file gsrNonstandardSwaptionEngine.hpp
    \brief
*/

#ifndef quantlib_pricers_gsr_nonstandardswaption_hpp
#define quantlib_pricers_gsr_nonstandardswaption_hpp

#include <ql/experimental/models/nonstandardswaption.hpp>
#include <ql/experimental/models/gsr.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>
#include <ql/math/optimization/costfunction.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>

//#define DEBUGOUTPUT

namespace QuantLib {

    //! Gsr non standard swaption engine
    /*! \ingroup swaptionengines

		All fixed coupons with start date greater or equal to the respective option expiry are considered 
        to be part of the exercise into right.

		All float coupons with start date greater or equal to the respective option expiry are consideres 
        to be part of the exercise into right.

		\warning Cash settled swaptions are not supported

        \warning the standardSwapBase index should have associated forward and discount curves. These curves are used 
        for setup of the swaption helper. This means that the market price of the calibration instrument is calculated using
        these curves. Therefore the model price must be calculated using the same curves, otherwise the calibration gets
        incosistent, i.e. the pricing engine used for model calibration has to be capable of using
		the same curves as associated to the index. Also the volatility structure passed to construct the calibration 
        helper should use curves that are consistent with the model calibration curve setup.
        Finally the discountCurve given in the constructor should be the same curve as the discounting curve of the
        swapIndex used to determine the calibration basket.
    */

    class GsrNonstandardSwaptionEngine
        : public GenericModelEngine<Gsr,
                                    NonstandardSwaption::arguments,
                                    NonstandardSwaption::results > {
      public:

        GsrNonstandardSwaptionEngine(
                         const boost::shared_ptr<Gsr>& model,
						 const int integrationPoints=64,
						 const Real stddevs=7.0,
						 const bool extrapolatePayoff=true,
						 const bool flatPayoffExtrapolation=false,
                         const Handle<Quote>& zSpread=Handle<Quote>(), // continuously compounded w.r.t. yts daycounter
						 const Handle<YieldTermStructure>& discountCurve=Handle<YieldTermStructure>())
            : GenericModelEngine<Gsr,NonstandardSwaption::arguments,
                                     NonstandardSwaption::results>(model),
		  integrationPoints_(integrationPoints) , stddevs_(stddevs), extrapolatePayoff_(extrapolatePayoff), 
          flatPayoffExtrapolation_(flatPayoffExtrapolation),
            zSpread_(zSpread), discountYts_(discountCurve) {
		

            if(zSpread_.empty()) 
                registerWith(zSpread_);

            if(!discountYts_.empty()) {
                  effectiveDiscountYts_ = &**discountYts_;
                  registerWith(discountYts_);
            }
            else {
                  effectiveDiscountYts_ = &**model_->termStructure();
            }
                  		
		}

        void calculate() const;

      
	private:

		const int integrationPoints_;
		const Real stddevs_;
		const bool extrapolatePayoff_,flatPayoffExtrapolation_;
		const Handle<YieldTermStructure> discountYts_;
        const Handle<Quote> zSpread_;
        mutable YieldTermStructure *effectiveDiscountYts_, *effectiveForwardYts_;
        mutable std::vector<Real> t_,yF_,yD_; // discrete zero rate spreads to market curves used in interpolation objects
		mutable boost::shared_ptr<Interpolation> spreadF_, spreadD_; // zero rate spreads to market curves

		friend class NonstandardSwaption;

		Disposable<std::vector<boost::shared_ptr<CalibrationHelper>>> 
            calibrationBasket(boost::shared_ptr<SwapIndex> standardSwapBase,
                              boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolatility,
                              const NonstandardSwaption::CalibrationBasketType basketType = 
                              NonstandardSwaption::MaturityStrikeByDeltaGamma ) const;
		
        void initSpreadCurves() const;
		Real fixedLegNpv(Size idx, Date& referenceDate, Real y) const;
		Real floatingLegNpv(Size idx, Date& referenceDate, Real y) const;

        class MatchHelper;
        friend class MatchHelper;
        class MatchHelper : public CostFunction {

        public:

            MatchHelper(const NonstandardSwap::Type type, const Real npv, const Real delta, const Real gamma, 
                        const Handle<Gsr>& model, const boost::shared_ptr<SwapIndex> indexBase, 
                        const Date& expiry, const Real h, const boost::shared_ptr<Interpolation> spreadD,
                        const boost::shared_ptr<Interpolation> spreadF) : 
                type_(type), mdl_(model), indexBase_(indexBase), expiry_(expiry), npv_(npv), 
                delta_(delta), gamma_(gamma), h_(h), spreadD_(spreadD), spreadF_(spreadF) {}

            Real NPV(boost::shared_ptr<VanillaSwap> swap, Real fixedRate, Real nominal, Real y, int type) const {
                Real npv=0.0;
                for(Size i=0; i < swap->fixedLeg().size() ; i++) {
                    boost::shared_ptr<FixedRateCoupon> c = 
                        boost::dynamic_pointer_cast<FixedRateCoupon>(swap->fixedLeg()[i]);
                    npv -= fixedRate * c->accrualPeriod() * nominal * mdl_->zerobond(c->date(),expiry_,y,spreadD_);
                }
                for(Size i=0; i < swap->floatingLeg().size() ; i++) {
                    boost::shared_ptr<IborCoupon> c = boost::dynamic_pointer_cast<IborCoupon>(swap->floatingLeg()[i]);
                    npv += mdl_->forwardRate(c->fixingDate(),c->iborIndex(),expiry_,y,spreadF_) * c->accrualPeriod() * 
                        nominal * mdl_->zerobond(c->date(),expiry_,y,spreadD_);
                }
                return (Real)type*npv;
            }

            Real value(const Array &v) const {
                Array vals = values(v);
                Real res=0.0;
                for(Size i=0;i<vals.size();i++) {
                    res += vals[i]*vals[i];
                }
                return std::sqrt(res/vals.size());
            }

            Disposable<Array> values(const Array &v) const {
                // transformations
                int type = type_; // start with same type as non standard underlying (1 means payer, -1 receiver)
                Real nominal = fabs(v[0]);
                if(v[0] < 0.0) type *=-1;
                Real maturity = fabs(v[1]);
                Real fixedRate = fabs(v[2]);
                if(v[2] < 0.0) type *=-1;
                Size years = (Size)std::floor(maturity);
                maturity -= (Real)years; maturity *= 12.0;
                Size months = (Size)std::floor(maturity);
                Real alpha = 1.0-(maturity-(Real)months);
                if(years==0 && months==0) {
                    months=1; // ensure a maturity of at least one months ...
                    alpha=1.0; // ... but in this case only look at the lower maturity swap
                }
                //maturity -= (Real)months; maturity *= 365.25;
                //Size days = (Size)std::floor(maturity);
                //Real alpha = 1.0-(maturity-(Real)days);
                // generate swap
                Period lowerPeriod = years*Years+months*Months;//+days*Days;
                Period upperPeriod = lowerPeriod + 1*Months; // 1*Days;
                boost::shared_ptr<SwapIndex> tmpIndexLower, tmpIndexUpper;
                if(indexBase_->exogenousDiscount()) {
                    tmpIndexLower = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),
                                                  lowerPeriod,indexBase_->fixingDays(),indexBase_->currency(),
                                                  indexBase_->fixingCalendar(), indexBase_->fixedLegTenor(),
                                                  indexBase_->fixedLegConvention(),indexBase_->dayCounter(),
                                                  indexBase_->iborIndex(),indexBase_->discountingTermStructure()));
                    tmpIndexUpper = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),
                                                  upperPeriod,indexBase_->fixingDays(),indexBase_->currency(),
                                                  indexBase_->fixingCalendar(),indexBase_->fixedLegTenor(),
                                                  indexBase_->fixedLegConvention(),indexBase_->dayCounter(),
                                                  indexBase_->iborIndex(),indexBase_->discountingTermStructure()));
                }
                else {
                    tmpIndexLower = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),
                                                  lowerPeriod,indexBase_->fixingDays(),indexBase_->currency(),
                                                  indexBase_->fixingCalendar(),indexBase_->fixedLegTenor(),
                                                  indexBase_->fixedLegConvention(),indexBase_->dayCounter(),
                                                  indexBase_->iborIndex()));
                    tmpIndexUpper = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),upperPeriod,
                                                  indexBase_->fixingDays(),indexBase_->currency(),indexBase_->fixingCalendar(),
                                                  indexBase_->fixedLegTenor(),indexBase_->fixedLegConvention(),
                                                  indexBase_->dayCounter(),indexBase_->iborIndex()));
                }
                boost::shared_ptr<VanillaSwap> swapLower = tmpIndexLower->underlyingSwap(expiry_);
                boost::shared_ptr<VanillaSwap> swapUpper = tmpIndexUpper->underlyingSwap(expiry_);
                // compute npv, delta, gamma
                Real npvm = alpha*NPV(swapLower,fixedRate,nominal,-h_,type) + 
                    (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,-h_,type);
                Real npv = alpha*NPV(swapLower,fixedRate,nominal,0.0,type) + 
                    (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,0.0,type);
                Real npvu = alpha*NPV(swapLower,fixedRate,nominal,h_,type) + 
                    (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,h_,type);
                Real delta = (npvu-npvm) / (2.0*h_);
                Real gamma = (npvu-2.0*npv+npvm) / (h_*h_);
                // debug output global standard underlying npv
#ifdef DEBUGOUTPUT
                Real xtmp=-5.0;
                std::cout << "globalStandardNpv;";
                while(xtmp<=5.0+QL_EPSILON) {
                    std::cout <<  alpha*NPV(swapLower,fixedRate,nominal,xtmp,type) + 
                        (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,xtmp,type) << ";";
                    xtmp+=0.1;
                }
                std::cout << std::endl;
#endif
                // return target function values
                Array res(3);
                res[0] = (npv-npv_) / delta_;
                res[1] = (delta-delta_) / delta_;
                res[2] = (gamma-gamma_) / gamma_;
                return res;
            }

            const Real npv_,delta_,gamma_,h_;
            const Date& expiry_;
            const boost::shared_ptr<SwapIndex> indexBase_;
            const Handle<Gsr>& mdl_;
            const NonstandardSwap::Type type_;
            
            const boost::shared_ptr<Interpolation> spreadD_, spreadF_;

        };


    };

}


#endif

