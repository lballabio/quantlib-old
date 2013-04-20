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


namespace QuantLib {

    //! Gsr non standard swaption engine
    /*! \ingroup swaptionengines
		All fixed coupons with start date greater or equal to the respective option expiry are considered to be part of the exercise into right.
		All float coupons with start date greater or equal to the respective option expiry are consideres to be part of the exercise into right.
		\warning Cash settled swaptions are not supported

		TODO at the moment the matching algorithm to find the representative swaption is restricted to the models monocurve setup at the moment. Extend this to multicurve ? Does that have an effect ?
		\warning the standardSwapBase index should have associated forward and discount curves. These curves are used for setup of the swaption helper.
		         this means that the market price of the calibration instrument is calculated using these curves. Therefore the model price must be calculated
	             using the same curves, otherwise the calibration gets incosistent, i.e. the pricing engine used for model calibration has to be capable of using
				 the same curves as associated to the index. Also the volatility structure passed to construct the calibration helper should use curves that are consistent with the model calibration curve setup.
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
						 const Handle<YieldTermStructure>& discountCurve=Handle<YieldTermStructure>(),
						 const Handle<YieldTermStructure>& forwardCurve=Handle<YieldTermStructure>())
        : GenericModelEngine<Gsr,
                             NonstandardSwaption::arguments,
                             NonstandardSwaption::results>(model),
		  integrationPoints_(integrationPoints) , stddevs_(stddevs), extrapolatePayoff_(extrapolatePayoff), flatPayoffExtrapolation_(flatPayoffExtrapolation),
		discountYts_(discountCurve), forwardYts_(forwardCurve) {
		
			  registerWith(discountYts_);
			  registerWith(forwardYts_);
		
		}
        void calculate() const;

      
	private:
		const int integrationPoints_;
		const Real stddevs_;
		const bool extrapolatePayoff_,flatPayoffExtrapolation_;
		const Handle<YieldTermStructure> discountYts_;
		const Handle<YieldTermStructure> forwardYts_;

		friend class NonstandardSwaption;

		Disposable<std::vector<boost::shared_ptr<CalibrationHelper>>> calibrationBasket(boost::shared_ptr<SwapIndex> standardSwapBase,
			boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolatility,
			const NonstandardSwaption::CalibrationBasketType basketType = NonstandardSwaption::MaturityStrikeByDeltaGamma ) const;
		
		Real fixedLegNpv(Size idx, Date& referenceDate, Real y) const;
		Real floatingLegNpv(Size idx, Date& referenceDate, Real y) const;

    };

}


#endif

