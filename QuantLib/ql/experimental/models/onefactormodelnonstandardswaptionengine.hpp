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

/*! \file onefactormodelnonstandardswaptionengine.hpp
    \brief
*/

#ifndef quantlib_pricers_onefactormodel_nonstandardswaption_hpp
#define quantlib_pricers_onefactormodel_nonstandardswaption_hpp

#include <ql/experimental/models/nonstandardswaption.hpp>
#include <ql/experimental/models/gsr.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>

//#define DEBUGOUTPUT

namespace QuantLib {

    //! One factor model non standard swaption engine
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

    class OneFactorModelNonstandardSwaptionEngine
        : public BasketGeneratingEngine, 
          public GenericModelEngine<OneFactorModel,
                                    NonstandardSwaption::arguments,
                                    NonstandardSwaption::results > {
      public:

        OneFactorModelNonstandardSwaptionEngine(
                         const boost::shared_ptr<OneFactorModel>& model,
						 const int integrationPoints=64,
						 const Real stddevs=7.0,
						 const bool extrapolatePayoff=true,
						 const bool flatPayoffExtrapolation=false,
                         const Handle<Quote>& oas=Handle<Quote>(), // continuously compounded w.r.t. yts daycounter
						 const Handle<YieldTermStructure>& discountCurve=Handle<YieldTermStructure>())
            :  BasketGeneratingEngine(model, oas, discountCurve),
               GenericModelEngine<OneFactorModel,NonstandardSwaption::arguments,
                                     NonstandardSwaption::results>(model),
		  integrationPoints_(integrationPoints) , stddevs_(stddevs), extrapolatePayoff_(extrapolatePayoff), 
          flatPayoffExtrapolation_(flatPayoffExtrapolation),
            oas_(oas), discountCurve_(discountCurve) {
		

            if(!oas_.empty()) 
                registerWith(oas_);

            if(!discountCurve_.empty())
                  registerWith(discountCurve_);
                  		
		}

        void calculate() const;

    protected:

        const Real underlyingNpv(const Date& expiry, const Real y) const;
        const VanillaSwap::Type underlyingType() const;
        const Date& underlyingLastDate() const;
        const Disposable<Array> initialGuess(const Date& expiry) const;
      
	private:

		const int integrationPoints_;
		const Real stddevs_;
		const bool extrapolatePayoff_,flatPayoffExtrapolation_;
		const Handle<YieldTermStructure> discountCurve_;
        const Handle<Quote> oas_;

    };

}


#endif

