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

/*! \file gsrSwaptionEngine.hpp
    \brief
*/

#ifndef quantlib_pricers_gsr_swaption_hpp
#define quantlib_pricers_gsr_swaption_hpp

#include <ql/instruments/swaption.hpp>
#include <ql/experimental/models/gsr/gsr.hpp>

#include <ql/pricingengines/genericmodelengine.hpp>


namespace QuantLib {

    //! Gsr swaption engine
    /*! \ingroup swaptionengines
		All fixed coupons with start date greater or equal to the respective option expiry are considered to be part of the exercise into right.
		All float coupons with start date greater or equal to the respective option expiry are consideres to be part of the exercise into right.
		\warning Cash settled swaptions are not supported

    */

    class GsrSwaptionEngine
        : public GenericModelEngine<Gsr,
                                    Swaption::arguments,
                                    Swaption::results > {
      public:
        GsrSwaptionEngine(
                         const boost::shared_ptr<Gsr>& model,
						 const int integrationPoints=64,
						 const Real stddevs=7.0,
						 const bool extrapolatePayoff=true,
						 const bool flatPayoffExtrapolation=false,
						 const Handle<YieldTermStructure>& discountCurve=Handle<YieldTermStructure>(),
						 const Handle<YieldTermStructure>& forwardCurve=Handle<YieldTermStructure>())
        : GenericModelEngine<Gsr,
                             Swaption::arguments,
                             Swaption::results>(model),
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
    };

}


#endif

