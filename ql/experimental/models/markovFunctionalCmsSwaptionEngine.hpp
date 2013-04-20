/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib liense.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file markovFunctionalCmsSwaptionEngine.hpp
    \brief
*/

#ifndef quantlib_pricers_markovFunctional_cmsswaption_hpp
#define quantlib_pricers_markovFunctional_cmsswaption_hpp

#include <ql/experimental/models/markovfunctional/cmsswaption.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>

#include <ql/experimental/models/markovfunctional/markovfunctional.hpp>

namespace QuantLib {

    //! Markov functional cms swaption engine
    /*! \ingroup swaptionengines
		All float coupons with start date greater or equal to the respective option expiry and all structured coupons with fixing date greater or equal to the respective option expiry
		are considered to be part of the exercise into right.
		We have the following curves:
		A The markov funcional models curve.
		B The instruments ibor index forwarding curve
		C The instruments swap index forwarding curve
		D The instruments swap index discounting curve
		Initially zero spread curves B-A_0, C-A_0, D-A_0 are built, where A_0 is the model curve at t=0. The spread curves are used as static corrections in the model solution, i.e.
		when evaluating ibor or swap coupons the relevant spreads from the curve are added to the model curve, the deflation is done using A + (D-A_0) and the final discounting is done on D.
		An example setup for a collaterized option on a swap exchanging EUR CMS10y vs. Euribor3m would be
		A = 6m Euribor forward curve
		B = 3m Euribor forward curve
		C = 6m Euribor forward curve
		D = Eonia curve

		// FIXME relevant floating coupons may have been fixed on or before today, this may cause problems below
		// FIXME ... how to register with the curves in iborIndex and swapIndex from which the spread curves are built? It is not possible in the constructor because we do not have the arguments there ...


	*/

    class MarkovFunctionalCmsSwaptionEngine
        : public GenericModelEngine<MarkovFunctional,
                                    CmsSwaption::arguments,
                                    CmsSwaption::results > {
      public:
        MarkovFunctionalCmsSwaptionEngine(
                         const boost::shared_ptr<MarkovFunctional>& model,
						 const int integrationPoints=64,
						 const Real stddevs=7.0,
						 const bool extrapolatePayoff=true,
						 const bool flatPayoffExtrapolation=false);
        
        void calculate() const;
      
	private:
		const int integrationPoints_;
		const Real stddevs_;
		const bool extrapolatePayoff_,flatPayoffExtrapolation_;
		const boost::shared_ptr<MarkovFunctional> model_;
    };

}


#endif

