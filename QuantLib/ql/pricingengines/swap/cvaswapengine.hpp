/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Jose Aparicio

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

#ifndef quantlib_cva_swap_engine_hpp
#define quantlib_cva_swap_engine_hpp

#include <ql/instruments/vanillaswap.hpp>
#include <ql/instruments/swaption.hpp>

//temporary, type is fixed:
/* ***************************************************************
#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/termstructures/defaulttermstructure.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>
*/

namespace QuantLib {

    class DefaultProbabilityTermStructure;

  /*! Bilateral (CVA and DVA) default adjusted vanilla swap pricing
    engine. Collateral is not considered. No wrong way risk is 
    considered (rates and counterparty default are uncorrelated).
    Based on:
    Sorensen,  E.H.  and  Bollier,  T.F.,  Pricing  swap  default 
    risk. Financial Analysts Journal, 1994, 50, 23–33
    Also see sect. II-5 in: Risk Neutral Pricing of Counterparty Risk
    D. Brigo, M. Masetti, 2004
    or in sections 3 and 4 of "A Formula for Interest Rate Swaps 
      Valuation under Counterparty Risk in presence of Netting Agreements"
    D. Brigo and M. Masetti; May 4, 2005

    to do: Compute fair rate through iteration instead of the 
    current approximation .
   */
  class CounterpartyAdjSwapEngine : public VanillaSwap::engine {
    public:
      
      CounterpartyAdjSwapEngine(
          const Handle<YieldTermStructure>& discountCurve,
          const Handle<Swaption::engine>& swaptionEngine,
          const Handle<DefaultProbabilityTermStructure>& ctptyDTS,
          Real ctptyRecoveryRate,
          const Handle<DefaultProbabilityTermStructure>& invstDTS,
          Real invstRecoveryRate);
    
      /*! Creates an engine with a black volatility model for the 
        exposure.
       */
      CounterpartyAdjSwapEngine(
          const Handle<YieldTermStructure>& discountCurve,
          const Volatility blackVol,
          const Handle<DefaultProbabilityTermStructure>& ctptyDTS,
          Real ctptyRecoveryRate,
          const Handle<DefaultProbabilityTermStructure>& invstDTS,
          Real invstRecoveryRate);
      /*! Creates an engine with a black volatility model for the 
        exposure.
       */
      CounterpartyAdjSwapEngine(
          const Handle<YieldTermStructure>& discountCurve,
          const Handle<Quote>& blackVol,
          const Handle<DefaultProbabilityTermStructure>& ctptyDTS,
          Real ctptyRecoveryRate,
          const Handle<DefaultProbabilityTermStructure>& invstDTS,
          Real invstRecoveryRate);
      // write another constructor with issuer and event type.

      void calculate() const;
    private:
      Handle<PricingEngine> baseSwapEngine_;
      Handle<Swaption::engine> swaptionletEngine_;
      Handle<YieldTermStructure> discountCurve_;
      Handle<DefaultProbabilityTermStructure> defaultTS_;	  
      Real ctptyRecoveryRate_;
      Handle<DefaultProbabilityTermStructure> invstDTS_;	  
      Real invstRecoveryRate_;
  };

}

#endif
