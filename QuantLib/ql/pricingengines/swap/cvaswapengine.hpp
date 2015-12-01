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
#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/termstructures/defaulttermstructure.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>

namespace QuantLib {

  /*! Unilateral counterparty adjusted swap pricing engine.
    See sect. II-5 in: Risk Neutral Pricing of Counterparty Risk
    D. Brigo, M. Masetti, 2004
    or in "A Formula for Interest Rate Swaps Valuation under
      Counterparty Risk in presence of Netting Agreements"
    D. Brigo and M. Masetti; May 4, 2005

    Using an object for the option engine. It might be more adecute
    to use a template. Using a type avoids resetting arguments and 
    results to the outside object with all the notifications passed.
   */
  class CounterpartyAdjSwapEngine : public VanillaSwap::engine { ///  ?????  : public Swap::engine
    public:
      // TODO: Registrations:
      CounterpartyAdjSwapEngine(
          const Handle<YieldTermStructure>& discountCurve,
          const Handle<DefaultProbabilityTermStructure>& ctptyDTS,
          Real ctptyRecoveryRate);

      // write another constructor with issuer and event type.

      void calculate() const;
    private:
      Handle<PricingEngine> baseSwapEngine_;
      //////  Handle<Swaption::engine> spationletEngine_;
      Handle<YieldTermStructure> discountCurve_;
      Handle<DefaultProbabilityTermStructure> defaultTS_;	  
      Real ctptyRecoveryRate_;
  };

}

#endif
