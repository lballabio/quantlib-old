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
