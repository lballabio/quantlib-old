/*! \file hullwhitesmilesection.hpp
    \brief Smile section implementation for hull white model generated swaption smile
*/

#ifndef quantlib_hullwhite_smile_section_hpp
#define quantlib_hullwhite_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/models/shortrate/onefactormodels/hullwhite.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/instruments/swaption.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/pricingengines/swaption/jamshidianswaptionengine.hpp>
#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/indexes/swapindex.hpp>
#include <vector>
#include <iostream>

namespace QuantLib {

    class HullWhiteSmileSection : public SmileSection {
      public:
        HullWhiteSmileSection(const Date& optionDate, const Period& swapLength, boost::shared_ptr<SwapIndex> swapIndex,
			const Handle<YieldTermStructure>& yts, const Real reversion, const Real sigma);
        Real minStrike () const { return 0.0; }
        Real maxStrike () const { return QL_MAX_REAL; }
        Real atmLevel() const { return atm_; } // not implemented
      protected:
        //Real varianceImpl(Rate strike) const;
        Volatility volatilityImpl(Rate strike) const;
      private:
        const Date optionDate_;
		const Period swapLength_;
		const Real reversion_, sigma_;
		boost::shared_ptr<HullWhite> model_;
		boost::shared_ptr<JamshidianSwaptionEngine> jamshidianEngine_;
		boost::shared_ptr<DiscountingSwapEngine> discountingEngine_;
		boost::shared_ptr<SwapIndex> swapIndex_;
		const Handle<YieldTermStructure> yts_;
		Real atm_;
    };


}

#endif
