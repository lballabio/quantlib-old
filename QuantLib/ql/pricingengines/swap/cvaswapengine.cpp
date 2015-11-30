#include <boost/make_shared.hpp>

#include <ql/pricingengines/swap/cvaswapengine.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/floatingratecoupon.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/exercise.hpp>

namespace QuantLib {

  CounterpartyAdjSwapEngine::CounterpartyAdjSwapEngine(
      const Handle<YieldTermStructure>& discountCurve,
      const Handle<DefaultProbabilityTermStructure>& ctptyDTS,
      Real ctptyRecoveryRate)
  : baseSwapEngine_(Handle<PricingEngine>(
      boost::make_shared<DiscountingSwapEngine>(discountCurve))),
      // to turn into a template arg and factory through constructors
      discountCurve_(discountCurve), 
      defaultTS_(ctptyDTS), 
      ctptyRecoveryRate_(ctptyRecoveryRate)
  {};


  void CounterpartyAdjSwapEngine::calculate() const {
    // test dates , ref dates, etc....

    Date priceDate = defaultTS_->referenceDate();
    //temporarilt fixed engine for intiial tests
    Volatility volRates = 0.15;
    boost::shared_ptr<Swaption::engine> spationletEngine = 
        boost::make_shared<BlackSwaptionEngine>(discountCurve_,
            volRates);

    Real cumOptVal = 0.;
    // Vanilla swap so 0 leg is floater

    std::vector<Date>::const_iterator iFD = 
	arguments_.floatingFixingDates.begin(); 
    std::vector<Date>::const_iterator endIt = 
        arguments_.floatingFixingDates.end();

    std::vector<Date>::const_iterator nextFD = 
      arguments_.fixedPayDates.begin();
    Date swapletStart = priceDate;
    while(*nextFD < priceDate) nextFD++;

    // Compute fair spread for strike value:
    // copy args into the non risky engine
    Swap::arguments * noCVAArgs = dynamic_cast<Swap::arguments*>(
      baseSwapEngine_->getArguments());

    noCVAArgs->legs = this->arguments_.legs;
    noCVAArgs->payer = this->arguments_.payer;

    baseSwapEngine_->calculate();

    Rate baseSwapRate = boost::dynamic_pointer_cast<FixedRateCoupon>(
	            arguments_.legs[0][0])->rate();
    Rate baseSwapFairRate = -baseSwapRate * 
      dynamic_cast<const Swap::results *>(
        baseSwapEngine_->getResults())->legNPV[1]   
        /  dynamic_cast<const Swap::results *>(
      baseSwapEngine_->getResults())->legNPV[0];
    Real baseSwapNPV =  dynamic_cast<const Swap::results *>(
      baseSwapEngine_->getResults())->value;

    // Swaplet options summatory:
    while(nextFD != arguments_.fixedPayDates.end()) {

	        // iFD coupon not fixed, create swaptionlet:
      boost::shared_ptr<IborIndex> swapIndex = 
	boost::dynamic_pointer_cast<IborIndex>(
          boost::dynamic_pointer_cast<FloatingRateCoupon>(
	    arguments_.legs[1][0])->index());

      // Alternatively one could cap this period to, say, 1M 
      Period swapPeriod = boost::dynamic_pointer_cast<FloatingRateCoupon>(
        arguments_.legs[1][0])->index()->tenor();

      Period baseSwapsTenor(arguments_.fixedPayDates.back().serialNumber() 
	- swapletStart.serialNumber(), Days);
      boost::shared_ptr<VanillaSwap> swaplet = MakeVanillaSwap(
        baseSwapsTenor,
        swapIndex, 
        baseSwapFairRate // strike
        )
	.withType(arguments_.type)
	.withNominal(arguments_.nominal)
	.withSettlementDays(2)
        .withEffectiveDate(swapletStart)
        .withTerminationDate(arguments_.fixedPayDates.back());
      Swaption swaptionlet(swaplet, 
        boost::make_shared<EuropeanExercise>(swapletStart));
      swaptionlet.setPricingEngine(spationletEngine);

      cumOptVal += swaptionlet.NPV() * defaultTS_->defaultProbability(
	swapletStart, *nextFD);

      swapletStart = *nextFD;
      nextFD++;
    }
  
    results_.value = baseSwapNPV - (1.-ctptyRecoveryRate_) * cumOptVal;

    results_.fairRate =  -baseSwapRate * (dynamic_cast<const Swap::results *>(
      baseSwapEngine_->getResults())->legNPV[1] - 
      (1.-ctptyRecoveryRate_) * cumOptVal )
      /  dynamic_cast<const Swap::results *>(
        baseSwapEngine_->getResults())->legNPV[0];
  }


}
