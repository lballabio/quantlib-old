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
      Real ctptyRecoveryRate,
      const Handle<DefaultProbabilityTermStructure>& invstDTS,
      Real invstRecoveryRate)
  : baseSwapEngine_(Handle<PricingEngine>(
      boost::make_shared<DiscountingSwapEngine>(discountCurve))),
      // to turn into a template arg and factory through constructors
      discountCurve_(discountCurve), 
      defaultTS_(ctptyDTS), 
    ctptyRecoveryRate_(ctptyRecoveryRate),
    invstDTS_(invstDTS),
    invstRecoveryRate_(invstRecoveryRate)
  {
      registerWith(discountCurve);
      registerWith(ctptyDTS);
      registerWith(invstDTS);
  };


  void CounterpartyAdjSwapEngine::calculate() const {
    // test dates , ref dates, etc....

    Date priceDate = defaultTS_->referenceDate();
    //temporarilt fixed engine for intiial tests
    Volatility volRates = 0.15;
    boost::shared_ptr<Swaption::engine> spationletEngine = 
        boost::make_shared<BlackSwaptionEngine>(discountCurve_,
            volRates);

    Real cumOptVal = 0., 
        cumPutVal = 0.;
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

    VanillaSwap::Type reversedType = arguments_.type == VanillaSwap::Payer ? 
        VanillaSwap::Receiver : VanillaSwap::Payer;

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
      boost::shared_ptr<VanillaSwap> revSwaplet = MakeVanillaSwap(
        baseSwapsTenor,
        swapIndex, 
        baseSwapFairRate // strike
        )
	    .withType(reversedType)
	    .withNominal(arguments_.nominal)
	    .withSettlementDays(2)
        .withEffectiveDate(swapletStart)
        .withTerminationDate(arguments_.fixedPayDates.back());

      Swaption swaptionlet(swaplet, 
        boost::make_shared<EuropeanExercise>(swapletStart));
      Swaption putSwaplet(revSwaplet, 
        boost::make_shared<EuropeanExercise>(swapletStart));
      swaptionlet.setPricingEngine(spationletEngine);
      putSwaplet.setPricingEngine(spationletEngine);

      // atm underlying swap means that the value of put = value
      // call so this double pricing is not needed
      cumOptVal += swaptionlet.NPV() * defaultTS_->defaultProbability(
          swapletStart, *nextFD);
      cumPutVal += putSwaplet.NPV()  * invstDTS_->defaultProbability(
	      swapletStart, *nextFD);

      swapletStart = *nextFD;
      nextFD++;
    }
  
    results_.value = baseSwapNPV - (1.-ctptyRecoveryRate_) * cumOptVal
        + (1.-invstRecoveryRate_) * cumPutVal;

    results_.fairRate =  -baseSwapRate * (dynamic_cast<const Swap::results *>(
      baseSwapEngine_->getResults())->legNPV[1] - 
      (1.-ctptyRecoveryRate_) * cumOptVal + (1.-invstRecoveryRate_) * cumPutVal )
      /  dynamic_cast<const Swap::results *>(
        baseSwapEngine_->getResults())->legNPV[0];
  }


}
