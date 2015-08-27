/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

#include <ql/models/marketmodels/products/multistep/multistepvolswap.hpp>
#include <ql/models/marketmodels/curvestate.hpp>
#include <ql/models/marketmodels/utilities.hpp>

namespace QuantLib {

    MultiStepVolSwap::MultiStepVolSwap(const std::vector<Time>& rateTimes,
						 const std::vector<Time>& obsTimes,
			             const std::vector<Time>& paymentTimes,
						 const std::vector<Real>& structuredAccruals,
						 const std::vector<Real>& floatingAccruals,
						 const std::vector<Size>& structuredFixingIndices,   
						 const std::vector<Size>& floatingFixingIndices,       
						 const std::vector<Size>& structuredPaymentIndices,   
						 const std::vector<Size>& floatingPaymentIndices,   
						 const Rate fixedRate, const Real multiplier, const Rate floor,
						 const std::pair<Size,Size>& referenceRateIndices,
						 const Size referenceRateStep,
						 const std::vector<Rate>& referenceRateFixings,
					     bool payer) 
	  : MultiProductMultiStep(rateTimes),  structuredAccruals_(structuredAccruals),  floatingAccruals_(floatingAccruals), 
        rateTimes_(rateTimes),
fixedRate_(fixedRate), multiplier_(multiplier), floor_(floor), 
	  structuredFixingIndices_(structuredFixingIndices), floatingFixingIndices_(floatingFixingIndices),
	  structuredPaymentIndices_(structuredPaymentIndices), floatingPaymentIndices_(floatingPaymentIndices), 
	   
	  referenceRateIndices_(referenceRateIndices), referenceRateStep_(referenceRateStep),
	  referenceRateFixings0_(referenceRateFixings),
      /*lastIndex_(obsTimes.size()-1),*/ payer_(payer), filterStructuredIndex_(-1) {
		 
		QL_REQUIRE(obsTimes_.size()>1,
                   "Rate times must contain at least two values");

		checkIncreasingTimes(obsTimes_);
		checkIncreasingTimes(paymentTimes_);

        Size n = obsTimes_.size();
        std::vector<Time> evolutionTimes(n);
        std::vector<std::pair<Size,Size> > relevanceRates(n);
        for (Size i=0; i<n; ++i) {
            evolutionTimes[i] = obsTimes_[i];
            relevanceRates[i] = std::make_pair(i, rateTimes_.size()-1);
        }

        evolution_ = EvolutionDescription(rateTimes_, evolutionTimes, relevanceRates);
		QL_REQUIRE(referenceRateFixings0_.size()==8,"Reference rate fixings for 8 previous dates needed (" << referenceRateFixings_.size() << ")");
		floatingFixingIndices_.push_back(QL_MAX_INTEGER); // an index that will never be reached
		structuredFixingIndices_.push_back(QL_MAX_INTEGER); // an index that will never be reached
    }

    bool MultiStepVolSwap::nextTimeStep(
            const CurveState& currentState,
            std::vector<Size>& numberCashFlowsThisStep,
            std::vector<std::vector<MarketModelMultiProduct::CashFlow> >& genCashFlows)
    {

		//std::cout << "currentIndex=" << currentIndex_ << std::endl;

		Rate referenceRate = currentState.swapRate(referenceRateIndices_.first, referenceRateIndices_.second, referenceRateStep_ );
		
		for(Size i=8; i>=1; i--) referenceRateFixings_[i] = referenceRateFixings_[i-1];
		referenceRateFixings_[0] = referenceRate;

		Size noCf=0;

		if(currentIndex_ == floatingFixingIndices_[currentFloatingIndex_]) {
	        Rate liborRate = currentState.forwardRate(currentFloatingIndex_);
			//std::cout << "generate float payment @" << floatingPaymentIndices_[currentFloatingIndex_] << std::endl;
			genCashFlows[0][noCf].timeIndex = floatingPaymentIndices_[currentFloatingIndex_];
			genCashFlows[0][noCf].amount = (payer_ ? 1.0 : -1.0)*liborRate*floatingAccruals_[currentFloatingIndex_];
			noCf++;
			currentFloatingIndex_++;
		}

		if(currentIndex_ == structuredFixingIndices_[currentStructuredIndex_]) {
			//std::cout << "generate structured payment @" << paymentTimes_[structuredPaymentIndices_[currentStructuredIndex_]] << std::endl;
			Real volIdx = (fabs(referenceRateFixings_[1]-referenceRateFixings_[5])+
				          fabs(referenceRateFixings_[2]-referenceRateFixings_[6])+
						  fabs(referenceRateFixings_[3]-referenceRateFixings_[7])+
						  fabs(referenceRateFixings_[4]-referenceRateFixings_[8])) / 4.0;
			genCashFlows[0][noCf].timeIndex = structuredPaymentIndices_[currentStructuredIndex_];
			genCashFlows[0][noCf].amount = ( (currentStructuredIndex_== (Size)filterStructuredIndex_ || filterStructuredIndex_== -1) ? 1.0 : 0.0)*(payer_ ? -1.0 : 1.0)*structuredAccruals_[currentStructuredIndex_]*std::max(floor_,fixedRate_+multiplier_*volIdx);
			//genCashFlows[0][noCf].amount = (currentIndex_==structuredFixingIndices_[0] ? 1.0 : 0.0)*std::max(referenceRate-fixedRate_,0.0)*currentState.swapAnnuity(12,12,52,2); // TEST (plain vanilla swaption payoff)
			noCf++;
			currentStructuredIndex_++;
		}

        numberCashFlowsThisStep[0] = noCf;

        ++currentIndex_;

		bool done = (floatingFixingIndices_[currentFloatingIndex_] == QL_MAX_INTEGER && structuredFixingIndices_[currentStructuredIndex_] == QL_MAX_INTEGER );

		//std::cout << "ok, finished = " << done << std::endl;

        return done; 
    }

    std::unique_ptr<MarketModelMultiProduct> MultiStepVolSwap::clone() const {
        return std::unique_ptr<MarketModelMultiProduct>(new MultiStepVolSwap(*this));
    }

}

