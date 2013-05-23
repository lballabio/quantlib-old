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

#ifndef quantlib_multistep_volswap_hpp
#define quantlib_multistep_volswap_hpp

#include <ql/models/marketmodels/products/multiproductmultistep.hpp>

namespace QuantLib {

    class MultiStepVolSwap : public MultiProductMultiStep {
      public:
        MultiStepVolSwap(const std::vector<Time>& rateTimes,
						 const std::vector<Time>& obsTimes,
			             const std::vector<Time>& paymentTimes,
						 const std::vector<Real>& structuredAccruals,
						 const std::vector<Real>& floatingAccruals,
						 const std::vector<Size>& structuredFixingIndices,      // indices of structured leg fixings w.r.t. obsTimes
						 const std::vector<Size>& floatingFixingIndices,        // indices of floating leg fixings w.r.t. obsTimes
						 const std::vector<Size>& structuredPaymentIndices,     // indices of structured leg payments w.r.t. paymentTimes (!)
						 const std::vector<Size>& floatingPaymentIndices,       // indices of floating leg payments w.r.t. paymentTimes (!)
						 const Rate fixedRate, const Real multiplier, const Rate floor,
						 const std::pair<Size,Size>& referenceRateIndices,
						 const Size referenceRateStep,
						 const std::vector<Rate>& referenceRateFixings,
					     const bool payer = true);
        //! \name MarketModelMultiProduct interface
        //@{
        std::vector<Time> possibleCashFlowTimes() const;
        Size numberOfProducts() const;
        Size maxNumberOfCashFlowsPerProductPerStep() const;
        void reset();
        bool nextTimeStep(
                     const CurveState& currentState,
                     std::vector<Size>& numberCashFlowsThisStep,
                     std::vector<std::vector<CashFlow> >& cashFlowsGenerated);
        std::unique_ptr<MarketModelMultiProduct> clone() const;
        //@}
		void filterStructuredIndex(int i) { filterStructuredIndex_ = i; }
      private:
        const std::vector<Real> structuredAccruals_, floatingAccruals_;
        const std::vector<Time> rateTimes_, paymentTimes_, obsTimes_;
        const Rate fixedRate_, multiplier_, floor_;
		std::vector<Size> structuredFixingIndices_, floatingFixingIndices_;
		const std::vector<Size> structuredPaymentIndices_, floatingPaymentIndices_;
		const std::pair<Size,Size> referenceRateIndices_;
		const Size referenceRateStep_;
		std::vector<Rate> referenceRateFixings0_;
        const bool payer_;
        //Size lastIndex_;
        // things that vary in a path
        Size currentIndex_, currentStructuredIndex_, currentFloatingIndex_;
		int filterStructuredIndex_;
		std::vector<Rate> referenceRateFixings_;
    };


    // inline definitions

    inline std::vector<Time>
    MultiStepVolSwap::possibleCashFlowTimes() const {
        return paymentTimes_;
    }

    inline Size MultiStepVolSwap::numberOfProducts() const {
        return 1;
    }

    inline Size MultiStepVolSwap::maxNumberOfCashFlowsPerProductPerStep() const {
        return 2;
    }

    inline void MultiStepVolSwap::reset() {
       currentIndex_=0;
	   currentStructuredIndex_=0;
	   currentFloatingIndex_=0;
	   referenceRateFixings_ = referenceRateFixings0_;
	   referenceRateFixings_.push_back(0.0);
    }

}

#endif
