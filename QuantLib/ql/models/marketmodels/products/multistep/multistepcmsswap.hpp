/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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

#ifndef quantlib_multistep_cms_swap_hpp
#define quantlib_multistep_cms_swap_hpp

#include <ql/models/marketmodels/products/multiproductmultistep.hpp>
#include <ql/utilities/null.hpp>

namespace QuantLib {

    class MultiStepCmsSwap : public MultiProductMultiStep {
      public:
        MultiStepCmsSwap(const std::vector<Time>& rateTimes,
                      const std::vector<Real>& cmsAccruals,
                      const std::vector<Real>& floatingAccruals,
                      const std::vector<Time>& paymentTimes,
                      Size swapLength,
                      Size swapStep = 1,
                      Rate cmsMargin = 0.0,
                      Rate floatMargin = 0.0,
                      Rate cmsFloor = Null<Real>(),
                      Rate cmsCap = Null<Real>(),
                      bool payer = true);
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
      private:
        std::vector<Real> cmsAccruals_, floatingAccruals_;
        std::vector<Time> paymentTimes_;
        Size swapLength_, swapStep_;
        Rate cmsMargin_, floatMargin_;
        Rate cmsFloor_, cmsCap_;
        bool payer_;
        Real multiplier_;
        Size lastIndex_;
        // things that vary in a path
        Size currentIndex_;
    };


    // inline definitions

    inline std::vector<Time>
    MultiStepCmsSwap::possibleCashFlowTimes() const {
        return paymentTimes_;
    }

    inline Size MultiStepCmsSwap::numberOfProducts() const {
        return 1;
    }

    inline Size
    MultiStepCmsSwap::maxNumberOfCashFlowsPerProductPerStep() const {
        return 2;
    }

    inline void MultiStepCmsSwap::reset() {
       currentIndex_=0;
    }

}

#endif
