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

#include <ql/models/marketmodels/products/multistep/multistepcmsswap.hpp>
#include <ql/models/marketmodels/curvestate.hpp>
#include <ql/models/marketmodels/utilities.hpp>

namespace QuantLib {

    MultiStepCmsSwap::MultiStepCmsSwap(const std::vector<Time>& rateTimes,
                                 const std::vector<Real>& cmsAccruals,
                                 const std::vector<Real>& floatingAccruals,
                                 const std::vector<Time>& paymentTimes,
                                 Size swapLength,
                                 Size swapStep,
                                 Real cmsMargin,
                                 Real floatMargin,
                                 Real cmsFloor,
                                 Real cmsCap,
                                 bool payer)
    : MultiProductMultiStep(rateTimes),
      cmsAccruals_(cmsAccruals), floatingAccruals_(floatingAccruals),
        paymentTimes_(paymentTimes), swapLength_(swapLength), swapStep_(swapStep),
        cmsMargin_(cmsMargin), floatMargin_(floatMargin),
        cmsFloor_(cmsFloor), cmsCap_(cmsCap),
        payer_(payer), multiplier_(payer ? 1.0 : -1.0), lastIndex_(rateTimes.size()-1) {
        
        // what is the requirement here ?
        // checkIncreasingTimes(paymentTimes);

    }

    bool MultiStepCmsSwap::nextTimeStep(
            const CurveState& currentState,
            std::vector<Size>& numberCashFlowsThisStep,
            std::vector<std::vector<MarketModelMultiProduct::CashFlow> >&
                                                                 genCashFlows)
    {

        Rate liborRate = currentState.forwardRate(currentIndex_);
        Rate swapRate = 0.0;
        if(currentIndex_ + swapLength_ <= lastIndex_)
            swapRate = currentState.swapRate(currentIndex_, currentIndex_ + swapLength_, swapStep_);

        if(cmsFloor_ != Null<Real>())
            swapRate = std::max(cmsFloor_, swapRate);
        if(cmsCap_ != Null<Real>())
            swapRate = std::min(cmsCap_, swapRate);
        
        genCashFlows[0][0].timeIndex = currentIndex_;
        genCashFlows[0][0].amount =
            -multiplier_*(swapRate+cmsMargin_)*cmsAccruals_[currentIndex_];

        genCashFlows[0][1].timeIndex = currentIndex_;
        genCashFlows[0][1].amount =
            multiplier_*(liborRate+floatMargin_)*floatingAccruals_[currentIndex_];

        numberCashFlowsThisStep[0] = 2;

        ++currentIndex_;

        return (currentIndex_ == lastIndex_);
    }

    std::unique_ptr<MarketModelMultiProduct> MultiStepCmsSwap::clone() const {
        return std::unique_ptr<MarketModelMultiProduct>(
                                                 new MultiStepCmsSwap(*this));
    }

}

