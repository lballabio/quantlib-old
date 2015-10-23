/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Mark Joshi

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

#include <ql/models/marketmodels/callability/custombasissystem.hpp>
#include <ql/models/marketmodels/curvestate.hpp>
#include <ql/models/marketmodels/utilities.hpp>

namespace QuantLib {

    CustomBasisSystem::CustomBasisSystem(const std::vector<Time>& rateTimes,
                        const std::vector<Time>& exerciseTimes,
                                         const std::vector<boost::function<Size(Size)> >& startByCurrentIndex,
                                         const std::vector<boost::function<Size(Size)> >& endByCurrentIndex,
                                         const Size step)
    : rateTimes_(rateTimes), exerciseTimes_(exerciseTimes),
      startByCurrentIndex_(startByCurrentIndex), endByCurrentIndex_(endByCurrentIndex),
      evolution_(rateTimes, exerciseTimes),  step_(step) {

        QL_REQUIRE(startByCurrentIndex_.size() == endByCurrentIndex_.size(),
                   "startByCurrentIndex (" << startByCurrentIndex_.size() << ") must have same size as" <<
                   "endByCurrentIndex (" << endByCurrentIndex_.size() << ")");

    }

    Size CustomBasisSystem::numberOfExercises() const {
        return exerciseTimes_.size();
    }

    std::vector<Size> CustomBasisSystem::numberOfFunctions() const {
        return std::vector<Size>(numberOfExercises(),startByCurrentIndex_.size() + 1);
    }

    const EvolutionDescription& CustomBasisSystem::evolution() const {
        return evolution_;
    }

    void CustomBasisSystem::nextStep(const CurveState&) {
        ++currentIndex_;
    }

    void CustomBasisSystem::reset() {
        currentIndex_ = 0;
    }

    std::valarray<bool> CustomBasisSystem::isExerciseTime() const {
        return std::valarray<bool>(true, exerciseTimes_.size());
    }

    void CustomBasisSystem::values(const CurveState& currentState,
                                 std::vector<Real>& results) const {
        results.reserve(startByCurrentIndex_.size()+1);
        results.resize(1);
        results[0] = 1.0;
        //        std::cout << "Custom basis system:" << std::endl;
        for(Size i=0;i<startByCurrentIndex_.size();i++) {
            results.push_back(currentState.swapRate(startByCurrentIndex_[i](currentIndex_),
                                                    endByCurrentIndex_[i](currentIndex_),
                                                    step_));
            //            std::cout << "rate (" << startByCurrentIndex_[i](currentIndex_) << ";" <<
            //    endByCurrentIndex_[i](currentIndex_) << ";" << step_ << ") " << results.back() << std::endl;
        }
        
    }

    std::unique_ptr<MarketModelBasisSystem> CustomBasisSystem::clone() const {
        return std::unique_ptr<MarketModelBasisSystem>(
                                                  new CustomBasisSystem(*this));
    }

}

