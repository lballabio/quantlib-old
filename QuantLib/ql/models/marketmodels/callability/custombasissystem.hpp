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


#ifndef quantlib_custom_basis_system_hpp
#define quantlib_custom_basis_system_hpp

#include <ql/models/marketmodels/callability/marketmodelbasissystem.hpp>
#include <ql/models/marketmodels/evolutiondescription.hpp>

#include <boost/function.hpp>

namespace QuantLib {

    class CustomBasisSystem : public MarketModelBasisSystem {
      public:
        CustomBasisSystem(const std::vector<Time>& rateTimes,
                        const std::vector<Time>& exerciseTimes,
                        // functions that take the exercise times index as an argument
                        // and return the start and end index of the desired swap rate
                        // the element 1 is always added to the basis
                        const std::vector<boost::function<Size(Size)> >& startByCurrentIndex,
                        const std::vector<boost::function<Size(Size)> >& endByCurrentIndex,
                        const Size step = 1);
        Size numberOfExercises() const;
        std::vector<Size> numberOfFunctions() const;
        const EvolutionDescription& evolution() const;
        void nextStep(const CurveState&);
        void reset();
        std::valarray<bool> isExerciseTime() const;
        void values(const CurveState&,
                    std::vector<Real>& results) const;
        std::unique_ptr<MarketModelBasisSystem> clone() const;
      private:
        std::vector<Time> rateTimes_, exerciseTimes_;
        Size currentIndex_;
        const std::vector<boost::function<Size(Size)> >& startByCurrentIndex_, endByCurrentIndex_;
        EvolutionDescription evolution_;
        const Size step_;
    };

}


#endif
