/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Mark Joshi
 Copyright (C) 2005, 2006 Klaus Spanderen

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/reference/license.html>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/


#ifndef quantlib_exp_corr_abcd_vol_hpp
#define quantlib_exp_corr_abcd_vol_hpp

#include <ql/marketmodels/marketmodel.hpp>
#include <ql/marketmodels/evolutiondescription.hpp>
#include <vector>

namespace QuantLib
{
    class Matrix;
    class ExpCorrAbcdVol : public MarketModel {
      public:
        ExpCorrAbcdVol(
            Real a,
            Real b,
            Real c,
            Real d,
            const std::vector<Real>& ks,
            const Matrix& correlations,
            const EvolutionDescription& evolution,
            const Size numberOfFactors,
            const std::vector<Rate>& initialRates,
            const std::vector<Spread>& displacements);
        //! \name MarketModel interface
        //@{
        const std::vector<Rate>& initialRates() const;
        const std::vector<Spread>& displacements() const;
        const EvolutionDescription& evolution() const;
        Size numberOfRates() const;
        Size numberOfFactors() const;
        Size numberOfSteps() const; 
        const Matrix& pseudoRoot(Size i) const;
        const Matrix& covariance(Size i) const;
        const Matrix& totalCovariance(Size endIndex) const;
        //@}
      private:
        Size numberOfFactors_, numberOfRates_, numberOfSteps_;
        std::vector<Rate> initialRates_;
        std::vector<Spread> displacements_;
        EvolutionDescription evolution_;
        std::vector<Matrix> pseudoRoots_, covariance_, totalCovariance_;
    };

    // inline

    inline const std::vector<Rate>& ExpCorrAbcdVol::initialRates() const {
        return initialRates_;
    }

    inline const std::vector<Spread>& ExpCorrAbcdVol::displacements() const {
        return displacements_;
    }

    inline const EvolutionDescription& ExpCorrAbcdVol::evolution() const {
        return evolution_;
    }

    inline Size ExpCorrAbcdVol::numberOfRates() const {
        return numberOfRates_;
    }

    inline Size ExpCorrAbcdVol::numberOfFactors() const {
        return numberOfFactors_;
    }

    inline Size ExpCorrAbcdVol::numberOfSteps() const {
        return numberOfSteps_;
    }

    inline const Matrix& ExpCorrAbcdVol::pseudoRoot(Size i) const {
        return pseudoRoots_[i];
    }

    inline const Matrix& ExpCorrAbcdVol::covariance(Size i) const {
        return covariance_[i];
    }

    inline const Matrix& ExpCorrAbcdVol::totalCovariance(Size endIndex) const {
        return totalCovariance_[endIndex];
    }

}

#endif
