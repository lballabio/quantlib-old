/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
  Copyright (C) 2013 Peter Caspers

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

/*! \file creditriskplus.hpp
    \brief Extended CreditRisk+ Model
*/

#ifndef quantlib_creditriskplus_hpp
#define quantlib_creditriskplus_hpp

#include <ql/qldefines.hpp>
#include <ql/types.hpp>
#include <ql/math/matrix.hpp>
#include <vector>

namespace QuantLib {

    /*! Extended CreditRisk+ model as described in [1] Integrating Correlations, Risk,
      July 1999 and the references therein.

      \warning the input correlation matrix is not checked for positive
      definiteness

    */

    class CreditRiskPlus {

      public:

        CreditRiskPlus(const std::vector<Real> &exposure,
                       const std::vector<Real> &defaultProbability,
                       const std::vector<Size> &sector,
                       const std::vector<Real> &relativeDefaultVariance,
                       const Matrix &correlation, const Real unit);

        const std::vector<Real> &loss() { return loss_; }
        const std::vector<Real> &marginalLoss() { return marginalLoss_; }

        Real exposure() { return exposureSum_; }
        Real expectedLoss() const { return el_; }
        Real unexpectedLoss() { return ul_; }
        Real relativeDefaultVariance() {
            return (unexpectedLoss() * unexpectedLoss() - el2_) /
                   (expectedLoss() * expectedLoss());
        }

        const std::vector<Real> &sectorExposures() const {
            return sectorExposure_;
        }
        const std::vector<Real> &sectorExpectedLoss() const {
            return sectorEl_;
        }
        const std::vector<Real> &sectorUnexpectedLoss() const {
            return sectorUl_;
        }

        Real lossQuantile(const Real p);

      private:

        const std::vector<Real> exposure_;
        const std::vector<Real> pd_;
        const std::vector<Size> sector_;
        const std::vector<Real> relativeDefaultVariance_;
        const Matrix correlation_;
        const Real unit_;

        Size n_, m_; // number of sectors, exposures

        std::vector<Real> sectorExposure_, sectorEl_, sectorUl_, marginalLoss_,
            loss_;

        Real exposureSum_, el_, el2_, ul_;
        unsigned long upperIndex_;

        void compute();
    };
}

#endif
