/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file cclgm1.hpp
    \brief multicurrency lgm model with piecewise parameters
*/

#ifndef quantlib_multicurrency_lgm1_hpp
#define quantlib_multicurrency_lgm1_hpp

#include <ql/experimental/models/cclgm.hpp>
#include <ql/experimental/models/cclgmparametrization.hpp>
#include <ql/experimental/models/lgmpiecewisealphaconstantkappa.hpp>
#include <ql/experimental/models/lgmfxpiecewisesigma.hpp>

namespace QuantLib {

class CcLgm1 : public CcLgm<CcLgmPiecewise, LgmFxPiecewiseSigma,
                            LgmPiecewiseAlphaConstantKappa>,
               public CalibratedModel {
  public:
    CcLgm1(const std::vector<boost::shared_ptr<Lgm<ImplLgm> > > &models,
           const std::vector<Real> &fxSpots,
           const std::vector<Date> &fxVolStepDates,
           const std::vector<std::vector<Real> > &fxVolatilities,
           const std::vector<Handle<YieldTermStructure> > &fxCurves =
               std::vector<Handle<YieldTermStructure> >());

  private:
    void updateTimes() const;
    void initialize();

    void update() {
        CcLgm<CcLgmPiecewise, LgmFxPiecewiseSignma,
              LgmPiecewiseAlphaConstantKappa>::update();
        updateTimes();
    }

    const std::vector<Real> fxSpots_;
    const std::vector<Date> fxVolStepDates_;
    const std::vector<std::vector<Real> > fxVolatilities_;
    const Matrix correlation_;
    std::vector<Handle<YieldTermStructure> > curves_;

    mutable std::vector<Time> fxVolStepTimes_;
    mutable Array fxVolStepTimesArray_;
};

} // namespace QuantLib

#endif
