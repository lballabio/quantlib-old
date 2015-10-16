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

/*! \file lgmswaptionengine_ad.hpp
    \brief
*/

#ifndef quantlib_pricers_lgm_swaption_ad_hpp
#define quantlib_pricers_lgm_swaption_ad_hpp

#include <ql/instruments/swaption.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/experimental/models/lgm1.hpp>

namespace QuantLib {

//! LGM swaption engine with AD support
/*! \ingroup swaptionengines

    this class uses the QuantLibOAD library to generate
    sensitivities to the model parameters by automatic
    differentiation

    see Gaussian1dSwaptionEngine for other remarks

    the class could easily be generalized from Lgm1 to
    Lgm models (but then we would introduce a template
    parameter, which we don't want to for the first
    implementation)
*/

class LgmSwaptionEngineAD
    : public GenericModelEngine<Lgm1, Swaption::arguments, Swaption::results> {
  public:

    LgmSwaptionEngineAD(const boost::shared_ptr<Lgm1> &model,
                        const int integrationPoints = 64,
                        const Real stddevs = 7.0,
                        const Handle<YieldTermStructure> &discountCurve =
                            Handle<YieldTermStructure>())
        : GenericModelEngine<Lgm1, Swaption::arguments, Swaption::results>(
              model),
          integrationPoints_(integrationPoints), stddevs_(stddevs),
          discountCurve_(discountCurve) {

        if (!discountCurve_.empty())
            registerWith(discountCurve_);
    }

    void calculate() const;

  private:
    const int integrationPoints_;
    const Real stddevs_;
    const Handle<YieldTermStructure> discountCurve_;
};
}

#endif
