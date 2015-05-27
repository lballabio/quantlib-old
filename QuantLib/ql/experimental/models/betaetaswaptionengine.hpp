/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers, Roland Lichters

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

/*! \file betaetaswaptionengine.hpp
    \brief beta eta model swaption engine
*/

#ifndef quantlib_pricers_betaeta_swaption_hpp
#define quantlib_pricers_betaeta_swaption_hpp

#include <ql/instruments/swaption.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/experimental/models/betaeta.hpp>

namespace QuantLib {

//! beta eta model swaption engine
/*! \ingroup swaptionengines

     All fixed coupons with start date greater or equal to the respective
    option expiry are considered to be
    part of the exercise into right.

    \warning Cash settled swaptions are not supported
*/

class BetaEtaSwaptionEngine
    : public GenericModelEngine<BetaEta, Swaption::arguments,
                                Swaption::results> {

    BetaEtaSwaptionEngine(const boost::shared_ptr<BetaEta> &model,
                          const int gridPoints = 64, const Real stddevs = 6.0,
                          const Handle<YieldTermStructure> &discountCurve =
                              Handle<YieldTermStructure>())
        : GenericModelEngine<BetaEta, Swaption::arguments, Swaption::results>(
              model),
          gridPoints_(gridPoints), stddevs_(stddevs),
          discountCurve_(discountCurve) {}

    void calculate() const;

  private:
    const int gridPoints_;
    const Real stddevs_;
    const Handle<YieldTermStructure> discountCurve_;
};

} // namesapce QuantLib

#endif
