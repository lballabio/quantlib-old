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

/*! \file proxynonstandardswaptionengine.hpp
    \brief proxy engine for non standard swaptions
*/

#ifndef quantlib_pricingengines_proxy_nonstandardswaptionengine_hpp
#define quantlib_pricingengines_proxy_nonstandardswaptionengine_hpp

#include <ql/instruments/nonstandardswaption.hpp>

namespace QuantLib {

/*! \warning the gaussian 1d model originally used for pricing must not
  use a floating reference date when using the proxy engine. Neither must
  the yield term structure of the model or any curve needed for pricing
  (like the optional discount curve or forwarding curves attached to the
  ibor coupons) use a floating reference date. The reason is that the proxy
  engine uses them and needs them to have the original reference date.
  The option adjusted spread from the original pricing is reused here,
  its value can not be changed. */

class ProxyNonstandardSwaptionEngine : public NonstandardSwaption::engine {

    /*! the reference rate and maturity are used to imply the original model's
        state; here the rate should be expressed as continuously compounded
        w.r.t. the original model's day counter. */

  public:
    ProxyNonstandardSwaptionEngine(
        boost::shared_ptr<ProxyInstrument::ProxyDescription> proxy,
        Handle<Quote> referenceRate, const Handle<Quote> referenceMaturity,
        const Size integrationPoints = 16, const Real stdDevs = 5.0,
        const bool includeTodaysExercise = false)
        : referenceRate_(referenceRate), referenceMaturity_(referenceMaturity),
          integrationPoints_(integrationPoints), stdDevs_(stdDevs),
          includeTodaysExercise_(includeTodaysExercise) {
        proxy_ = boost::dynamic_pointer_cast<NonstandardSwaption::Proxy>(proxy);
        QL_REQUIRE(proxy_, "no NonstandardSwaption::Proxy given");
        QL_REQUIRE(stdDevs > 0.0, "stdDevs (" << stdDevs
                                              << ") must be positive");
        registerWith(referenceRate);
        registerWith(referenceMaturity);
        registerWith(proxy_->model);
    }

    void calculate() const;

  private:
    boost::shared_ptr<NonstandardSwaption::Proxy> proxy_;
    Handle<Quote> referenceRate_, referenceMaturity_;
    const Size integrationPoints_;
    const Real stdDevs_;
    const bool includeTodaysExercise_;

}; // class ProxyNonstandardSwaptionEngine

} // namespace QuantLib

#endif
