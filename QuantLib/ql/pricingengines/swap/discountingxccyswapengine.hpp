/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

	Copyright (C) 2013 Mehdi Bouassab
 
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

/*! \file discountingxccyswapengine.hpp
    \brief discounting xccyswap engine
*/

#ifndef quantlib_discounting_xccyswap_engine_hpp
#define quantlib_discounting_xccyswap_engine_hpp

#include <ql/instruments/swap.hpp>
#include <ql/instruments/crosscurrencyswap.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/handle.hpp>
#include <ql/quotes/simplequote.hpp>

namespace QuantLib {

    class DiscountingXccySwapEngine : public crosscurrencyswap::engine {
      public:
        DiscountingXccySwapEngine(
			   const Handle<YieldTermStructure>& floatDiscountcurve
			   =Handle<YieldTermStructure>(),
			   const Handle<YieldTermStructure>& fixedDiscountcurve
			   =Handle<YieldTermStructure>(),
			   const Handle<Quote>& FxSpot= Handle<Quote>(new SimpleQuote(1.0)),
               boost::optional<bool> includeSettlementDateFlows = boost::none,
               Date settlementDate = Date(),
               Date npvDate = Date());
        void calculate() const;
        Handle<YieldTermStructure> floatDiscountcurve() const {
            return floatDiscountcurve_;
        }
		Handle<YieldTermStructure> fixedDiscountcurve() const {
			return fixedDiscountcurve_;
		}
		
      private:
		Handle<YieldTermStructure> floatDiscountcurve_;
		Handle<YieldTermStructure> fixedDiscountcurve_;
		Handle<Quote> FxSpot_;
        boost::optional<bool> includeSettlementDateFlows_;
        Date settlementDate_, npvDate_;
    };

}

#endif
