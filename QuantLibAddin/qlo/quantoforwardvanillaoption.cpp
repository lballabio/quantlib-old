
/*
 Copyright (C) 2006 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/quantoforwardvanillaoption.hpp>
#include <qlo/exercise.hpp>
#include <qlo/volatilities.hpp>

namespace QuantLibAddin {

    QuantoForwardVanillaOption::QuantoForwardVanillaOption(
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const boost::shared_ptr < QuantLib::BlackVolTermStructure > &blackVolTermStructure,
            const double &correlation,
            const double &moneyness,
            QuantLib::Date resetDate,
            const boost::shared_ptr < QuantLib::GeneralizedBlackScholesProcess > &blackScholesProcess,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff> &payoff,
            const boost::shared_ptr < QuantLib::Exercise > &exercise,
            const boost::shared_ptr<QuantLib::PricingEngine> &pricingEngine) {
        QuantLib::Handle<QuantLib::BlackVolTermStructure>
            blackVolTermStructureH(blackVolTermStructure);

        QuantLib::Handle<QuantLib::Quote> correlationH(
            boost::shared_ptr<QuantLib::Quote>(
            new QuantLib::SimpleQuote(correlation)));

        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::QuantoForwardVanillaOption(
                hYTS,
                blackVolTermStructureH,
                correlationH,
                moneyness,
                QuantLib::Date(resetDate),
                blackScholesProcess,
                payoff,
                exercise,
                pricingEngine));
    }

}

