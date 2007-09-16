
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005 Eric Ehlers

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

#include <qlo/processes.hpp>

#include <ql/quote.hpp>
#include <ql/yieldtermstructures/flatforward.hpp>
#include <ql/voltermstructures/equityfx/blackvoltermstructure.hpp>

namespace QuantLibAddin {

    GeneralizedBlackScholesProcess::GeneralizedBlackScholesProcess(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr < QuantLib::BlackVolTermStructure > &blackVolTermStructureP,
            QuantLib::Real underlying,
            const QuantLib::DayCounter &dayCounter,
            const QuantLib::Date &settlementDate,
            QuantLib::Rate riskFreeRate,
            QuantLib::Spread dividendYield,
            bool permanent)
        : ObjectHandler::LibraryObject<QuantLib::GeneralizedBlackScholesProcess>(properties, permanent) {

        QuantLib::Handle<QuantLib::Quote> underlyingH( 
            boost::shared_ptr<QuantLib::Quote>(
            new QuantLib::SimpleQuote(underlying)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatTermStructure(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, riskFreeRate, dayCounter)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatDividendTS(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, dividendYield, dayCounter)));

        QuantLib::Handle<QuantLib::BlackVolTermStructure> 
            blackVolTermStructureH(blackVolTermStructureP);

        libraryObject_ = boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess> (
            new QuantLib::GeneralizedBlackScholesProcess(
                underlyingH,
                flatDividendTS,
                flatTermStructure,
                blackVolTermStructureH));
    }

}

