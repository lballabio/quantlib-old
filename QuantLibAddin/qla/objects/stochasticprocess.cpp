
/*
 Copyright (C) 2004, 2005 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/objects/stochasticprocess.hpp>

#include <ql/DayCounters/all.hpp>
#include <ql/quote.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/basicdataformatters.hpp>

namespace QuantLibAddin {

    QuantLib::DayCounter IDtoDayCounter(const std::string &dayCounterID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(dayCounterID);
        if (idUpper.compare("ACT365FIX") ==0)
            return QuantLib::Actual365Fixed();
        else if (idUpper.compare("ACT360") == 0)
            return QuantLib::Actual360();
        else
            QL_FAIL("IDtoDayCounter: unrecognized dayCounterID: " + dayCounterID);
    }

    StochasticProcess::StochasticProcess(
            const double &underlying,
            const std::string &dayCounterID,
            const long &settlementDateLong,
            const double &riskFreeRate,
            const double &dividendYield,
            const double &volatility) {
        QuantLib::Date settlementDate(settlementDateLong);
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);
        QuantLib::Handle<QuantLib::Quote> underlyingH( 
            boost::shared_ptr<QuantLib::Quote>(
            new QuantLib::SimpleQuote(underlying)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatTermStructure(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, riskFreeRate, dayCounter)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatDividendTS(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, dividendYield, dayCounter)));
        QuantLib::Handle<QuantLib::BlackVolTermStructure> flatVolTS(
            boost::shared_ptr<QuantLib::BlackVolTermStructure>(
            new QuantLib::BlackConstantVol(settlementDate, volatility, dayCounter)));
        stochasticProcess_ = boost::shared_ptr<QuantLib::BlackScholesProcess> (
            new QuantLib::BlackScholesProcess(
                underlyingH,
                flatDividendTS,
                flatTermStructure,
                flatVolTS));
    }

}

