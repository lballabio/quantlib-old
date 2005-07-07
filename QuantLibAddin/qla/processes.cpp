
/*
 Copyright (C) 2005 Plamen Neykov
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
#include <qla/processes.hpp>
#include <qla/volatilities.hpp>
#include <qla/generalutils.hpp>
#include <qla/typefactory.hpp>
#include <ql/quote.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/voltermstructure.hpp>

namespace QuantLibAddin {

    BlackScholesProcess::BlackScholesProcess(ObjHandler::ArgumentStack &arguments) {
        double dividendYield        = OH_POP_ARGUMENT(double, arguments);
        double riskFreeRate         = OH_POP_ARGUMENT(double, arguments);
        long settlementDateLong     = OH_POP_ARGUMENT(long, arguments);
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        double underlying           = OH_POP_ARGUMENT(double, arguments);
        std::string handleBlackVol  = OH_POP_ARGUMENT(std::string, arguments);

        QuantLib::Date settlementDate(settlementDateLong);
		QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        QuantLib::Handle<QuantLib::Quote> underlyingH( 
            boost::shared_ptr<QuantLib::Quote>(
            new QuantLib::SimpleQuote(underlying)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatTermStructure(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, riskFreeRate, dayCounter)));
        QuantLib::Handle<QuantLib::YieldTermStructure> flatDividendTS(
            boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::FlatForward(settlementDate, dividendYield, dayCounter)));

        boost::shared_ptr<BlackVolTermStructure> blackVolTermStructure =
            OH_GET_OBJECT(BlackVolTermStructure, handleBlackVol);
        if (!blackVolTermStructure)
            QL_FAIL("BlackScholesProcess: error retrieving object " + handleBlackVol);
        boost::shared_ptr<QuantLib::BlackVolTermStructure> blackVolTermStructureP = 
            OH_GET_REFERENCE(QuantLib::BlackVolTermStructure, blackVolTermStructure);
        QuantLib::Handle<QuantLib::BlackVolTermStructure> 
            blackVolTermStructureH(blackVolTermStructureP);

        blackScholesProcess_ = boost::shared_ptr<QuantLib::BlackScholesProcess> (
            new QuantLib::BlackScholesProcess(
                underlyingH,
                flatDividendTS,
                flatTermStructure,
                blackVolTermStructureH));
    }

}
