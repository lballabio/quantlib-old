
/*
 Copyright (C) 2004 Eric Ehlers

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

#include "objectstochastic.hpp"

ObjectStochastic::ObjectStochastic(
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate) {
    DayCounter dayCounter = Actual365Fixed();
	Handle<Quote> underlyingH( boost::shared_ptr<Quote>(new
        SimpleQuote(underlying)));
    Handle<YieldTermStructure> flatTermStructure(
    	boost::shared_ptr<YieldTermStructure>(
    	new FlatForward(settlementDate, riskFreeRate, dayCounter)));
	Handle<YieldTermStructure> flatDividendTS(
    	boost::shared_ptr<YieldTermStructure>(
    	new FlatForward(settlementDate, dividendYield, dayCounter)));
    Handle<BlackVolTermStructure> flatVolTS(
    	boost::shared_ptr<BlackVolTermStructure>(new
        BlackConstantVol(settlementDate, volatility, dayCounter)));
	stochasticProcess_ = boost::shared_ptr<BlackScholesProcess> (new
        BlackScholesProcess(
            underlyingH,
            flatDividendTS,
            flatTermStructure,
            flatVolTS));
}

ObjectStochastic::~ObjectStochastic() {
}

boost::shared_ptr<void> ObjectStochastic::getReference() const {
	return boost::static_pointer_cast<void>(stochasticProcess_);
}
