#include "objectstochastic.hpp"

ObjectStochastic::ObjectStochastic(
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate) {
    DayCounter rateDayCounter = Actual365Fixed();
	Handle<Quote> underlyingH( boost::shared_ptr<Quote>(new SimpleQuote(underlying)));
    Handle<YieldTermStructure> flatTermStructure(
    	boost::shared_ptr<YieldTermStructure>(
    	new FlatForward(settlementDate, riskFreeRate, rateDayCounter)));
	Handle<YieldTermStructure> flatDividendTS(
    	boost::shared_ptr<YieldTermStructure>(
    	new FlatForward(settlementDate, dividendYield, rateDayCounter)));
    Handle<BlackVolTermStructure> flatVolTS(
    	boost::shared_ptr<BlackVolTermStructure>( new BlackConstantVol(settlementDate, volatility)));
    boost::shared_ptr<BlackScholesProcess> temp(new
        BlackScholesProcess(
            underlyingH,
            flatDividendTS,
            flatTermStructure,
            flatVolTS));
	stochasticProcess_ = temp;
}

ObjectStochastic::~ObjectStochastic()
{
}
