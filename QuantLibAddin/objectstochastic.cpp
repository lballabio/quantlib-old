#include "objectstochastic.hpp"

ObjectStochastic::ObjectStochastic(
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate) {
    DayCounter rateDayCounter = Actual365();
	RelinkableHandle<Quote> underlyingH( boost::shared_ptr<Quote>(new SimpleQuote(underlying)));
    RelinkableHandle<TermStructure> flatTermStructure(
    	boost::shared_ptr<TermStructure>(
    	new FlatForward(todaysDate, settlementDate, riskFreeRate, rateDayCounter)));
	RelinkableHandle<TermStructure> flatDividendTS(
    	boost::shared_ptr<TermStructure>(
    	new FlatForward(todaysDate, settlementDate, dividendYield, rateDayCounter)));
    RelinkableHandle<BlackVolTermStructure> flatVolTS(
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
