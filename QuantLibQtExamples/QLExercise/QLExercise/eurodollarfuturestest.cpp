
#include "eurodollarfuturestest.hpp"
#include "customutilities.hpp"

#include <ql/quotes/eurodollarfuturesquote.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/quotes/simplequote.hpp>

using namespace QuantLib;

void EurodollarFuturesTest::testEurodollarFutures()
{
	/*
	EurodollarFuturesImpliedStdDevQuote(const Handle<Quote>& forward,
	const Handle<Quote>& callPrice,
	const Handle<Quote>& putPrice,
	Real strike,
	Real guess = .15,
	Real accuracy = 1.0e-6,
	Natural maxIter = 100);
	*/

	Handle<Quote> forward(boost::shared_ptr<Quote>(new SimpleQuote(168.17)));
	Handle<Quote> callPrice(boost::shared_ptr<Quote>(new SimpleQuote(13.9/100.0)));
	Handle<Quote> putPrice(boost::shared_ptr<Quote>(new SimpleQuote(10.6/100.0)));
	Real strike = 100.0;

	EurodollarFuturesImpliedStdDevQuote edfimpstdev(forward, callPrice, putPrice, strike);

	std::cout << edfimpstdev.value() << std::endl;

}