#ifndef objectstochastic_h
#define objectstochastic_h

#include "ObjectHandler/objecthandler.hpp"
#include <ql/quantlib.hpp>
using namespace QuantLib;

class ObjectStochastic : public Object {
public:
	ObjectStochastic(
		const Spread &dividendYield,
		const Rate &riskFreeRate,
		const Volatility &volatility,
		const Real &underlying,
		const Date &todaysDate,
		const Date &settlementDate);
	~ObjectStochastic();
	virtual const boost::shared_ptr<void> getReference() {
		return boost::static_pointer_cast<void>(stochasticProcess_);
	}
private:
	boost::shared_ptr<BlackScholesProcess> stochasticProcess_;
};

#endif
