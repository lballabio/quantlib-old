#ifndef objectoption_h
#define objectoption_h

#include "ObjectHandler/objecthandler.hpp"
#include "objectstochastic.hpp"

// FIXME
#define FIELD_NPV 						"NPV"
#define FIELD_ENGINE 					"ENGINE"
#define BINOMIAL_JARROW_RUDD			"BINOMIAL JARROW-RUDD"
#define BINOMIAL_COX_ROSS_RUBINSTEIN	"BINOMIAL COX-ROSS-RUBINSTEIN"
#define ADDITIVE_EQUIPROBABILITIES		"ADDITIVE EQUIPROBABILITIES"

class ObjectOption : public Object {
public:
	ObjectOption(boost::shared_ptr<ObjectStochastic>,
		const string &typestr,
		const Real &strike,
		const Size &timeSteps,
		const Date &exerciseDate,
		const Date &settlementDate);
	~ObjectOption();
	void setEngine(const std::string &engineName,
		const Size &timeSteps);
	virtual const boost::shared_ptr<void> getReference() {
		return boost::static_pointer_cast<void>(vanillaOption_);
	}
private:
	boost::shared_ptr<VanillaOption> vanillaOption_;
};

#endif
