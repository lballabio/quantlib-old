
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

#include <QuantLibAddin/objectoption.hpp>

ObjectOption::ObjectOption(boost::shared_ptr<ObjectStochastic> objectStochastic,
		const string &typeString,
		const Real &strike,
		const Size &timeSteps,
		const Date &exerciseDate,
		const Date &settlementDate) {
	Option::Type type;
	string typeUpper = toUpper(typeString);
	if ((typeUpper.compare("P") == 0) || (typeUpper.compare("PUT") == 0))
		type = Option::Put;
	else if ((typeUpper.compare("C") == 0) || (typeUpper.compare("CALL") == 0))
		type = Option::Call;
	else
		QL_FAIL("ObjectOption constructor: unrecognized option type: " + typeString);
    boost::shared_ptr<Exercise> amExercise( 
		new AmericanExercise(settlementDate, exerciseDate));
    boost::shared_ptr<StrikedTypePayoff> payoff(
		new PlainVanillaPayoff(type, strike));
	boost::shared_ptr<PricingEngine> pricingEngine( 
		new BinomialVanillaEngine<JarrowRudd>(timeSteps));
	const boost::shared_ptr<BlackScholesProcess> stochasticProcess = 
		boost::static_pointer_cast<BlackScholesProcess>
		(objectStochastic->getReference());
    boost::shared_ptr<VanillaOption> temp( 
		new VanillaOption(stochasticProcess, payoff, 
			amExercise, pricingEngine));
	vanillaOption_ = temp;
	any_ptr any_npv(new boost::any(vanillaOption_->NPV()));
	any_ptr any_engine(new boost::any(string(BINOMIAL_JARROW_RUDD)));
	ObjectProperty prop_npv(FIELD_NPV, any_npv);
	ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
	properties_.push_back(prop_npv);
	properties_.push_back(prop_engine);
}

ObjectOption::~ObjectOption() {
}

void ObjectOption::setEngine(
		const std::string &engineName,
		const Size &timeSteps) {
	string engineUpper = toUpper(engineName);
	if (engineUpper.compare(BINOMIAL_JARROW_RUDD) == 0) 	{
		boost::shared_ptr<PricingEngine> pricingEngine( 
			new BinomialVanillaEngine<JarrowRudd>(timeSteps));
        vanillaOption_->setPricingEngine(pricingEngine);
	} else if (engineUpper.compare(BINOMIAL_COX_ROSS_RUBINSTEIN) == 0) 	{
		boost::shared_ptr<PricingEngine> pricingEngine( 
			new BinomialVanillaEngine<CoxRossRubinstein>(timeSteps));
        vanillaOption_->setPricingEngine(pricingEngine);
	} else if (engineUpper.compare(ADDITIVE_EQUIPROBABILITIES) == 0) 	{
		boost::shared_ptr<PricingEngine> pricingEngine( 
			new BinomialVanillaEngine<AdditiveEQPBinomialTree>(timeSteps));
        vanillaOption_->setPricingEngine(pricingEngine);
	} else
		QL_FAIL("setOptionEngine: unrecognized engine name: " + engineName);
	*properties_[IDX_NPV]() = vanillaOption_->NPV();
	*properties_[IDX_ENGINE]() = engineUpper;
}

boost::shared_ptr<void> ObjectOption::getReference() const {
	return boost::static_pointer_cast<void>(vanillaOption_);
}
