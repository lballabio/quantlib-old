
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
	string typeUpper= toUpper(typeString);
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
    fieldNames_.push_back(FIELD_NPV);
    fieldNames_.push_back(FIELD_ENGINE);
	any_ptr npv(new boost::any(vanillaOption_->NPV()));
	any_ptr engine(new boost::any(string(BINOMIAL_JARROW_RUDD)));
    valueList_[FIELD_NPV] = npv;
    valueList_[FIELD_ENGINE] = engine;
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
	*valueList_[FIELD_NPV] = vanillaOption_->NPV();
	*valueList_[FIELD_ENGINE] = engineUpper;
}
