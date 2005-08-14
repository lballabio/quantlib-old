
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
#include <qla/vanillaoption.hpp>
#include <qla/typefactory.hpp>

#include <ql/DayCounters/all.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/PricingEngines/all.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_NPV                       "NPV"
#define FIELD_ENGINE                    "ENGINE"
#define IDX_NPV                         0
#define IDX_ENGINE                      1

#define FIELD0 "FIELD0"
#define FIELD1 "FIELD1"
#define FIELD2 "FIELD2"
#define FIELD3 "FIELD3"
#define FIELD4 "FIELD4"
#define FIELD5 "FIELD5"
#define FIELD6 "FIELD6"
#define FIELD7 "FIELD7"

namespace QuantLibAddin {

    VanillaOption::VanillaOption(
            const std::string &handleBlackScholes,
            const std::string &optionTypeID,
            const std::string &payoffID,
            const double &strike,
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engineID,
            const long &timeSteps) {

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            OH_GET_OBJECT(BlackScholesProcess, handleBlackScholes);
        if (!blackScholesProcess)
            QL_FAIL("VanillaOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            OH_GET_REFERENCE(QuantLib::BlackScholesProcess, blackScholesProcess);

        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            Create<boost::shared_ptr<QuantLib::StrikedTypePayoff> >()(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            Create<boost::shared_ptr<QuantLib::Exercise> >()(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, timeSteps);
        vanillaOption_ = boost::shared_ptr<QuantLib::VanillaOption>(
            new QuantLib::VanillaOption(
                blackScholesProcessQL, 
                payoff, 
                exercise, 
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(vanillaOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

    const ObjHandler::Properties& VanillaOption::setEngine(
            const std::string &engineID,
            const long &timeSteps) {
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, timeSteps);
        vanillaOption_->setPricingEngine(pricingEngine);
        *properties_[IDX_NPV]() = vanillaOption_->NPV();
        *properties_[IDX_ENGINE]() = engineID;
        return properties_;
    }

    VanillaOption2::VanillaOption2(
        const double &underlying,
        const long &settlementDateLong,
        const long &exerciseDateLong,
        const double &riskFreeRate,
        const double &dividendYield,
        const double &volatility,
        const double &strike,
        const long &timeSteps) {

    QuantLib::Date settlementDate(settlementDateLong);
    QuantLib::Date exerciseDate(exerciseDateLong);
    QuantLib::DayCounter dayCounter = QuantLib::Actual360();
    QuantLib::Handle<QuantLib::BlackVolTermStructure> blackVolTermStructure(
        boost::shared_ptr<QuantLib::BlackVolTermStructure> (
        new QuantLib::BlackConstantVol(settlementDate, volatility, dayCounter)));

    QuantLib::Handle<QuantLib::Quote> underlyingH( 
        boost::shared_ptr<QuantLib::Quote>(
        new QuantLib::SimpleQuote(underlying)));
    QuantLib::Handle<QuantLib::YieldTermStructure> flatTermStructure(
        boost::shared_ptr<QuantLib::YieldTermStructure>(
        new QuantLib::FlatForward(settlementDate, riskFreeRate, dayCounter)));
    QuantLib::Handle<QuantLib::YieldTermStructure> flatDividendTS(
        boost::shared_ptr<QuantLib::YieldTermStructure>(
        new QuantLib::FlatForward(settlementDate, dividendYield, dayCounter)));

    boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcess = 
        boost::shared_ptr<QuantLib::BlackScholesProcess> (
            new QuantLib::BlackScholesProcess(
                underlyingH,
                flatDividendTS,
                flatTermStructure,
                blackVolTermStructure));

    QuantLib::Option::Type type = QuantLib::Option::Put;

    boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff (
        new QuantLib::PlainVanillaPayoff(type, strike));
    boost::shared_ptr<QuantLib::Exercise> exercise(
        new QuantLib::AmericanExercise(settlementDate, exerciseDate));

    boost::shared_ptr<QuantLib::PricingEngine> pricingEngine(
        new QuantLib::BinomialVanillaEngine<QuantLib::CoxRossRubinstein>(timeSteps));

        vanillaOption_ = boost::shared_ptr<QuantLib::VanillaOption>(
        new QuantLib::VanillaOption(
            blackScholesProcess, 
            payoff, 
            exercise, 
            pricingEngine));

        ObjHandler::any_ptr any_npv(new boost::any(vanillaOption_->NPV()));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        properties_.push_back(prop_npv);
    }


    VanillaOption3::VanillaOption3(
        const double &underlying,
        const long &settlementDateLong,
        const long &exerciseDateLong,
        const double &riskFreeRate,
        const double &dividendYield,
        const double &volatility,
        const double &strike,
        const long &timeSteps) {

        ObjHandler::any_ptr any0(new boost::any(timeSteps));
        ObjHandler::any_ptr any1(new boost::any(strike));
        ObjHandler::any_ptr any2(new boost::any(volatility));
        ObjHandler::any_ptr any3(new boost::any(dividendYield));
        ObjHandler::any_ptr any4(new boost::any(riskFreeRate));
        ObjHandler::any_ptr any5(new boost::any(exerciseDateLong));
        ObjHandler::any_ptr any6(new boost::any(settlementDateLong));
        ObjHandler::any_ptr any7(new boost::any(underlying));

        ObjHandler::ObjectProperty prop0(FIELD0, any0);
        ObjHandler::ObjectProperty prop1(FIELD1, any1);
        ObjHandler::ObjectProperty prop2(FIELD2, any2);
        ObjHandler::ObjectProperty prop3(FIELD3, any3);
        ObjHandler::ObjectProperty prop4(FIELD4, any4);
        ObjHandler::ObjectProperty prop5(FIELD5, any5);
        ObjHandler::ObjectProperty prop6(FIELD6, any6);
        ObjHandler::ObjectProperty prop7(FIELD7, any7);

        properties_.push_back(prop0);
        properties_.push_back(prop1);
        properties_.push_back(prop2);
        properties_.push_back(prop3);
        properties_.push_back(prop4);
        properties_.push_back(prop5);
        properties_.push_back(prop6);
        properties_.push_back(prop7);
    }

}

