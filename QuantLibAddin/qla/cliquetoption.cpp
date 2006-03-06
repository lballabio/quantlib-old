
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Eric Ehlers

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
#include <qla/cliquetoption.hpp>
#include <qla/typefactory.hpp>
#include <qla/generalutils.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_NPV                       "NPV"
#define FIELD_ENGINE                    "ENGINE"
#define IDX_NPV                         0
#define IDX_ENGINE                      1

namespace QuantLibAddin {

    CliquetOption::CliquetOption(
            const std::string &handleBlackScholes,
            const std::vector < long > &resetDates,
            const std::string &optionTypeID,
            const double &strike,
            const long &exerciseDate,
            const std::string &engineID,
            const long &timeSteps) {

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            OH_GET_OBJECT(BlackScholesProcess, handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            OH_GET_REFERENCE(QuantLib::BlackScholesProcess, blackScholesProcess);

        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);

        boost::shared_ptr<QuantLib::PercentageStrikePayoff> payoff(
            new QuantLib::PercentageStrikePayoff(type, strike));
        boost::shared_ptr<QuantLib::EuropeanExercise> exercise(
            new QuantLib::EuropeanExercise(QuantLib::Date(exerciseDate)));
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, timeSteps);
        std::vector<QuantLib::Date> resetDatesQL =
            longVectorToDateVector(resetDates);
        mInstrument = boost::shared_ptr<QuantLib::CliquetOption>(
            new QuantLib::CliquetOption(
                blackScholesProcessQL, 
                payoff, 
                exercise, 
                resetDatesQL,
                pricingEngine));
        //createProperty(FIELD_NPV, mInstrument->NPV());
        //createProperty(FIELD_ENGINE, engineID);
    }

}

