
/*
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
#include <qla/barrieroption.hpp>
#include <qla/optionutils.hpp>

namespace QuantLibAddin {

    QuantLib::Barrier::Type IDtoBarrierType(const std::string &typeBarrier) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(typeBarrier);
        if (idUpper.compare("DOWNIN") ==0)
            return QuantLib::Barrier::DownIn;
        else if (idUpper.compare("UPIN") == 0)
            return QuantLib::Barrier::UpIn;
        else if (idUpper.compare("DOWNOUT") == 0)
            return QuantLib::Barrier::DownOut;
        else if (idUpper.compare("UPOUT") == 0)
            return QuantLib::Barrier::UpOut;
        else
            QL_FAIL("IDtoBarrierType: unrecognized typeID: " + typeBarrier);
    }

    BarrierOption::BarrierOption(ObjHandler::ArgStack &args) {
        long timeSteps                  = ObjHandler::Args<long>::popArg(args);
        std::string engineID            = ObjHandler::Args<std::string>::popArg(args);
        long settlementDate             = ObjHandler::Args<long>::popArg(args);
        long exerciseDate               = ObjHandler::Args<long>::popArg(args);
        std::string exerciseID          = ObjHandler::Args<std::string>::popArg(args);
        double strike                   = ObjHandler::Args<double>::popArg(args);
        std::string payoffID            = ObjHandler::Args<std::string>::popArg(args);
        std::string optionTypeID        = ObjHandler::Args<std::string>::popArg(args);
        double rebate                   = ObjHandler::Args<double>::popArg(args);
        double barrier                  = ObjHandler::Args<double>::popArg(args);
        std::string typeBarrier         = ObjHandler::Args<std::string>::popArg(args);
        std::string handleBlackScholes  = ObjHandler::Args<std::string>::popArg(args);

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            boost::dynamic_pointer_cast<BlackScholesProcess>
            (QL_OBJECT_GET(handleBlackScholes));
        if (!blackScholesProcess)
            QL_FAIL("BarrierOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (blackScholesProcess->getReference());

        QuantLib::Barrier::Type barrierType = 
            IDtoBarrierType(typeBarrier);
        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        barrierOption_ = boost::shared_ptr<QuantLib::BarrierOption>(
            new QuantLib::BarrierOption(
                barrierType,
                barrier,
                rebate,
                blackScholesProcessQL, 
                payoff, 
                exercise, 
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(barrierOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

}

