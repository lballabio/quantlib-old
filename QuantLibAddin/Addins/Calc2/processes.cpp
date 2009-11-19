
/*  
 Copyright (C) 2004, 2005 Eric Ehlers
 
 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// This file was generated automatically by gensrc.py.
// Editing this file manually is not recommended.

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/Enumerations/Factories/all.hpp>
#include <qlo/Conversions/all.hpp>
#include <oh/Enumerations/typefactory.hpp>
#include <qlo/processes.hpp>
#include <qlo/volatilities.hpp>
#include <qlo/ValueObjects/vo_processes.hpp>

#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlGeneralizedBlackScholesProcess(
        const STRING &ObjectId,
        const STRING &BlackVolID,
        double Underlying,
        const ANY &DayCounter,
        const ANY &SettlementDate,
        double RiskFreeRate,
        double DividendYield,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw (RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string BlackVolIDCpp = ouStringToStlString(BlackVolID);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        ObjectHandler::property_t SettlementDateCpp;
        calcToScalar(SettlementDateCpp, SettlementDate);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date SettlementDateLib;
        calcToScalar(SettlementDateLib, SettlementDate);

        // convert object IDs into library objects

        OH_GET_REFERENCE(BlackVolIDLibObj, BlackVolIDCpp,
            QuantLibAddin::BlackVolTermStructure, QuantLib::BlackVolTermStructure)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlGeneralizedBlackScholesProcess(
                ObjectIdCpp,
                BlackVolIDCpp,
                Underlying,
                DayCounterCpp,
                SettlementDateCpp,
                RiskFreeRate,
                DividendYield,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::GeneralizedBlackScholesProcess(
                valueObject,
                BlackVolIDLibObj,
                Underlying,
                DayCounterEnum,
                SettlementDateLib,
                RiskFreeRate,
                DividendYield,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlGeneralizedBlackScholesProcess: " << e.what());
        THROW_RTE;
    }
}


