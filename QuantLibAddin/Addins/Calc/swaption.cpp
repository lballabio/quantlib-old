
/*  
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco
 Copyright (C) 2006 Eric Ehlers
 
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

// This file was generated automatically by gensrc.py.  If you edit this file
// manually then your changes will be lost the next time gensrc runs.

// This source code file was generated from the following stub:
//      gensrc/gensrc/stubs/stub.calc.includes

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/all.hpp>
#include <qlo/conversions/all.hpp>
#include <oh/enumerations/typefactory.hpp>
#include <qlo/swaption.hpp>
#include <qlo/indexes/swapindex.hpp>
#include <qlo/vanillaswap.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/pricingengines.hpp>
#include <qlo/exercise.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/exercise.hpp>
#include <qlo/valueobjects/vo_swaption.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlMakeSwaption(
        const STRING &ObjectId,
        const STRING &SwapIndex,
        const STRING &OptionTenor,
        const ANY &Strike,
        const STRING &PricingEngineID,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string SwapIndexCpp = ouStringToStlString(SwapIndex);

        std::string OptionTenorCpp = ouStringToStlString(OptionTenor);

        double StrikeCpp;
        calcToScalar(StrikeCpp, Strike);

        std::string PricingEngineIDCpp = ouStringToStlString(PricingEngineID);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period OptionTenorLib;
        calcToScalar(OptionTenorLib, OptionTenor);

        // convert object IDs into library objects

        OH_GET_REFERENCE(SwapIndexLibObjPtr, SwapIndexCpp,
            QuantLibAddin::SwapIndex, QuantLib::SwapIndex)

        OH_GET_REFERENCE(PricingEngineIDLibObjPtr, PricingEngineIDCpp,
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeSwaption(
                ObjectIdCpp,
                SwapIndexCpp,
                OptionTenorCpp,
                StrikeCpp,
                PricingEngineIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Swaption(
                valueObject,
                SwapIndexLibObjPtr,
                OptionTenorLib,
                StrikeCpp,
                PricingEngineIDLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlMakeSwaption: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSwaption(
        const STRING &ObjectId,
        const STRING &VanillaSwap,
        const STRING &Exercise,
        const STRING &SettlementType,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string VanillaSwapCpp = ouStringToStlString(VanillaSwap);

        std::string ExerciseCpp = ouStringToStlString(Exercise);

        std::string SettlementTypeCpp = ouStringToStlString(SettlementType);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_REFERENCE(VanillaSwapLibObjPtr, VanillaSwapCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        OH_GET_REFERENCE(ExerciseLibObjPtr, ExerciseCpp,
            QuantLibAddin::EuropeanExercise, QuantLib::EuropeanExercise)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Settlement::Type SettlementTypeEnum =
            ObjectHandler::Create<QuantLib::Settlement::Type>()(SettlementTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSwaption(
                ObjectIdCpp,
                VanillaSwapCpp,
                ExerciseCpp,
                SettlementTypeCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Swaption(
                valueObject,
                VanillaSwapLibObjPtr,
                ExerciseLibObjPtr,
                SettlementTypeEnum,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwaption: " << e.what());
        THROW_RTE;
    }
}


