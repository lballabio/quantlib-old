
/*  
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 
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
#include <qlo/vanillaswap.hpp>
#include <qlo/indexes/iborindex.hpp>
#include <qlo/indexes/swapindex.hpp>
#include <qlo/ratehelpers.hpp>
#include <qlo/schedule.hpp>
#include <qlo/termstructures.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <qlo/valueobjects/vo_vanillaswap.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlMakeVanillaSwap(
        const STRING &ObjectId,
        const STRING &SwapTenor,
        const STRING &IborIndex,
        const ANY &FixedRate,
        const STRING &ForwardStart,
        const ANY &FixDayCounter,
        const ANY &Spread,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string SwapTenorCpp = ouStringToStlString(SwapTenor);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        double FixedRateCpp;
        calcToScalar(FixedRateCpp, FixedRate);

        std::string ForwardStartCpp = ouStringToStlString(ForwardStart);

        std::string FixDayCounterCpp;
        calcToScalar(FixDayCounterCpp, FixDayCounter);

        double SpreadCpp;
        calcToScalar(SpreadCpp, Spread);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period SwapTenorLib;
        calcToScalar(SwapTenorLib, SwapTenor);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter FixDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixDayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeVanillaSwap(
                ObjectIdCpp,
                SwapTenorCpp,
                IborIndexCpp,
                FixedRateCpp,
                ForwardStartCpp,
                FixDayCounterCpp,
                SpreadCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::VanillaSwap(
                valueObject,
                SwapTenorLib,
                IborIndexLibObjPtr,
                FixedRateCpp,
                ForwardStartLib,
                FixDayCounterEnum,
                SpreadCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlMakeVanillaSwap: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlVanillaSwap(
        const STRING &ObjectId,
        const ANY &PayerReceiver,
        const ANY &Nominal,
        const STRING &FixSchedule,
        const ANY &FixedRate,
        const STRING &FixDayCounter,
        const STRING &FloatingLegSchedule,
        const STRING &IborIndex,
        const ANY &Spread,
        const STRING &FloatingLegDayCounter,
        const ANY &PaymentConvention,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string PayerReceiverCpp;
        calcToScalar(PayerReceiverCpp, PayerReceiver);

        double NominalCpp;
        calcToScalar(NominalCpp, Nominal);

        std::string FixScheduleCpp = ouStringToStlString(FixSchedule);

        double FixedRateCpp;
        calcToScalar(FixedRateCpp, FixedRate);

        std::string FixDayCounterCpp = ouStringToStlString(FixDayCounter);

        std::string FloatingLegScheduleCpp = ouStringToStlString(FloatingLegSchedule);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        double SpreadCpp;
        calcToScalar(SpreadCpp, Spread);

        std::string FloatingLegDayCounterCpp = ouStringToStlString(FloatingLegDayCounter);

        std::string PaymentConventionCpp;
        calcToScalar(PaymentConventionCpp, PaymentConvention);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_REFERENCE(FixScheduleLibObjPtr, FixScheduleCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        OH_GET_REFERENCE(FloatingLegScheduleLibObjPtr, FloatingLegScheduleCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::VanillaSwap::Type PayerReceiverEnum =
            ObjectHandler::Create<QuantLib::VanillaSwap::Type>()(PayerReceiverCpp);

        QuantLib::DayCounter FixDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixDayCounterCpp);

        QuantLib::DayCounter FloatingLegDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FloatingLegDayCounterCpp);

        QuantLib::BusinessDayConvention PaymentConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentConventionCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlVanillaSwap(
                ObjectIdCpp,
                PayerReceiverCpp,
                NominalCpp,
                FixScheduleCpp,
                FixedRateCpp,
                FixDayCounterCpp,
                FloatingLegScheduleCpp,
                IborIndexCpp,
                SpreadCpp,
                FloatingLegDayCounterCpp,
                PaymentConventionCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::VanillaSwap(
                valueObject,
                PayerReceiverEnum,
                NominalCpp,
                FixScheduleLibObjPtr,
                FixedRateCpp,
                FixDayCounterEnum,
                FloatingLegScheduleLibObjPtr,
                IborIndexLibObjPtr,
                SpreadCpp,
                FloatingLegDayCounterEnum,
                PaymentConventionEnum,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwap: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFairRate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->fairRate();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFairRate: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFairSpread(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->fairSpread();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFairSpread: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFixedLegBPS(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->fixedLegBPS();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFixedLegBPS: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFixedLegNPV(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->fixedLegNPV();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFixedLegNPV: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFloatingLegBPS(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->floatingLegBPS();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFloatingLegBPS: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlVanillaSwapFloatingLegNPV(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->floatingLegNPV();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVanillaSwapFloatingLegNPV: " << e.what());
        THROW_RTE;
    }
}


