
/*  
 Copyright (C) 2006, 2007, 2011, 2015 Ferdinando Ametrano
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.includes

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
#include <qlo/pricingengines.hpp>
#include <qlo/termstructures.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <qlo/valueobjects/vo_vanillaswap.hpp>

#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlMakeIMMSwap(
        const ANY &ObjectId,
        const ANY &SwapTenor,
        const ANY &IborIndex,
        const ANY &FixedRate,
        const ANY &FirstImmDate,
        const ANY &FixDayCounter,
        const ANY &Spread,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string SwapTenorCpp;
        calcToScalar(SwapTenorCpp, SwapTenor);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        double FixedRateCpp;
        if(FixedRate.hasValue()) 
            calcToScalar(FixedRateCpp, FixedRate);
        else
            FixedRateCpp = QuantLib::Null<QuantLib::Rate>();

        ObjectHandler::property_t FirstImmDateCpp;
        calcToScalar(FirstImmDateCpp, FirstImmDate);

        std::string FixDayCounterCpp;
        if(FixDayCounter.hasValue()) 
            calcToScalar(FixDayCounterCpp, FixDayCounter);
        else
            FixDayCounterCpp = "30/360 (Bond Basis)";

        double SpreadCpp;
        if(Spread.hasValue()) 
            calcToScalar(SpreadCpp, Spread);
        else
            SpreadCpp = 0.0;

        std::string PricingEngineIDCpp;
        calcToScalar(PricingEngineIDCpp, PricingEngineID);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period SwapTenorLib;
        calcToScalar(SwapTenorLib, SwapTenor);

        QuantLib::Date FirstImmDateLib;
        if(!FirstImmDate.hasValue() and typeid(QuantLib::Date())==typeid(QuantLib::Date())) 
            FirstImmDateLib = QuantLib::Date();
        else
            calcToScalar(FirstImmDateLib, FirstImmDate);

        // convert object IDs into library objects

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_REFERENCE(PricingEngineIDLibObjPtr, PricingEngineIDCpp,
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter FixDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixDayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeIMMSwap(
                ObjectIdCpp,
                SwapTenorCpp,
                IborIndexCpp,
                FixedRateCpp,
                FirstImmDateCpp,
                FixDayCounterCpp,
                SpreadCpp,
                PricingEngineIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::VanillaSwap(
                valueObject,
                SwapTenorLib,
                IborIndexLibObjPtr,
                FixedRateCpp,
                FirstImmDateLib,
                FixDayCounterEnum,
                SpreadCpp,
                PricingEngineIDLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite, valueObject);

        // Convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlMakeIMMSwap: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlMakeVanillaSwap(
        const ANY &ObjectId,
        const ANY &SettlDays,
        const ANY &SwapTenor,
        const ANY &IborIndex,
        const ANY &FixedRate,
        const ANY &ForwardStart,
        const ANY &FixDayCounter,
        const ANY &Spread,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        long SettlDaysCpp;
        calcToScalar(SettlDaysCpp, SettlDays);

        std::string SwapTenorCpp;
        calcToScalar(SwapTenorCpp, SwapTenor);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        double FixedRateCpp;
        if(FixedRate.hasValue()) 
            calcToScalar(FixedRateCpp, FixedRate);
        else
            FixedRateCpp = QuantLib::Null<QuantLib::Rate>();

        std::string ForwardStartCpp;
        calcToScalar(ForwardStartCpp, ForwardStart);

        std::string FixDayCounterCpp;
        if(FixDayCounter.hasValue()) 
            calcToScalar(FixDayCounterCpp, FixDayCounter);
        else
            FixDayCounterCpp = "DayCounter";

        double SpreadCpp;
        if(Spread.hasValue()) 
            calcToScalar(SpreadCpp, Spread);
        else
            SpreadCpp = 0.0;

        std::string PricingEngineIDCpp;
        calcToScalar(PricingEngineIDCpp, PricingEngineID);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Natural SettlDaysLib;
        calcToScalar(SettlDaysLib, SettlDays);

        QuantLib::Period SwapTenorLib;
        calcToScalar(SwapTenorLib, SwapTenor);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_REFERENCE(PricingEngineIDLibObjPtr, PricingEngineIDCpp,
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter FixDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixDayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeVanillaSwap(
                ObjectIdCpp,
                SettlDaysCpp,
                SwapTenorCpp,
                IborIndexCpp,
                FixedRateCpp,
                ForwardStartCpp,
                FixDayCounterCpp,
                SpreadCpp,
                PricingEngineIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::VanillaSwap(
                valueObject,
                SettlDaysLib,
                SwapTenorLib,
                IborIndexLibObjPtr,
                FixedRateCpp,
                ForwardStartLib,
                FixDayCounterEnum,
                SpreadCpp,
                PricingEngineIDLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite, valueObject);

        // Convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlMakeVanillaSwap: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwap(
        const ANY &ObjectId,
        const ANY &PayerReceiver,
        const ANY &Nominal,
        const ANY &FixSchedule,
        const ANY &FixedRate,
        const ANY &FixDayCounter,
        const ANY &FloatingLegSchedule,
        const ANY &IborIndex,
        const ANY &Spread,
        const ANY &FloatingLegDayCounter,
        const ANY &PaymentConvention,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PayerReceiverCpp;
        if(PayerReceiver.hasValue()) 
            calcToScalar(PayerReceiverCpp, PayerReceiver);
        else
            PayerReceiverCpp = "Payer";

        double NominalCpp;
        if(Nominal.hasValue()) 
            calcToScalar(NominalCpp, Nominal);
        else
            NominalCpp = 100;

        std::string FixScheduleCpp;
        calcToScalar(FixScheduleCpp, FixSchedule);

        double FixedRateCpp;
        if(FixedRate.hasValue()) 
            calcToScalar(FixedRateCpp, FixedRate);
        else
            FixedRateCpp = 0.0;

        std::string FixDayCounterCpp;
        calcToScalar(FixDayCounterCpp, FixDayCounter);

        std::string FloatingLegScheduleCpp;
        calcToScalar(FloatingLegScheduleCpp, FloatingLegSchedule);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        double SpreadCpp;
        if(Spread.hasValue()) 
            calcToScalar(SpreadCpp, Spread);
        else
            SpreadCpp = 0.0;

        std::string FloatingLegDayCounterCpp;
        calcToScalar(FloatingLegDayCounterCpp, FloatingLegDayCounter);

        std::string PaymentConventionCpp;
        if(PaymentConvention.hasValue()) 
            calcToScalar(PaymentConventionCpp, PaymentConvention);
        else
            PaymentConventionCpp = "Following";

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite, valueObject);

        // Convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwap: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFairRate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->fairRate();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFairRate: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFairSpread(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->fairSpread();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFairSpread: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFixedLegNPV(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->fixedLegNPV();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFixedLegNPV: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFixedRate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->fixedRate();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFixedRate: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFloatingLegBPS(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->floatingLegBPS();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFloatingLegBPS: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapFloatingLegNPV(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->floatingLegNPV();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapFloatingLegNPV: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapNominal(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->nominal();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapNominal: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaSwapSpread(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::VanillaSwap, QuantLib::VanillaSwap)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->spread();

        // convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlVanillaSwapSpread: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}


