
/*  
 Copyright (C) 2006, 2007, 2008, 2014 Ferdinando Ametrano
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.includes

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

#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlMakeSwaption(
        const ANY &ObjectId,
        const ANY &SwapIndex,
        const ANY &OptionTenor,
        const ANY &Strike,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string SwapIndexCpp;
        calcToScalar(SwapIndexCpp, SwapIndex);

        std::string OptionTenorCpp;
        calcToScalar(OptionTenorCpp, OptionTenor);

        double StrikeCpp;
        if(Strike.hasValue()) 
            calcToScalar(StrikeCpp, Strike);
        else
            StrikeCpp = QuantLib::Null<QuantLib::Rate>();

        std::string PricingEngineIDCpp;
        calcToScalar(PricingEngineIDCpp, PricingEngineID);

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
            errorMsg << "ERROR: qlMakeSwaption: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwaption(
        const ANY &ObjectId,
        const ANY &VanillaSwap,
        const ANY &Exercise,
        const ANY &SettlementType,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string VanillaSwapCpp;
        calcToScalar(VanillaSwapCpp, VanillaSwap);

        std::string ExerciseCpp;
        calcToScalar(ExerciseCpp, Exercise);

        std::string SettlementTypeCpp;
        calcToScalar(SettlementTypeCpp, SettlementType);

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
            errorMsg << "ERROR: qlSwaption: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwaptionImpliedVolatility(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &YieldCurve,
        const ANY &Guess,
        const ANY &Accuracy,
        const ANY &MaxIter,
        const ANY &MinVol,
        const ANY &MaxVol,
        const ANY &Displacement,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        double PriceCpp;
        calcToScalar(PriceCpp, Price);

        std::string YieldCurveCpp;
        calcToScalar(YieldCurveCpp, YieldCurve);

        double GuessCpp;
        if(Guess.hasValue()) 
            calcToScalar(GuessCpp, Guess);
        else
            GuessCpp = 0.10;

        double AccuracyCpp;
        if(Accuracy.hasValue()) 
            calcToScalar(AccuracyCpp, Accuracy);
        else
            AccuracyCpp = 1.0e-6;

        long MaxIterCpp;
        calcToScalar(MaxIterCpp, MaxIter);

        double MinVolCpp;
        if(MinVol.hasValue()) 
            calcToScalar(MinVolCpp, MinVol);
        else
            MinVolCpp = 1.0e-7;

        double MaxVolCpp;
        if(MaxVol.hasValue()) 
            calcToScalar(MaxVolCpp, MaxVol);
        else
            MaxVolCpp = 4.0;

        double DisplacementCpp;
        if(Displacement.hasValue()) 
            calcToScalar(DisplacementCpp, Displacement);
        else
            DisplacementCpp = 0.0;

        // convert input datatypes to QuantLib datatypes

        QuantLib::Real PriceLib;
        calcToScalar(PriceLib, Price);

        QuantLib::Real AccuracyLib;
        calcToScalar(AccuracyLib, Accuracy);

        QuantLib::Natural MaxIterLib;
        calcToScalar(MaxIterLib, MaxIter);

        QuantLib::Real DisplacementLib;
        calcToScalar(DisplacementLib, Displacement);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Swaption, QuantLib::Swaption)

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->impliedVolatility(
                PriceLib,
                YieldCurveLibObj,
                GuessCpp,
                AccuracyLib,
                MaxIterLib,
                MinVolCpp,
                MaxVolCpp,
                DisplacementLib);

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
            errorMsg << "ERROR: qlSwaptionImpliedVolatility: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwaptionSettlementType(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Swaption, QuantLib::Swaption)

        // invoke the member function

        QuantLib::Settlement::Type returnValue = ObjectIdLibObjPtr->settlementType();

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
            errorMsg << "ERROR: qlSwaptionSettlementType: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwaptionType(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Swaption, QuantLib::Swaption)

        // invoke the member function

        QuantLib::VanillaSwap::Type returnValue = ObjectIdLibObjPtr->type();

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
            errorMsg << "ERROR: qlSwaptionType: " << e.what(); 
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


