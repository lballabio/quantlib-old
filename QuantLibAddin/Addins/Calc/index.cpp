
/*  
 Copyright (C) 2006, 2007, 2008, 2009, 2010 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 
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
#include <qlo/enumerations/factories/calendarfactory.hpp>
#include <qlo/indexes/bmaindex.hpp>
#include <qlo/indexes/ibor/euribor.hpp>
#include <qlo/indexes/ibor/libor.hpp>
#include <qlo/indexes/swap/euriborswap.hpp>
#include <qlo/indexes/swap/liborswap.hpp>
#include <qlo/indexes/swap/isdafixaswap.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/handleimpl.hpp>
#include <qlo/timeseries.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/experimental/coupons/proxyibor.hpp>
#include <ql/indexes/swapindex.hpp>
#include <qlo/valueobjects/vo_index.hpp>
#include <qlo/loop/loop_index.hpp>
#include <loop.hpp>
#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlBMAIndex(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlBMAIndex(
                ObjectIdCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::BMAIndex(
                valueObject,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlBMAIndex: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlEonia(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlEonia(
                ObjectIdCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Eonia(
                valueObject,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlEonia: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlEuribor(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlEuribor(
                ObjectIdCpp,
                TenorCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Euribor(
                valueObject,
                TenorCpp,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlEuribor: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlEuribor365(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlEuribor365(
                ObjectIdCpp,
                TenorCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Euribor365(
                valueObject,
                TenorCpp,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlEuribor365: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlEuriborSwap(
        const ANY &ObjectId,
        const ANY &FixingType,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string FixingTypeCpp;
        if(FixingType.hasValue()) 
            calcToScalar(FixingTypeCpp, FixingType);
        else
            FixingTypeCpp = "Default";

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string FwdCurveCpp;
        if(FwdCurve.hasValue()) 
            calcToScalar(FwdCurveCpp, FwdCurve);
        else
            FwdCurveCpp = "";

        std::string DiscCurveCpp;
        if(DiscCurve.hasValue()) 
            calcToScalar(DiscCurveCpp, DiscCurve);
        else
            DiscCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FwdCurveCoerce, FwdCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> FwdCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    FwdCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        OH_GET_OBJECT_DEFAULT(DiscCurveCoerce, DiscCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLibAddin::SwapIndex::FixingType FixingTypeEnum =
            ObjectHandler::Create<QuantLibAddin::SwapIndex::FixingType>()(FixingTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlEuriborSwap(
                ObjectIdCpp,
                FixingTypeCpp,
                TenorCpp,
                FwdCurveCpp,
                DiscCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::EuriborSwap(
                valueObject,
                FixingTypeEnum,
                TenorLib,
                FwdCurveLibObj,
                DiscCurveLibObj,
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
            errorMsg << "ERROR: qlEuriborSwap: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlEuriborSwapIsdaFixA(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string FwdCurveCpp;
        if(FwdCurve.hasValue()) 
            calcToScalar(FwdCurveCpp, FwdCurve);
        else
            FwdCurveCpp = "";

        std::string DiscCurveCpp;
        if(DiscCurve.hasValue()) 
            calcToScalar(DiscCurveCpp, DiscCurve);
        else
            DiscCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FwdCurveCoerce, FwdCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> FwdCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    FwdCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        OH_GET_OBJECT_DEFAULT(DiscCurveCoerce, DiscCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlEuriborSwapIsdaFixA(
                ObjectIdCpp,
                TenorCpp,
                FwdCurveCpp,
                DiscCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::EuriborSwapIsdaFixA(
                valueObject,
                TenorLib,
                FwdCurveLibObj,
                DiscCurveLibObj,
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
            errorMsg << "ERROR: qlEuriborSwapIsdaFixA: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlIborIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &BDayConvention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const ANY &FwdCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string FamilyNameCpp;
        calcToScalar(FamilyNameCpp, FamilyName);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        long FixingDaysCpp;
        calcToScalar(FixingDaysCpp, FixingDays);

        std::string CurrencyCpp;
        calcToScalar(CurrencyCpp, Currency);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string BDayConventionCpp;
        calcToScalar(BDayConventionCpp, BDayConvention);

        bool EndOfMonthCpp;
        calcToScalar(EndOfMonthCpp, EndOfMonth);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        std::string FwdCurveCpp;
        if(FwdCurve.hasValue()) 
            calcToScalar(FwdCurveCpp, FwdCurve);
        else
            FwdCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Natural FixingDaysLib;
        calcToScalar(FixingDaysLib, FixingDays);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FwdCurveCoerce, FwdCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> FwdCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    FwdCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Currency CurrencyEnum =
            ObjectHandler::Create<QuantLib::Currency>()(CurrencyCpp);

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention BDayConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(BDayConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlIborIndex(
                ObjectIdCpp,
                FamilyNameCpp,
                TenorCpp,
                FixingDaysCpp,
                CurrencyCpp,
                CalendarCpp,
                BDayConventionCpp,
                EndOfMonthCpp,
                DayCounterCpp,
                FwdCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::IborIndex(
                valueObject,
                FamilyNameCpp,
                TenorLib,
                FixingDaysLib,
                CurrencyEnum,
                CalendarEnum,
                BDayConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
                FwdCurveLibObj,
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
            errorMsg << "ERROR: qlIborIndex: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlIndexAddFixings(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &FixingDates,
        const SEQSEQ(ANY) &FixingValues,
        const sal_Int32 ForceOverwrite,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::vector<ObjectHandler::property_t> FixingDatesCpp;
        calcToVector(FixingDatesCpp, FixingDates);

        std::vector<double> FixingValuesCpp;
        calcToVector(FixingValuesCpp, FixingValues);

        bool ForceOverwriteCpp;
        calcToScalar(ForceOverwriteCpp, ForceOverwrite);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> FixingDatesLib;
        calcToVector(FixingDatesLib, FixingDates);

        std::vector<QuantLib::Real> FixingValuesLib;
        calcToVector(FixingValuesLib, FixingValues);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdObjPtr, ObjectIdCpp, QuantLibAddin::Index)

        // invoke the member function

        static bool returnValue = true;
        ObjectIdObjPtr->addFixings(
                FixingDatesLib,
                FixingValuesLib,
                ForceOverwriteCpp);

        // convert and return the return value



        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        STRING s = STRFROMASCII( std::string("VOID").c_str() );    
        retAnyVector[0] = CSS::uno::makeAny( s );
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlIndexAddFixings: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlIndexClearFixings(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Index, QuantLib::Index)

        // invoke the member function

        static bool returnValue = true;
        ObjectIdLibObjPtr->clearFixings();

        // convert and return the return value



        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        STRING s = STRFROMASCII( std::string("VOID").c_str() );    
        retAnyVector[0] = CSS::uno::makeAny( s );
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlIndexClearFixings: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlIndexName(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Index, QuantLib::Index)

        // invoke the member function

        std::string returnValue = ObjectIdLibObjPtr->name();

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
            errorMsg << "ERROR: qlIndexName: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlLibor(
        const ANY &ObjectId,
        const ANY &Currency,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string CurrencyCpp;
        calcToScalar(CurrencyCpp, Currency);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Currency CurrencyEnum =
            ObjectHandler::Create<QuantLib::Currency>()(CurrencyCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlLibor(
                ObjectIdCpp,
                CurrencyCpp,
                TenorCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Libor(
                valueObject,
                CurrencyEnum,
                TenorCpp,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlLibor: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlLiborSwap(
        const ANY &ObjectId,
        const ANY &Currency,
        const ANY &FixingType,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string CurrencyCpp;
        calcToScalar(CurrencyCpp, Currency);

        std::string FixingTypeCpp;
        if(FixingType.hasValue()) 
            calcToScalar(FixingTypeCpp, FixingType);
        else
            FixingTypeCpp = "Default";

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string FwdCurveCpp;
        if(FwdCurve.hasValue()) 
            calcToScalar(FwdCurveCpp, FwdCurve);
        else
            FwdCurveCpp = "";

        std::string DiscCurveCpp;
        if(DiscCurve.hasValue()) 
            calcToScalar(DiscCurveCpp, DiscCurve);
        else
            DiscCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FwdCurveCoerce, FwdCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> FwdCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    FwdCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        OH_GET_OBJECT_DEFAULT(DiscCurveCoerce, DiscCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Currency CurrencyEnum =
            ObjectHandler::Create<QuantLib::Currency>()(CurrencyCpp);

        QuantLibAddin::SwapIndex::FixingType FixingTypeEnum =
            ObjectHandler::Create<QuantLibAddin::SwapIndex::FixingType>()(FixingTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlLiborSwap(
                ObjectIdCpp,
                CurrencyCpp,
                FixingTypeCpp,
                TenorCpp,
                FwdCurveCpp,
                DiscCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::LiborSwap(
                valueObject,
                CurrencyEnum,
                FixingTypeEnum,
                TenorLib,
                FwdCurveLibObj,
                DiscCurveLibObj,
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
            errorMsg << "ERROR: qlLiborSwap: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlOvernightIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &DayCounter,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string FamilyNameCpp;
        calcToScalar(FamilyNameCpp, FamilyName);

        long FixingDaysCpp;
        calcToScalar(FixingDaysCpp, FixingDays);

        std::string CurrencyCpp;
        calcToScalar(CurrencyCpp, Currency);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Natural FixingDaysLib;
        calcToScalar(FixingDaysLib, FixingDays);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Currency CurrencyEnum =
            ObjectHandler::Create<QuantLib::Currency>()(CurrencyCpp);

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlOvernightIndex(
                ObjectIdCpp,
                FamilyNameCpp,
                FixingDaysCpp,
                CurrencyCpp,
                CalendarCpp,
                DayCounterCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::OvernightIndex(
                valueObject,
                FamilyNameCpp,
                FixingDaysLib,
                CurrencyEnum,
                CalendarEnum,
                DayCounterEnum,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlOvernightIndex: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSonia(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string YieldCurveCpp;
        if(YieldCurve.hasValue()) 
            calcToScalar(YieldCurveCpp, YieldCurve);
        else
            YieldCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSonia(
                ObjectIdCpp,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Sonia(
                valueObject,
                YieldCurveLibObj,
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
            errorMsg << "ERROR: qlSonia: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwapIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &FixedLegTenor,
        const ANY &FixedLegBDC,
        const ANY &FixedLegDayCounter,
        const ANY &IborIndex,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string FamilyNameCpp;
        calcToScalar(FamilyNameCpp, FamilyName);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        long FixingDaysCpp;
        calcToScalar(FixingDaysCpp, FixingDays);

        std::string CurrencyCpp;
        calcToScalar(CurrencyCpp, Currency);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string FixedLegTenorCpp;
        calcToScalar(FixedLegTenorCpp, FixedLegTenor);

        std::string FixedLegBDCCpp;
        calcToScalar(FixedLegBDCCpp, FixedLegBDC);

        std::string FixedLegDayCounterCpp;
        calcToScalar(FixedLegDayCounterCpp, FixedLegDayCounter);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        std::string DiscCurveCpp;
        if(DiscCurve.hasValue()) 
            calcToScalar(DiscCurveCpp, DiscCurve);
        else
            DiscCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Natural FixingDaysLib;
        calcToScalar(FixingDaysLib, FixingDays);

        QuantLib::Period FixedLegTenorLib;
        calcToScalar(FixedLegTenorLib, FixedLegTenor);

        // convert object IDs into library objects

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_OBJECT_DEFAULT(DiscCurveCoerce, DiscCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Currency CurrencyEnum =
            ObjectHandler::Create<QuantLib::Currency>()(CurrencyCpp);

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention FixedLegBDCEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(FixedLegBDCCpp);

        QuantLib::DayCounter FixedLegDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixedLegDayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSwapIndex(
                ObjectIdCpp,
                FamilyNameCpp,
                TenorCpp,
                FixingDaysCpp,
                CurrencyCpp,
                CalendarCpp,
                FixedLegTenorCpp,
                FixedLegBDCCpp,
                FixedLegDayCounterCpp,
                IborIndexCpp,
                DiscCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SwapIndex(
                valueObject,
                FamilyNameCpp,
                TenorLib,
                FixingDaysLib,
                CurrencyEnum,
                CalendarEnum,
                FixedLegTenorLib,
                FixedLegBDCEnum,
                FixedLegDayCounterEnum,
                IborIndexLibObjPtr,
                DiscCurveLibObj,
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
            errorMsg << "ERROR: qlSwapIndex: " << e.what(); 
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


