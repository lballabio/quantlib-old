
/*  
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007, 2008, 2009, 2015 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2015 Maddalena Zanzi
 
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
#include <qlo/ratehelpers.hpp>
#include <qlo/indexes/swapindex.hpp>
#include <qlo/schedule.hpp>
#include <qlo/bonds.hpp>
#include <qlo/yieldtermstructures.hpp>
#include <qlo/indexes/ibor/euribor.hpp>
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/instruments/bonds/fixedratebond.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <qlo/valueobjects/vo_ratehelpers.hpp>

#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlBondHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &Bond,
        const sal_Int32 UseCleanPrice,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PriceCpp;
        calcToScalar(PriceCpp, Price);

        std::string BondCpp;
        calcToScalar(BondCpp, Bond);

        bool UseCleanPriceCpp;
        calcToScalar(UseCleanPriceCpp, UseCleanPrice);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(PriceCoerce, PriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> PriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    PriceCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(BondLibObjPtr, BondCpp,
            QuantLibAddin::Bond, QuantLib::Bond)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlBondHelper(
                ObjectIdCpp,
                PriceCpp,
                BondCpp,
                UseCleanPriceCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::BondHelper(
                valueObject,
                PriceLibObj,
                BondLibObjPtr,
                UseCleanPriceCpp,
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
            errorMsg << "ERROR: qlBondHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDatedOISRateHelper(
        const ANY &ObjectId,
        const ANY &StartDate,
        const ANY &EndDate,
        const ANY &FixedRate,
        const ANY &ONIndex,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        ObjectHandler::property_t StartDateCpp;
        calcToScalar(StartDateCpp, StartDate);

        ObjectHandler::property_t EndDateCpp;
        calcToScalar(EndDateCpp, EndDate);

        std::string FixedRateCpp;
        calcToScalar(FixedRateCpp, FixedRate);

        std::string ONIndexCpp;
        calcToScalar(ONIndexCpp, ONIndex);

        std::string DiscountingCurveCpp;
        if(DiscountingCurve.hasValue()) 
            calcToScalar(DiscountingCurveCpp, DiscountingCurve);
        else
            DiscountingCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date StartDateLib;
        calcToScalar(StartDateLib, StartDate);

        QuantLib::Date EndDateLib;
        calcToScalar(EndDateLib, EndDate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FixedRateCoerce, FixedRateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> FixedRateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    FixedRateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(ONIndexLibObjPtr, ONIndexCpp,
            QuantLibAddin::OvernightIndex, QuantLib::OvernightIndex)

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlDatedOISRateHelper(
                ObjectIdCpp,
                StartDateCpp,
                EndDateCpp,
                FixedRateCpp,
                ONIndexCpp,
                DiscountingCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DatedOISRateHelper(
                valueObject,
                StartDateLib,
                EndDateLib,
                FixedRateLibObj,
                ONIndexLibObjPtr,
                DiscountingCurveLibObj,
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
            errorMsg << "ERROR: qlDatedOISRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDepositRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &IborIndex,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlDepositRateHelper(
                ObjectIdCpp,
                RateCpp,
                IborIndexCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DepositRateHelper(
                valueObject,
                RateLibObj,
                IborIndexLibObjPtr,
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
            errorMsg << "ERROR: qlDepositRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDepositRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        long FixingDaysCpp;
        calcToScalar(FixingDaysCpp, FixingDays);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string ConventionCpp;
        calcToScalar(ConventionCpp, Convention);

        bool EndOfMonthCpp;
        calcToScalar(EndOfMonthCpp, EndOfMonth);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Natural FixingDaysLib;
        calcToScalar(FixingDaysLib, FixingDays);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention ConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(ConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlDepositRateHelper2(
                ObjectIdCpp,
                RateCpp,
                TenorCpp,
                FixingDaysCpp,
                CalendarCpp,
                ConventionCpp,
                EndOfMonthCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DepositRateHelper(
                valueObject,
                RateLibObj,
                TenorLib,
                FixingDaysLib,
                CalendarEnum,
                ConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
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
            errorMsg << "ERROR: qlDepositRateHelper2: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFixedRateBondHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &SettlementDays,
        const ANY &FaceAmount,
        const ANY &ScheduleID,
        const SEQSEQ(ANY) &Coupons,
        const ANY &DayCounter,
        const ANY &PaymentBDC,
        const ANY &Redemption,
        const ANY &IssueDate,
        const ANY &PaymentCalendar,
        const ANY &ExCouponPeriod,
        const ANY &ExCouponCalendar,
        const ANY &ExCouponBDC,
        const sal_Int32 ExCouponEndOfMonth,
        const sal_Int32 UseCleanPrice,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PriceCpp;
        calcToScalar(PriceCpp, Price);

        long SettlementDaysCpp;
        calcToScalar(SettlementDaysCpp, SettlementDays);

        double FaceAmountCpp;
        if(FaceAmount.hasValue()) 
            calcToScalar(FaceAmountCpp, FaceAmount);
        else
            FaceAmountCpp = 100.0;

        std::string ScheduleIDCpp;
        calcToScalar(ScheduleIDCpp, ScheduleID);

        std::vector<double> CouponsCpp;
        calcToVector(CouponsCpp, Coupons);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        std::string PaymentBDCCpp;
        if(PaymentBDC.hasValue()) 
            calcToScalar(PaymentBDCCpp, PaymentBDC);
        else
            PaymentBDCCpp = "Following";

        double RedemptionCpp;
        if(Redemption.hasValue()) 
            calcToScalar(RedemptionCpp, Redemption);
        else
            RedemptionCpp = 100.0;

        ObjectHandler::property_t IssueDateCpp;
        calcToScalar(IssueDateCpp, IssueDate);

        std::string PaymentCalendarCpp;
        calcToScalar(PaymentCalendarCpp, PaymentCalendar);

        std::string ExCouponPeriodCpp;
        calcToScalar(ExCouponPeriodCpp, ExCouponPeriod);

        std::string ExCouponCalendarCpp;
        calcToScalar(ExCouponCalendarCpp, ExCouponCalendar);

        std::string ExCouponBDCCpp;
        calcToScalar(ExCouponBDCCpp, ExCouponBDC);

        bool ExCouponEndOfMonthCpp;
        calcToScalar(ExCouponEndOfMonthCpp, ExCouponEndOfMonth);

        bool UseCleanPriceCpp;
        calcToScalar(UseCleanPriceCpp, UseCleanPrice);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Size SettlementDaysLib;
        calcToScalar(SettlementDaysLib, SettlementDays);

        QuantLib::Date IssueDateLib;
        if(!IssueDate.hasValue() and typeid(QuantLib::Date())==typeid(QuantLib::Date())) 
            IssueDateLib = QuantLib::Date();
        else
            calcToScalar(IssueDateLib, IssueDate);

        QuantLib::Period ExCouponPeriodLib;
        calcToScalar(ExCouponPeriodLib, ExCouponPeriod);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(PriceCoerce, PriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> PriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    PriceCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(ScheduleIDLibObjPtr, ScheduleIDCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        QuantLib::BusinessDayConvention PaymentBDCEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentBDCCpp);

        QuantLib::Calendar PaymentCalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(PaymentCalendarCpp);

        QuantLib::Calendar ExCouponCalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(ExCouponCalendarCpp);

        QuantLib::BusinessDayConvention ExCouponBDCEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(ExCouponBDCCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFixedRateBondHelper(
                ObjectIdCpp,
                PriceCpp,
                SettlementDaysCpp,
                FaceAmountCpp,
                ScheduleIDCpp,
                CouponsCpp,
                DayCounterCpp,
                PaymentBDCCpp,
                RedemptionCpp,
                IssueDateCpp,
                PaymentCalendarCpp,
                ExCouponPeriodCpp,
                ExCouponCalendarCpp,
                ExCouponBDCCpp,
                ExCouponEndOfMonthCpp,
                UseCleanPriceCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FixedRateBondHelper(
                valueObject,
                PriceLibObj,
                SettlementDaysLib,
                FaceAmountCpp,
                ScheduleIDLibObjPtr,
                CouponsCpp,
                DayCounterEnum,
                PaymentBDCEnum,
                RedemptionCpp,
                IssueDateLib,
                PaymentCalendarEnum,
                ExCouponPeriodLib,
                ExCouponCalendarEnum,
                ExCouponBDCEnum,
                ExCouponEndOfMonthCpp,
                UseCleanPriceCpp,
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
            errorMsg << "ERROR: qlFixedRateBondHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFraRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &PeriodToStart,
        const ANY &IborIndex,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string PeriodToStartCpp;
        calcToScalar(PeriodToStartCpp, PeriodToStart);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period PeriodToStartLib;
        calcToScalar(PeriodToStartLib, PeriodToStart);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFraRateHelper(
                ObjectIdCpp,
                RateCpp,
                PeriodToStartCpp,
                IborIndexCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FraRateHelper(
                valueObject,
                RateLibObj,
                PeriodToStartLib,
                IborIndexLibObjPtr,
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
            errorMsg << "ERROR: qlFraRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFraRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &PeriodToStart,
        const ANY &LengthInMonths,
        const ANY &FixingDays,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string PeriodToStartCpp;
        calcToScalar(PeriodToStartCpp, PeriodToStart);

        long LengthInMonthsCpp;
        calcToScalar(LengthInMonthsCpp, LengthInMonths);

        long FixingDaysCpp;
        calcToScalar(FixingDaysCpp, FixingDays);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string ConventionCpp;
        calcToScalar(ConventionCpp, Convention);

        bool EndOfMonthCpp;
        calcToScalar(EndOfMonthCpp, EndOfMonth);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period PeriodToStartLib;
        calcToScalar(PeriodToStartLib, PeriodToStart);

        QuantLib::Natural LengthInMonthsLib;
        calcToScalar(LengthInMonthsLib, LengthInMonths);

        QuantLib::Natural FixingDaysLib;
        calcToScalar(FixingDaysLib, FixingDays);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention ConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(ConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFraRateHelper2(
                ObjectIdCpp,
                RateCpp,
                PeriodToStartCpp,
                LengthInMonthsCpp,
                FixingDaysCpp,
                CalendarCpp,
                ConventionCpp,
                EndOfMonthCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FraRateHelper(
                valueObject,
                RateLibObj,
                PeriodToStartLib,
                LengthInMonthsLib,
                FixingDaysLib,
                CalendarEnum,
                ConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
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
            errorMsg << "ERROR: qlFraRateHelper2: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFuturesRateHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &IborIndex,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PriceCpp;
        calcToScalar(PriceCpp, Price);

        std::string FuturesTypeCpp;
        if(FuturesType.hasValue()) 
            calcToScalar(FuturesTypeCpp, FuturesType);
        else
            FuturesTypeCpp = "IMM";

        ObjectHandler::property_t FuturesDateCpp;
        calcToScalar(FuturesDateCpp, FuturesDate);

        std::string IborIndexCpp;
        if(IborIndex.hasValue()) 
            calcToScalar(IborIndexCpp, IborIndex);
        else
            IborIndexCpp = "Euribor3M";

        std::string ConvexityAdjQuoteCpp;
        calcToScalar(ConvexityAdjQuoteCpp, ConvexityAdjQuote);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date FuturesDateLib;
        calcToScalar(FuturesDateLib, FuturesDate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(PriceCoerce, PriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> PriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    PriceCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_OBJECT_DEFAULT(ConvexityAdjQuoteCoerce, ConvexityAdjQuoteCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> ConvexityAdjQuoteLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    ConvexityAdjQuoteCoerce, QuantLib::Handle<QuantLib::Quote>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Futures::Type FuturesTypeEnum =
            ObjectHandler::Create<QuantLib::Futures::Type>()(FuturesTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFuturesRateHelper(
                ObjectIdCpp,
                PriceCpp,
                FuturesTypeCpp,
                FuturesDateCpp,
                IborIndexCpp,
                ConvexityAdjQuoteCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FuturesRateHelper(
                valueObject,
                PriceLibObj,
                FuturesTypeEnum,
                FuturesDateLib,
                IborIndexLibObjPtr,
                ConvexityAdjQuoteLibObj,
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
            errorMsg << "ERROR: qlFuturesRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFuturesRateHelper2(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &LengthInMonths,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PriceCpp;
        calcToScalar(PriceCpp, Price);

        std::string FuturesTypeCpp;
        if(FuturesType.hasValue()) 
            calcToScalar(FuturesTypeCpp, FuturesType);
        else
            FuturesTypeCpp = "IMM";

        ObjectHandler::property_t FuturesDateCpp;
        calcToScalar(FuturesDateCpp, FuturesDate);

        long LengthInMonthsCpp;
        calcToScalar(LengthInMonthsCpp, LengthInMonths);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string ConventionCpp;
        if(Convention.hasValue()) 
            calcToScalar(ConventionCpp, Convention);
        else
            ConventionCpp = "Modified Following";

        bool EndOfMonthCpp;
        calcToScalar(EndOfMonthCpp, EndOfMonth);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/360";

        std::string ConvexityAdjQuoteCpp;
        calcToScalar(ConvexityAdjQuoteCpp, ConvexityAdjQuote);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date FuturesDateLib;
        calcToScalar(FuturesDateLib, FuturesDate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(PriceCoerce, PriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> PriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    PriceCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(ConvexityAdjQuoteCoerce, ConvexityAdjQuoteCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> ConvexityAdjQuoteLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    ConvexityAdjQuoteCoerce, QuantLib::Handle<QuantLib::Quote>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Futures::Type FuturesTypeEnum =
            ObjectHandler::Create<QuantLib::Futures::Type>()(FuturesTypeCpp);

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention ConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(ConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFuturesRateHelper2(
                ObjectIdCpp,
                PriceCpp,
                FuturesTypeCpp,
                FuturesDateCpp,
                LengthInMonthsCpp,
                CalendarCpp,
                ConventionCpp,
                EndOfMonthCpp,
                DayCounterCpp,
                ConvexityAdjQuoteCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FuturesRateHelper(
                valueObject,
                PriceLibObj,
                FuturesTypeEnum,
                FuturesDateLib,
                LengthInMonthsCpp,
                CalendarEnum,
                ConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
                ConvexityAdjQuoteLibObj,
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
            errorMsg << "ERROR: qlFuturesRateHelper2: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFuturesRateHelper3(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &EndDate,
        const ANY &DayCounter,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PriceCpp;
        calcToScalar(PriceCpp, Price);

        std::string FuturesTypeCpp;
        if(FuturesType.hasValue()) 
            calcToScalar(FuturesTypeCpp, FuturesType);
        else
            FuturesTypeCpp = "IMM";

        ObjectHandler::property_t FuturesDateCpp;
        calcToScalar(FuturesDateCpp, FuturesDate);

        ObjectHandler::property_t EndDateCpp;
        calcToScalar(EndDateCpp, EndDate);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/360";

        std::string ConvexityAdjQuoteCpp;
        calcToScalar(ConvexityAdjQuoteCpp, ConvexityAdjQuote);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date FuturesDateLib;
        calcToScalar(FuturesDateLib, FuturesDate);

        QuantLib::Date EndDateLib;
        if(!EndDate.hasValue() and typeid(QuantLib::Date())==typeid(QuantLib::Date())) 
            EndDateLib = QuantLib::Date();
        else
            calcToScalar(EndDateLib, EndDate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(PriceCoerce, PriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> PriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    PriceCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(ConvexityAdjQuoteCoerce, ConvexityAdjQuoteCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> ConvexityAdjQuoteLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    ConvexityAdjQuoteCoerce, QuantLib::Handle<QuantLib::Quote>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Futures::Type FuturesTypeEnum =
            ObjectHandler::Create<QuantLib::Futures::Type>()(FuturesTypeCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFuturesRateHelper3(
                ObjectIdCpp,
                PriceCpp,
                FuturesTypeCpp,
                FuturesDateCpp,
                EndDateCpp,
                DayCounterCpp,
                ConvexityAdjQuoteCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FuturesRateHelper(
                valueObject,
                PriceLibObj,
                FuturesTypeEnum,
                FuturesDateLib,
                EndDateLib,
                DayCounterEnum,
                ConvexityAdjQuoteLibObj,
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
            errorMsg << "ERROR: qlFuturesRateHelper3: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlOISRateHelper(
        const ANY &ObjectId,
        const ANY &SettlDays,
        const ANY &Tenor,
        const ANY &FixedRate,
        const ANY &ONIndex,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        long SettlDaysCpp;
        calcToScalar(SettlDaysCpp, SettlDays);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string FixedRateCpp;
        calcToScalar(FixedRateCpp, FixedRate);

        std::string ONIndexCpp;
        calcToScalar(ONIndexCpp, ONIndex);

        std::string DiscountingCurveCpp;
        if(DiscountingCurve.hasValue()) 
            calcToScalar(DiscountingCurveCpp, DiscountingCurve);
        else
            DiscountingCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Natural SettlDaysLib;
        calcToScalar(SettlDaysLib, SettlDays);

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(FixedRateCoerce, FixedRateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> FixedRateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    FixedRateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(ONIndexLibObjPtr, ONIndexCpp,
            QuantLibAddin::OvernightIndex, QuantLib::OvernightIndex)

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlOISRateHelper(
                ObjectIdCpp,
                SettlDaysCpp,
                TenorCpp,
                FixedRateCpp,
                ONIndexCpp,
                DiscountingCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::OISRateHelper(
                valueObject,
                SettlDaysLib,
                TenorLib,
                FixedRateLibObj,
                ONIndexLibObjPtr,
                DiscountingCurveLibObj,
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
            errorMsg << "ERROR: qlOISRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperEarliestDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::RateHelper, QuantLib::RateHelper)

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->earliestDate();

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
            errorMsg << "ERROR: qlRateHelperEarliestDate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperImpliedQuote(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::RateHelper, QuantLib::RateHelper)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->impliedQuote();

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
            errorMsg << "ERROR: qlRateHelperImpliedQuote: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperLatestDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::RateHelper, QuantLib::RateHelper)

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->latestDate();

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
            errorMsg << "ERROR: qlRateHelperLatestDate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperQuoteIsValid(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::RateHelper, QuantLib::RateHelper)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->quote()->isValid();

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
            errorMsg << "ERROR: qlRateHelperQuoteIsValid: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperQuoteName(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdObjPtr, ObjectIdCpp, QuantLibAddin::RateHelper)

        // invoke the member function

        std::string returnValue = ObjectIdObjPtr->quoteName();

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
            errorMsg << "ERROR: qlRateHelperQuoteName: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRateHelperQuoteValue(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::RateHelper, QuantLib::RateHelper)

        // invoke the member function

        static double returnValue;
        returnValue = ObjectIdLibObjPtr->quote()->value();

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
            errorMsg << "ERROR: qlRateHelperQuoteValue: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwapRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &SwapIndex,
        const ANY &Spread,
        const ANY &ForwardStart,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string SwapIndexCpp;
        calcToScalar(SwapIndexCpp, SwapIndex);

        std::string SpreadCpp;
        calcToScalar(SpreadCpp, Spread);

        std::string ForwardStartCpp;
        calcToScalar(ForwardStartCpp, ForwardStart);

        std::string DiscountingCurveCpp;
        if(DiscountingCurve.hasValue()) 
            calcToScalar(DiscountingCurveCpp, DiscountingCurve);
        else
            DiscountingCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(SwapIndexLibObjPtr, SwapIndexCpp,
            QuantLibAddin::SwapIndex, QuantLib::SwapIndex)

        OH_GET_OBJECT_DEFAULT(SpreadCoerce, SpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> SpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    SpreadCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSwapRateHelper(
                ObjectIdCpp,
                RateCpp,
                SwapIndexCpp,
                SpreadCpp,
                ForwardStartCpp,
                DiscountingCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SwapRateHelper(
                valueObject,
                RateLibObj,
                SwapIndexLibObjPtr,
                SpreadLibObj,
                ForwardStartLib,
                DiscountingCurveLibObj,
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
            errorMsg << "ERROR: qlSwapRateHelper: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlSwapRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &SettlDays,
        const ANY &Tenor,
        const ANY &Calendar,
        const ANY &FixedLegFrequency,
        const ANY &FixedLegConvention,
        const ANY &FixedLegDayCounter,
        const ANY &IborIndex,
        const ANY &Spread,
        const ANY &ForwardStart,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        long SettlDaysCpp;
        calcToScalar(SettlDaysCpp, SettlDays);

        std::string TenorCpp;
        calcToScalar(TenorCpp, Tenor);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string FixedLegFrequencyCpp;
        calcToScalar(FixedLegFrequencyCpp, FixedLegFrequency);

        std::string FixedLegConventionCpp;
        calcToScalar(FixedLegConventionCpp, FixedLegConvention);

        std::string FixedLegDayCounterCpp;
        calcToScalar(FixedLegDayCounterCpp, FixedLegDayCounter);

        std::string IborIndexCpp;
        calcToScalar(IborIndexCpp, IborIndex);

        std::string SpreadCpp;
        calcToScalar(SpreadCpp, Spread);

        std::string ForwardStartCpp;
        calcToScalar(ForwardStartCpp, ForwardStart);

        std::string DiscountingCurveCpp;
        if(DiscountingCurve.hasValue()) 
            calcToScalar(DiscountingCurveCpp, DiscountingCurve);
        else
            DiscountingCurveCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Natural SettlDaysLib;
        calcToScalar(SettlDaysLib, SettlDays);

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_OBJECT_DEFAULT(SpreadCoerce, SpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> SpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    SpreadCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::Frequency FixedLegFrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FixedLegFrequencyCpp);

        QuantLib::BusinessDayConvention FixedLegConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(FixedLegConventionCpp);

        QuantLib::DayCounter FixedLegDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(FixedLegDayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSwapRateHelper2(
                ObjectIdCpp,
                RateCpp,
                SettlDaysCpp,
                TenorCpp,
                CalendarCpp,
                FixedLegFrequencyCpp,
                FixedLegConventionCpp,
                FixedLegDayCounterCpp,
                IborIndexCpp,
                SpreadCpp,
                ForwardStartCpp,
                DiscountingCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SwapRateHelper(
                valueObject,
                RateLibObj,
                SettlDaysLib,
                TenorLib,
                CalendarEnum,
                FixedLegFrequencyEnum,
                FixedLegConventionEnum,
                FixedLegDayCounterEnum,
                IborIndexLibObjPtr,
                SpreadLibObj,
                ForwardStartLib,
                DiscountingCurveLibObj,
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
            errorMsg << "ERROR: qlSwapRateHelper2: " << e.what(); 
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


