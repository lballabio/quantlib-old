
/*  
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007, 2008, 2009 Ferdinando Ametrano
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

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlBondHelper(
        const STRING &ObjectId,
        const STRING &CleanPrice,
        const STRING &Bond,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string CleanPriceCpp = ouStringToStlString(CleanPrice);

        std::string BondCpp = ouStringToStlString(Bond);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT(CleanPriceCoerce, CleanPriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> CleanPriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    CleanPriceCoerce);

        OH_GET_REFERENCE(BondLibObjPtr, BondCpp,
            QuantLibAddin::Bond, QuantLib::Bond)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlBondHelper(
                ObjectIdCpp,
                CleanPriceCpp,
                BondCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::BondHelper(
                valueObject,
                CleanPriceLibObj,
                BondLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlBondHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlDatedOISRateHelper(
        const STRING &ObjectId,
        const ANY &StartDate,
        const ANY &EndDate,
        const STRING &FixedRate,
        const STRING &ONIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        ObjectHandler::property_t StartDateCpp;
        calcToScalar(StartDateCpp, StartDate);

        ObjectHandler::property_t EndDateCpp;
        calcToScalar(EndDateCpp, EndDate);

        std::string FixedRateCpp = ouStringToStlString(FixedRate);

        std::string ONIndexCpp = ouStringToStlString(ONIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date StartDateLib;
        calcToScalar(StartDateLib, StartDate);

        QuantLib::Date EndDateLib;
        calcToScalar(EndDateLib, EndDate);

        // convert object IDs into library objects

        OH_GET_OBJECT(FixedRateCoerce, FixedRateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> FixedRateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    FixedRateCoerce);

        OH_GET_REFERENCE(ONIndexLibObjPtr, ONIndexCpp,
            QuantLibAddin::OvernightIndex, QuantLib::OvernightIndex)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlDatedOISRateHelper(
                ObjectIdCpp,
                StartDateCpp,
                EndDateCpp,
                FixedRateCpp,
                ONIndexCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DatedOISRateHelper(
                valueObject,
                StartDateLib,
                EndDateLib,
                FixedRateLibObj,
                ONIndexLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlDatedOISRateHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlDepositRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &IborIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlDepositRateHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlDepositRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &Tenor,
        sal_Int32 FixingDays,
        const STRING &Calendar,
        const STRING &Convention,
        sal_Int32 EndOfMonth,
        const STRING &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string TenorCpp = ouStringToStlString(Tenor);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string ConventionCpp = ouStringToStlString(Convention);

        bool EndOfMonthCpp = static_cast<bool>(EndOfMonth);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

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
                FixingDays,
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
                FixingDays,
                CalendarEnum,
                ConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlDepositRateHelper2: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlFixedRateBondHelper(
        const STRING &ObjectId,
        const STRING &CleanPrice,
        sal_Int32 SettlementDays,
        const ANY &FaceAmount,
        const STRING &ScheduleID,
        const SEQSEQ(double) &Coupons,
        const STRING &DayCounter,
        const ANY &PaymentBDC,
        const ANY &Redemption,
        const ANY &IssueDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string CleanPriceCpp = ouStringToStlString(CleanPrice);

        double FaceAmountCpp;
        calcToScalar(FaceAmountCpp, FaceAmount);

        std::string ScheduleIDCpp = ouStringToStlString(ScheduleID);

        std::vector<double> CouponsCpp;
        calcToVector(CouponsCpp, Coupons);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        std::string PaymentBDCCpp;
        calcToScalar(PaymentBDCCpp, PaymentBDC);

        double RedemptionCpp;
        calcToScalar(RedemptionCpp, Redemption);

        long IssueDateCpp;
        calcToScalar(IssueDateCpp, IssueDate);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date IssueDateLib;
        calcToScalar(IssueDateLib, IssueDate);

        // convert object IDs into library objects

        OH_GET_OBJECT(CleanPriceCoerce, CleanPriceCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> CleanPriceLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    CleanPriceCoerce);

        OH_GET_REFERENCE(ScheduleIDLibObjPtr, ScheduleIDCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        QuantLib::BusinessDayConvention PaymentBDCEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentBDCCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFixedRateBondHelper(
                ObjectIdCpp,
                CleanPriceCpp,
                SettlementDays,
                FaceAmountCpp,
                ScheduleIDCpp,
                CouponsCpp,
                DayCounterCpp,
                PaymentBDCCpp,
                RedemptionCpp,
                IssueDateCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FixedRateBondHelper(
                valueObject,
                CleanPriceLibObj,
                SettlementDays,
                FaceAmountCpp,
                ScheduleIDLibObjPtr,
                CouponsCpp,
                DayCounterEnum,
                PaymentBDCEnum,
                RedemptionCpp,
                IssueDateLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlFixedRateBondHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlFraRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &PeriodToStart,
        const STRING &IborIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string PeriodToStartCpp = ouStringToStlString(PeriodToStart);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period PeriodToStartLib;
        calcToScalar(PeriodToStartLib, PeriodToStart);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlFraRateHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlFraRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &PeriodToStart,
        sal_Int32 LengthInMonths,
        sal_Int32 FixingDays,
        const STRING &Calendar,
        const STRING &Convention,
        sal_Int32 EndOfMonth,
        const STRING &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string PeriodToStartCpp = ouStringToStlString(PeriodToStart);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string ConventionCpp = ouStringToStlString(Convention);

        bool EndOfMonthCpp = static_cast<bool>(EndOfMonth);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period PeriodToStartLib;
        calcToScalar(PeriodToStartLib, PeriodToStart);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

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
                LengthInMonths,
                FixingDays,
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
                LengthInMonths,
                FixingDays,
                CalendarEnum,
                ConventionEnum,
                EndOfMonthCpp,
                DayCounterEnum,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlFraRateHelper2: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlOISRateHelper(
        const STRING &ObjectId,
        sal_Int32 SettlDays,
        const STRING &Tenor,
        const STRING &FixedRate,
        const STRING &ONIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string TenorCpp = ouStringToStlString(Tenor);

        std::string FixedRateCpp = ouStringToStlString(FixedRate);

        std::string ONIndexCpp = ouStringToStlString(ONIndex);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        // convert object IDs into library objects

        OH_GET_OBJECT(FixedRateCoerce, FixedRateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> FixedRateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    FixedRateCoerce);

        OH_GET_REFERENCE(ONIndexLibObjPtr, ONIndexCpp,
            QuantLibAddin::OvernightIndex, QuantLib::OvernightIndex)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlOISRateHelper(
                ObjectIdCpp,
                SettlDays,
                TenorCpp,
                FixedRateCpp,
                ONIndexCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::OISRateHelper(
                valueObject,
                SettlDays,
                TenorLib,
                FixedRateLibObj,
                ONIndexLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlOISRateHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSwapRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &SwapIndex,
        const STRING &Spread,
        const STRING &ForwardStart,
        const ANY &DiscountingCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string SwapIndexCpp = ouStringToStlString(SwapIndex);

        std::string SpreadCpp = ouStringToStlString(Spread);

        std::string ForwardStartCpp = ouStringToStlString(ForwardStart);

        std::string DiscountingCurveCpp;
        calcToScalar(DiscountingCurveCpp, DiscountingCurve);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

        OH_GET_REFERENCE(SwapIndexLibObjPtr, SwapIndexCpp,
            QuantLibAddin::SwapIndex, QuantLib::SwapIndex)

        OH_GET_OBJECT(SpreadCoerce, SpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> SpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    SpreadCoerce);

        OH_GET_OBJECT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwapRateHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSwapRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &Tenor,
        const STRING &Calendar,
        const STRING &FixedLegFrequency,
        const STRING &FixedLegConvention,
        const STRING &FixedLegDayCounter,
        const STRING &IborIndex,
        const STRING &Spread,
        const STRING &ForwardStart,
        const ANY &DiscountingCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string TenorCpp = ouStringToStlString(Tenor);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string FixedLegFrequencyCpp = ouStringToStlString(FixedLegFrequency);

        std::string FixedLegConventionCpp = ouStringToStlString(FixedLegConvention);

        std::string FixedLegDayCounterCpp = ouStringToStlString(FixedLegDayCounter);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        std::string SpreadCpp = ouStringToStlString(Spread);

        std::string ForwardStartCpp = ouStringToStlString(ForwardStart);

        std::string DiscountingCurveCpp;
        calcToScalar(DiscountingCurveCpp, DiscountingCurve);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_OBJECT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce);

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_OBJECT(SpreadCoerce, SpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> SpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    SpreadCoerce);

        OH_GET_OBJECT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwapRateHelper2: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlSwapRateHelperSpread(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::SwapRateHelper, QuantLib::SwapRateHelper)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->spread();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwapRateHelperSpread: " << e.what());
        THROW_RTE;
    }
}


