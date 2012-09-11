
/*  
 Copyright (C) 2010 Roland Lichters
 
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
#include <qlo/credit.hpp>
#include <qlo/pricingengines.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/handleimpl.hpp>
#include <qlo/conversions/coercetermstructure.hpp>
#include <qlo/ratehelpers.hpp>
#include <ql/termstructures/credit/defaultprobabilityhelpers.hpp>
#include <ql/pricingengines/credit/midpointcdsengine.hpp>
#include <qlo/valueobjects/vo_credit.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlCreditDefaultSwap(
        const STRING &ObjectId,
        const ANY &BuyerSeller,
        double Notional,
        double Upfront,
        double Spread,
        const STRING &PremiumSchedule,
        const ANY &PaymentConvention,
        const STRING &DayCounter,
        const ANY &SettlesAccrual,
        const ANY &PayAtDefault,
        const ANY &ProtectionStart,
        const ANY &UpfrontDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string BuyerSellerCpp;
        calcToScalar(BuyerSellerCpp, BuyerSeller);

        std::string PremiumScheduleCpp = ouStringToStlString(PremiumSchedule);

        std::string PaymentConventionCpp;
        calcToScalar(PaymentConventionCpp, PaymentConvention);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        bool SettlesAccrualCpp;
        calcToScalar(SettlesAccrualCpp, SettlesAccrual);

        bool PayAtDefaultCpp;
        calcToScalar(PayAtDefaultCpp, PayAtDefault);

        long ProtectionStartCpp;
        calcToScalar(ProtectionStartCpp, ProtectionStart);

        long UpfrontDateCpp;
        calcToScalar(UpfrontDateCpp, UpfrontDate);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Real NotionalLib;
        calcToScalar(NotionalLib, Notional);

        QuantLib::Date ProtectionStartLib;
        calcToScalar(ProtectionStartLib, ProtectionStart);

        QuantLib::Date UpfrontDateLib;
        calcToScalar(UpfrontDateLib, UpfrontDate);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Protection::Side BuyerSellerEnum =
            ObjectHandler::Create<QuantLib::Protection::Side>()(BuyerSellerCpp);

        QuantLib::BusinessDayConvention PaymentConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // convert object IDs into library objects

        OH_GET_REFERENCE(PremiumScheduleLibObjPtr, PremiumScheduleCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlCreditDefaultSwap(
                ObjectIdCpp,
                BuyerSellerCpp,
                Notional,
                Upfront,
                Spread,
                PremiumScheduleCpp,
                PaymentConventionCpp,
                DayCounterCpp,
                SettlesAccrualCpp,
                PayAtDefaultCpp,
                ProtectionStartCpp,
                UpfrontDateCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::CreditDefaultSwap(
                valueObject,
                BuyerSellerEnum,
                NotionalLib,
                Upfront,
                Spread,
                PremiumScheduleLibObjPtr,
                PaymentConventionEnum,
                DayCounterEnum,
                SettlesAccrualCpp,
                PayAtDefaultCpp,
                ProtectionStartLib,
                UpfrontDateLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCreditDefaultSwap: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlHazardRateCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &CurveRates,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> CurveRatesCpp;
        calcToVector(CurveRatesCpp, CurveRates);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> CurveDatesLib;
        calcToVector(CurveDatesLib, CurveDates);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlHazardRateCurve(
                ObjectIdCpp,
                CurveDatesCpp,
                CurveRatesCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::HazardRateCurve(
                valueObject,
                CurveDatesLib,
                CurveRatesCpp,
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
        OH_LOG_MESSAGE("ERROR: qlHazardRateCurve: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlMidPointCdsEngine(
        const STRING &ObjectId,
        const STRING &DefaultCurve,
        double RecoveryRate,
        const STRING &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string DefaultCurveCpp = ouStringToStlString(DefaultCurve);

        std::string YieldCurveCpp = ouStringToStlString(YieldCurve);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Real RecoveryRateLib;
        calcToScalar(RecoveryRateLib, RecoveryRate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(DefaultCurveCoerce, DefaultCurveCpp, ObjectHandler::Object)

	  // FIXME !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  // works for hazard rate structure only

	  boost::shared_ptr<QuantLibAddin::TermStructure> 
	  qlats = boost::dynamic_pointer_cast<QuantLibAddin::TermStructure>(DefaultCurveCoerce);

	boost::shared_ptr<QuantLib::Extrapolator> qlex;
	qlats->getLibraryObject(qlex); 

	boost::shared_ptr<QuantLib::HazardRateStructure> 
	  qlts1 = boost::dynamic_pointer_cast<QuantLib::HazardRateStructure>(qlex);
	boost::shared_ptr<QuantLib::DefaultProbabilityTermStructure> 
	  qlts = qlts1; 

	QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure> DefaultCurveLibObj(qlts1);

	// END OF FIXME ///////////////////////////////////////////////////////

	/*
         QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure> DefaultCurveLibObj =
             QuantLibAddin::CoerceHandle<
                 QuantLibAddin::DefaultProbabilityTermStructure,
                 QuantLib::DefaultProbabilityTermStructure>()(
                     DefaultCurveCoerce, QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure>());
	*/

        OH_GET_OBJECT_DEFAULT(YieldCurveCoerce, YieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> YieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    YieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMidPointCdsEngine(
                ObjectIdCpp,
                DefaultCurveCpp,
                RecoveryRate,
                YieldCurveCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::MidPointCdsEngine(
                valueObject,
                DefaultCurveLibObj,
                RecoveryRateLib,
                YieldCurveLibObj,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlMidPointCdsEngine: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlPiecewiseFlatForwardCurve(
        const STRING &ObjectId,
        const ANY &ReferenceDate,
        const SEQSEQ(ANY) &RateHelpers,
        const ANY &DayCounter,
        const ANY &Accuracy,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        long ReferenceDateCpp;
        calcToScalar(ReferenceDateCpp, ReferenceDate);

        std::vector<std::string> RateHelpersCpp;
        calcToVector(RateHelpersCpp, RateHelpers);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        double AccuracyCpp;
        calcToScalar(AccuracyCpp, Accuracy);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date ReferenceDateLib;
        calcToScalar(ReferenceDateLib, ReferenceDate);

        std::vector<boost::shared_ptr<QuantLib::RateHelper> > RateHelpersLibObjPtr =
            ObjectHandler::getLibraryObjectVector<QuantLibAddin::RateHelper, QuantLib::RateHelper>(RateHelpersCpp);

        QuantLib::Real AccuracyLib;
        calcToScalar(AccuracyLib, Accuracy);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlPiecewiseFlatForwardCurve(
                ObjectIdCpp,
                ReferenceDateCpp,
                RateHelpersCpp,
                DayCounterCpp,
                AccuracyCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::PiecewiseFlatForwardCurve(
                valueObject,
                ReferenceDateLib,
                RateHelpersLibObjPtr,
                DayCounterEnum,
                AccuracyLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlPiecewiseFlatForwardCurve: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlPiecewiseFlatHazardRateCurve(
        const STRING &ObjectId,
        const ANY &ReferenceDate,
        const SEQSEQ(ANY) &Helpers,
        const ANY &DayCounter,
        const ANY &Accuracy,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        long ReferenceDateCpp;
        calcToScalar(ReferenceDateCpp, ReferenceDate);

        std::vector<std::string> HelpersCpp;
        calcToVector(HelpersCpp, Helpers);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        double AccuracyCpp;
        calcToScalar(AccuracyCpp, Accuracy);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date ReferenceDateLib;
        calcToScalar(ReferenceDateLib, ReferenceDate);

        std::vector<boost::shared_ptr<QuantLib::DefaultProbabilityHelper> > HelpersLibObjPtr =
            ObjectHandler::getLibraryObjectVector<QuantLibAddin::DefaultProbabilityHelper, QuantLib::DefaultProbabilityHelper>(HelpersCpp);

        QuantLib::Real AccuracyLib;
        calcToScalar(AccuracyLib, Accuracy);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlPiecewiseFlatHazardRateCurve(
                ObjectIdCpp,
                ReferenceDateCpp,
                HelpersCpp,
                DayCounterCpp,
                AccuracyCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::PiecewiseFlatHazardRateCurve(
                valueObject,
                ReferenceDateLib,
                HelpersLibObjPtr,
                DayCounterEnum,
                AccuracyLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlPiecewiseFlatHazardRateCurve: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSpreadCdsHelper(
        const STRING &ObjectId,
        const STRING &RunningSpread,
        const STRING &Tenor,
        const ANY &SettlementDays,
        const STRING &Calendar,
        const STRING &Frequency,
        const STRING &PaymentConvention,
        const STRING &GenRule,
        const STRING &DayCounter,
        double RecoveryRate,
        const STRING &DiscountingCurve,
        const ANY &SettleAccrual,
        const ANY &PayAtDefault,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string RunningSpreadCpp = ouStringToStlString(RunningSpread);

        std::string TenorCpp = ouStringToStlString(Tenor);

        long SettlementDaysCpp;
        calcToScalar(SettlementDaysCpp, SettlementDays);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string FrequencyCpp = ouStringToStlString(Frequency);

        std::string PaymentConventionCpp = ouStringToStlString(PaymentConvention);

        std::string GenRuleCpp = ouStringToStlString(GenRule);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        std::string DiscountingCurveCpp = ouStringToStlString(DiscountingCurve);

        bool SettleAccrualCpp;
        calcToScalar(SettleAccrualCpp, SettleAccrual);

        bool PayAtDefaultCpp;
        calcToScalar(PayAtDefaultCpp, PayAtDefault);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Natural SettlementDaysLib;
        calcToScalar(SettlementDaysLib, SettlementDays);

        QuantLib::Real RecoveryRateLib;
        calcToScalar(RecoveryRateLib, RecoveryRate);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::Frequency FrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FrequencyCpp);

        QuantLib::BusinessDayConvention PaymentConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentConventionCpp);

        QuantLib::DateGeneration::Rule GenRuleEnum =
            ObjectHandler::Create<QuantLib::DateGeneration::Rule>()(GenRuleCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RunningSpreadCoerce, RunningSpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RunningSpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RunningSpreadCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSpreadCdsHelper(
                ObjectIdCpp,
                RunningSpreadCpp,
                TenorCpp,
                SettlementDaysCpp,
                CalendarCpp,
                FrequencyCpp,
                PaymentConventionCpp,
                GenRuleCpp,
                DayCounterCpp,
                RecoveryRate,
                DiscountingCurveCpp,
                SettleAccrualCpp,
                PayAtDefaultCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SpreadCdsHelper(
                valueObject,
                RunningSpreadLibObj,
                TenorLib,
                SettlementDaysLib,
                CalendarEnum,
                FrequencyEnum,
                PaymentConventionEnum,
                GenRuleEnum,
                DayCounterEnum,
                RecoveryRateLib,
                DiscountingCurveLibObj,
                SettleAccrualCpp,
                PayAtDefaultCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSpreadCdsHelper: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlUpfrontCdsHelper(
        const STRING &ObjectId,
        const STRING &UpfrontSpread,
        double RunningSpread,
        const STRING &Tenor,
        const ANY &SettlementDays,
        const STRING &Calendar,
        const STRING &Frequency,
        const STRING &PaymentConvention,
        const STRING &GenRule,
        const STRING &DayCounter,
        double RecoveryRate,
        const STRING &DiscountingCurve,
        sal_Int32 UpfrontSettlementDays,
        const ANY &SettleAccrual,
        const ANY &PayAtDefault,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string UpfrontSpreadCpp = ouStringToStlString(UpfrontSpread);

        std::string TenorCpp = ouStringToStlString(Tenor);

        long SettlementDaysCpp;
        calcToScalar(SettlementDaysCpp, SettlementDays);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string FrequencyCpp = ouStringToStlString(Frequency);

        std::string PaymentConventionCpp = ouStringToStlString(PaymentConvention);

        std::string GenRuleCpp = ouStringToStlString(GenRule);

        std::string DayCounterCpp = ouStringToStlString(DayCounter);

        std::string DiscountingCurveCpp = ouStringToStlString(DiscountingCurve);

        bool SettleAccrualCpp;
        calcToScalar(SettleAccrualCpp, SettleAccrual);

        bool PayAtDefaultCpp;
        calcToScalar(PayAtDefaultCpp, PayAtDefault);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Natural SettlementDaysLib;
        calcToScalar(SettlementDaysLib, SettlementDays);

        QuantLib::Real RecoveryRateLib;
        calcToScalar(RecoveryRateLib, RecoveryRate);

        QuantLib::Integer UpfrontSettlementDaysLib;
        calcToScalar(UpfrontSettlementDaysLib, UpfrontSettlementDays);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::Frequency FrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FrequencyCpp);

        QuantLib::BusinessDayConvention PaymentConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(PaymentConventionCpp);

        QuantLib::DateGeneration::Rule GenRuleEnum =
            ObjectHandler::Create<QuantLib::DateGeneration::Rule>()(GenRuleCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(UpfrontSpreadCoerce, UpfrontSpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> UpfrontSpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    UpfrontSpreadCoerce, QuantLib::Handle<QuantLib::Quote>());

        OH_GET_OBJECT_DEFAULT(DiscountingCurveCoerce, DiscountingCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> DiscountingCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    DiscountingCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlUpfrontCdsHelper(
                ObjectIdCpp,
                UpfrontSpreadCpp,
                RunningSpread,
                TenorCpp,
                SettlementDaysCpp,
                CalendarCpp,
                FrequencyCpp,
                PaymentConventionCpp,
                GenRuleCpp,
                DayCounterCpp,
                RecoveryRate,
                DiscountingCurveCpp,
                UpfrontSettlementDays,
                SettleAccrualCpp,
                PayAtDefaultCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::UpfrontCdsHelper(
                valueObject,
                UpfrontSpreadLibObj,
                RunningSpread,
                TenorLib,
                SettlementDaysLib,
                CalendarEnum,
                FrequencyEnum,
                PaymentConventionEnum,
                GenRuleEnum,
                DayCounterEnum,
                RecoveryRateLib,
                DiscountingCurveLibObj,
                UpfrontSettlementDaysLib,
                SettleAccrualCpp,
                PayAtDefaultCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlUpfrontCdsHelper: " << e.what());
        THROW_RTE;
    }
}


