
/*  
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007, 2009 Ferdinando Ametrano
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
#include <qlo/enumerations/factories/calendarfactory.hpp>
#include <qlo/handleimpl.hpp>
#include <qlo/conversions/coercetermstructure.hpp>
#include <qlo/ratehelpers.hpp>
#include <qlo/valueobjects/vo_termstructures.hpp>
#include <qlo/loop/loop_termstructures.hpp>
#include <loop.hpp>
#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDiscountCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &CurveDiscounts,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> CurveDiscountsCpp;
        calcToVector(CurveDiscountsCpp, CurveDiscounts);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

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
            new QuantLibAddin::ValueObjects::qlDiscountCurve(
                ObjectIdCpp,
                CurveDatesCpp,
                CurveDiscountsCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DiscountCurve(
                valueObject,
                CurveDatesLib,
                CurveDiscountsCpp,
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
            errorMsg << "ERROR: qlDiscountCurve: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFlatForward(
        const ANY &ObjectId,
        const ANY &NDays,
        const ANY &Calendar,
        const ANY &Rate,
        const ANY &DayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        long NDaysCpp;
        calcToScalar(NDaysCpp, NDays);

        std::string CalendarCpp;
        if(Calendar.hasValue()) 
            calcToScalar(CalendarCpp, Calendar);
        else
            CalendarCpp = "NullCalendar";

        std::string RateCpp;
        calcToScalar(RateCpp, Rate);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

        std::string CompoundingCpp;
        if(Compounding.hasValue()) 
            calcToScalar(CompoundingCpp, Compounding);
        else
            CompoundingCpp = "Continuous";

        std::string FrequencyCpp;
        if(Frequency.hasValue()) 
            calcToScalar(FrequencyCpp, Frequency);
        else
            FrequencyCpp = "Annual";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Size NDaysLib;
        calcToScalar(NDaysLib, NDays);

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

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        QuantLib::Compounding CompoundingEnum =
            ObjectHandler::Create<QuantLib::Compounding>()(CompoundingCpp);

        QuantLib::Frequency FrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FrequencyCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFlatForward(
                ObjectIdCpp,
                NDaysCpp,
                CalendarCpp,
                RateCpp,
                DayCounterCpp,
                CompoundingCpp,
                FrequencyCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FlatForward(
                valueObject,
                NDaysLib,
                CalendarEnum,
                RateLibObj,
                DayCounterEnum,
                CompoundingEnum,
                FrequencyEnum,
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
            errorMsg << "ERROR: qlFlatForward: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlForwardCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &ForwardYields,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> ForwardYieldsCpp;
        calcToVector(ForwardYieldsCpp, ForwardYields);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

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
            new QuantLibAddin::ValueObjects::qlForwardCurve(
                ObjectIdCpp,
                CurveDatesCpp,
                ForwardYieldsCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::ForwardCurve(
                valueObject,
                CurveDatesLib,
                ForwardYieldsCpp,
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
            errorMsg << "ERROR: qlForwardCurve: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlForwardSpreadedTermStructure(
        const ANY &ObjectId,
        const ANY &BaseYieldCurve,
        const ANY &Spread,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string BaseYieldCurveCpp;
        calcToScalar(BaseYieldCurveCpp, BaseYieldCurve);

        std::string SpreadCpp;
        calcToScalar(SpreadCpp, Spread);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(BaseYieldCurveCoerce, BaseYieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> BaseYieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    BaseYieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        OH_GET_OBJECT_DEFAULT(SpreadCoerce, SpreadCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> SpreadLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    SpreadCoerce, QuantLib::Handle<QuantLib::Quote>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlForwardSpreadedTermStructure(
                ObjectIdCpp,
                BaseYieldCurveCpp,
                SpreadCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::ForwardSpreadedTermStructure(
                valueObject,
                BaseYieldCurveLibObj,
                SpreadLibObj,
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
            errorMsg << "ERROR: qlForwardSpreadedTermStructure: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlImpliedTermStructure(
        const ANY &ObjectId,
        const ANY &BaseYieldCurve,
        const ANY &ReferenceDate,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string BaseYieldCurveCpp;
        calcToScalar(BaseYieldCurveCpp, BaseYieldCurve);

        ObjectHandler::property_t ReferenceDateCpp;
        calcToScalar(ReferenceDateCpp, ReferenceDate);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date ReferenceDateLib;
        calcToScalar(ReferenceDateLib, ReferenceDate);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(BaseYieldCurveCoerce, BaseYieldCurveCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::YieldTermStructure> BaseYieldCurveLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    BaseYieldCurveCoerce, QuantLib::Handle<QuantLib::YieldTermStructure>());

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlImpliedTermStructure(
                ObjectIdCpp,
                BaseYieldCurveCpp,
                ReferenceDateCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::ImpliedTermStructure(
                valueObject,
                BaseYieldCurveLibObj,
                ReferenceDateLib,
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
            errorMsg << "ERROR: qlImpliedTermStructure: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlInterpolatedYieldCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const SEQSEQ(ANY) &Data,
        const ANY &Calendar,
        const ANY &DayCounter,
        const SEQSEQ(ANY) &Jumps,
        const SEQSEQ(ANY) &JumpDates,
        const ANY &TraitsID,
        const ANY &InterpolatorID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::vector<ObjectHandler::property_t> DatesCpp;
        calcToVector(DatesCpp, Dates);

        std::vector<double> DataCpp;
        calcToVector(DataCpp, Data);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

        std::vector<ObjectHandler::property_t> JumpsCpp;
        calcToVector(JumpsCpp, Jumps);

        std::vector<ObjectHandler::property_t> JumpDatesCpp;
        calcToVector(JumpDatesCpp, JumpDates);

        std::string TraitsIDCpp;
        if(TraitsID.hasValue()) 
            calcToScalar(TraitsIDCpp, TraitsID);
        else
            TraitsIDCpp = "Discount";

        std::string InterpolatorIDCpp;
        if(InterpolatorID.hasValue()) 
            calcToScalar(InterpolatorIDCpp, InterpolatorID);
        else
            InterpolatorIDCpp = "MonotonicLogCubicNaturalSpline";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DatesLib;
        calcToVector(DatesLib, Dates);

        std::vector<QuantLib::Real> DataLib;
        calcToVector(DataLib, Data);

        std::vector<QuantLib::Handle<QuantLib::Quote> > JumpsLibObj =
            ObjectHandler::vector::convert2<QuantLib::Handle<QuantLib::Quote> >(JumpsCpp, "Jumps");

        std::vector<QuantLib::Date> JumpDatesLib;
        calcToVector(JumpDatesLib, JumpDates);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlInterpolatedYieldCurve(
                ObjectIdCpp,
                DatesCpp,
                DataCpp,
                CalendarCpp,
                DayCounterCpp,
                JumpsCpp,
                JumpDatesCpp,
                TraitsIDCpp,
                InterpolatorIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::InterpolatedYieldCurve(
                valueObject,
                DatesLib,
                DataLib,
                CalendarEnum,
                DayCounterEnum,
                JumpsLibObj,
                JumpDatesLib,
                TraitsIDCpp,
                InterpolatorIDCpp,
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
            errorMsg << "ERROR: qlInterpolatedYieldCurve: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRelinkableHandleYieldTermStructure(
        const ANY &ObjectId,
        const ANY &CurrentLink,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string CurrentLinkCpp;
        if(CurrentLink.hasValue()) 
            calcToScalar(CurrentLinkCpp, CurrentLink);
        else
            CurrentLinkCpp = "";

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlRelinkableHandleYieldTermStructure(
                ObjectIdCpp,
                CurrentLinkCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::RelinkableHandleImpl<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure>(
                valueObject,
                CurrentLinkCpp,
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
            errorMsg << "ERROR: qlRelinkableHandleYieldTermStructure: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlTermStructureCalendar(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::TermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::TermStructure,
                QuantLib::TermStructure>()(
                    ObjectIdTemp);

        // invoke the member function

        QuantLib::Calendar returnValue = ObjectIdLibObjPtr->calendar();

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
            errorMsg << "ERROR: qlTermStructureCalendar: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlTermStructureMaxDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::TermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::TermStructure,
                QuantLib::TermStructure>()(
                    ObjectIdTemp);

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->maxDate();

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
            errorMsg << "ERROR: qlTermStructureMaxDate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlTermStructureReferenceDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::TermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::TermStructure,
                QuantLib::TermStructure>()(
                    ObjectIdTemp);

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->referenceDate();

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
            errorMsg << "ERROR: qlTermStructureReferenceDate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlTermStructureSettlementDays(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::TermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::TermStructure,
                QuantLib::TermStructure>()(
                    ObjectIdTemp);

        // invoke the member function

        QuantLib::Natural returnValue = ObjectIdLibObjPtr->settlementDays();

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
            errorMsg << "ERROR: qlTermStructureSettlementDays: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlYieldTSDiscount(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &DfDates,
        const sal_Int32 AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        bool AllowExtrapolationCpp;
        calcToScalar(AllowExtrapolationCpp, AllowExtrapolation);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DfDatesLib;
        calcToVector(DfDatesLib, DfDates);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::YieldTermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    ObjectIdTemp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlYieldTSDiscountBind bindObject = 
            boost::bind((QuantLibAddin::qlYieldTSDiscountSignature)
                    &QuantLib::YieldTermStructure::discount, 
                ObjectIdLibObjPtr,
                _1,
                AllowExtrapolationCpp);
                    
        {
            returnValue.realloc(DfDatesLib.size());
            for (unsigned int i=0; i<DfDatesLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DfDatesLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlYieldTSDiscountBind, QuantLib::Date, double>
            (functionCall, bindObject, DfDatesLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlYieldTSDiscount: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlYieldTSZeroRate(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const ANY &ResultDayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const sal_Int32 AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string ResultDayCounterCpp;
        calcToScalar(ResultDayCounterCpp, ResultDayCounter);

        std::string CompoundingCpp;
        if(Compounding.hasValue()) 
            calcToScalar(CompoundingCpp, Compounding);
        else
            CompoundingCpp = "Continuous";

        std::string FrequencyCpp;
        if(Frequency.hasValue()) 
            calcToScalar(FrequencyCpp, Frequency);
        else
            FrequencyCpp = "Annual";

        bool AllowExtrapolationCpp;
        calcToScalar(AllowExtrapolationCpp, AllowExtrapolation);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DatesLib;
        calcToVector(DatesLib, Dates);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::YieldTermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::YieldTermStructure,
                QuantLib::YieldTermStructure>()(
                    ObjectIdTemp);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter ResultDayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(ResultDayCounterCpp);

        QuantLib::Compounding CompoundingEnum =
            ObjectHandler::Create<QuantLib::Compounding>()(CompoundingCpp);

        QuantLib::Frequency FrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FrequencyCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlYieldTSZeroRateBind bindObject = 
            boost::bind((QuantLibAddin::qlYieldTSZeroRateSignature)
                    &QuantLib::YieldTermStructure::zeroRate, 
                ObjectIdLibObjPtr,
                _1,
                ResultDayCounterEnum,
                CompoundingEnum,
                FrequencyEnum,
                AllowExtrapolationCpp);
                    
        {
            returnValue.realloc(DatesLib.size());
            for (unsigned int i=0; i<DatesLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DatesLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlYieldTSZeroRateBind, QuantLib::Date, QuantLib::InterestRate>
            (functionCall, bindObject, DatesLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlYieldTSZeroRate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlZeroCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &CurveYields,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> CurveYieldsCpp;
        calcToVector(CurveYieldsCpp, CurveYields);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

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
            new QuantLibAddin::ValueObjects::qlZeroCurve(
                ObjectIdCpp,
                CurveDatesCpp,
                CurveYieldsCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::ZeroCurve(
                valueObject,
                CurveDatesLib,
                CurveYieldsCpp,
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
            errorMsg << "ERROR: qlZeroCurve: " << e.what(); 
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


