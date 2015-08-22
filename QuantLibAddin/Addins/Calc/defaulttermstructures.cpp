
/*  
 Copyright (C) 2009 Ferdinando Ametrano
 
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
#include <qlo/valueobjects/vo_defaulttermstructures.hpp>
#include <qlo/loop/loop_defaulttermstructures.hpp>
#include <loop.hpp>
#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDefaultTSDefaultProbability(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const sal_Int32 AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        bool AllowExtrapolationCpp;
        calcToScalar(AllowExtrapolationCpp, AllowExtrapolation);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DatesLib;
        calcToVector(DatesLib, Dates);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)
        boost::shared_ptr<QuantLib::DefaultProbabilityTermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::DefaultProbabilityTermStructure,
                QuantLib::DefaultProbabilityTermStructure>()(
                    ObjectIdTemp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlDefaultTSDefaultProbabilityBind bindObject = 
            boost::bind((QuantLibAddin::qlDefaultTSDefaultProbabilitySignature)
                    &QuantLib::DefaultProbabilityTermStructure::defaultProbability, 
                ObjectIdLibObjPtr,
                _1,
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
            <QuantLibAddin::qlDefaultTSDefaultProbabilityBind, QuantLib::Date, double>
            (functionCall, bindObject, DatesLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlDefaultTSDefaultProbability: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlFlatHazardRate(
        const ANY &ObjectId,
        const ANY &NDays,
        const ANY &Calendar,
        const ANY &Rate,
        const ANY &DayCounter,
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

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlFlatHazardRate(
                ObjectIdCpp,
                NDaysCpp,
                CalendarCpp,
                RateCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::FlatHazardRate(
                valueObject,
                NDaysLib,
                CalendarEnum,
                RateLibObj,
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
            errorMsg << "ERROR: qlFlatHazardRate: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlRelinkableHandleDefaultProbabilityTermStructure(
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
            new QuantLibAddin::ValueObjects::qlRelinkableHandleDefaultProbabilityTermStructure(
                ObjectIdCpp,
                CurrentLinkCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::RelinkableHandleImpl<QuantLibAddin::DefaultProbabilityTermStructure, QuantLib::DefaultProbabilityTermStructure>(
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
            errorMsg << "ERROR: qlRelinkableHandleDefaultProbabilityTermStructure: " << e.what(); 
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


