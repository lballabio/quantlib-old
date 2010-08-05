
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
//      gensrc/gensrc/stubs/stub.calc.includes

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
//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

#include <iostream>

STRING SAL_CALL CalcAddins_impl::qlDiscountCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &CurveDiscounts,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> CurveDiscountsCpp;
        calcToVector(CurveDiscountsCpp, CurveDiscounts);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlDiscountCurve: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlFlatForward(
        const STRING &ObjectId,
        const ANY &NDays,
        const ANY &Calendar,
        const STRING &Rate,
        const ANY &DayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        long NDaysCpp;
        calcToScalar(NDaysCpp, NDays);

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        std::string RateCpp = ouStringToStlString(Rate);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        std::string CompoundingCpp;
        calcToScalar(CompoundingCpp, Compounding);

        std::string FrequencyCpp;
        calcToScalar(FrequencyCpp, Frequency);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Size NDaysLib;
        calcToScalar(NDaysLib, NDays);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        QuantLib::Compounding CompoundingEnum =
            ObjectHandler::Create<QuantLib::Compounding>()(CompoundingCpp);

        QuantLib::Frequency FrequencyEnum =
            ObjectHandler::Create<QuantLib::Frequency>()(FrequencyCpp);

        // convert object IDs into library objects

        OH_GET_OBJECT_DEFAULT(RateCoerce, RateCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> RateLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    RateCoerce, QuantLib::Handle<QuantLib::Quote>());

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlFlatForward: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlForwardCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &ForwardYields,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> ForwardYieldsCpp;
        calcToVector(ForwardYieldsCpp, ForwardYields);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlForwardCurve: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlForwardSpreadedTermStructure(
        const STRING &ObjectId,
        const STRING &BaseYieldCurve,
        const STRING &Spread,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string BaseYieldCurveCpp = ouStringToStlString(BaseYieldCurve);

        std::string SpreadCpp = ouStringToStlString(Spread);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlForwardSpreadedTermStructure: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlImpliedTermStructure(
        const STRING &ObjectId,
        const STRING &BaseYieldCurve,
        const ANY &ReferenceDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string BaseYieldCurveCpp = ouStringToStlString(BaseYieldCurve);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlImpliedTermStructure: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlRelinkableHandleYieldTermStructure(
        const STRING &ObjectId,
        const ANY &CurrentLink,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string CurrentLinkCpp;
        calcToScalar(CurrentLinkCpp, CurrentLink);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlRelinkableHandleYieldTermStructure: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlTermStructureCalendar(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

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




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlTermStructureCalendar: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlTermStructureMaxDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

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




        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlTermStructureMaxDate: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlTermStructureReferenceDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdTemp, ObjectIdCpp, ObjectHandler::Object)

	  std::cout <<std::endl
		    << "ObjectID: " << ObjectIdTemp->properties()->objectId() 
		    << std::endl
		    << "ClassName: " << ObjectIdTemp->properties()->className()
		    << std::endl;
       	    	
	QuantLib::Date returnValue;
	
	// FIXME:
	// Here is a "pedestrian" coerce for hazard rate curve and the piecewise
	// flat hazard rate curve. The "default" coerce (see lines 651 ff) does not
	// work. Why?
	if (ObjectIdTemp->properties()->className() == "qlHazardRateCurve") {
	  // this is part of coerce
	  boost::shared_ptr<QuantLibAddin::TermStructure> 
	    qlats = boost::dynamic_pointer_cast<QuantLibAddin::TermStructure>(ObjectIdTemp);

	  boost::shared_ptr<QuantLib::Extrapolator> qlex;
	  qlats->getLibraryObject(qlex); 

	  // this interim cast seems necessary 
	  // direct cast to TermStructure fails 
	  boost::shared_ptr<QuantLib::HazardRateStructure> 
	    qlts1 = boost::dynamic_pointer_cast<QuantLib::HazardRateStructure>(qlex);
	  boost::shared_ptr<QuantLib::TermStructure> qlts = qlts1; 

	  returnValue = qlts->referenceDate();
	}
	else if (ObjectIdTemp->properties()->className() 
		 == "qlPiecewiseFlatHazardRateCurve") {
	  // this is part of coerce
	  boost::shared_ptr<QuantLibAddin::TermStructure> 
	    qlats = boost::dynamic_pointer_cast<QuantLibAddin::TermStructure>(ObjectIdTemp);

	  boost::shared_ptr<QuantLib::Extrapolator> qlex;
	  qlats->getLibraryObject(qlex); 

	  // this interim cast seems necessary 
	  // direct cast to TermStructure fails 
	  typedef QuantLib::PiecewiseDefaultCurve<QuantLib::HazardRate, QuantLib::BackwardFlat> PiecewiseFlatHazardRateCurve;
	  boost::shared_ptr<PiecewiseFlatHazardRateCurve> 
	    qlts1 = boost::dynamic_pointer_cast<PiecewiseFlatHazardRateCurve>(qlex);
	  boost::shared_ptr<QuantLib::TermStructure> qlts = qlts1; 

	  returnValue = qlts->referenceDate();
	}
	else {
	  // FIXME:
	  // This is the autogenerated code which does not work for the
	  // default term structures
        boost::shared_ptr<QuantLib::TermStructure> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceTermStructure<
                QuantLibAddin::TermStructure,
                QuantLib::TermStructure>()(
                    ObjectIdTemp);

        // invoke the member function
	returnValue = ObjectIdLibObjPtr->referenceDate();

 	}

	std::cout << "term structure reference date = " 
		  << returnValue << std::endl << std::endl;



        // convert and return the return value




        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlTermStructureReferenceDate: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlTermStructureSettlementDays(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

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




        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlTermStructureSettlementDays: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlZeroCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &CurveYields,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::vector<ObjectHandler::property_t> CurveDatesCpp;
        calcToVector(CurveDatesCpp, CurveDates);

        std::vector<double> CurveYieldsCpp;
        calcToVector(CurveYieldsCpp, CurveYields);

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
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlZeroCurve: " << e.what());
        THROW_RTE;
    }
}


