
/*  
 Copyright (C) 2007, 2009 Ferdinando Ametrano
 Copyright (C) 2007 Eric Ehlers
 
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
#include <qlo/yieldtermstructures.hpp>
#include <qlo/piecewiseyieldcurve.hpp>
#include <qlo/ratehelpers.hpp>
#include <qlo/handleimpl.hpp>
#include <ql/interestrate.hpp>
#include <qlo/conversions/coercetermstructure.hpp>
#include <qlo/valueobjects/vo_piecewiseyieldcurve.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

#include <iostream>
using namespace std;

STRING SAL_CALL CalcAddins_impl::qlPiecewiseYieldCurve(
        const STRING &ObjectId,
        const ANY &NDays,
        const STRING &Calendar,
        const SEQSEQ(ANY) &RateHelpers,
        const ANY &DayCounter,
        const SEQSEQ(ANY) &Jumps,
        const SEQSEQ(ANY) &JumpDates,
        const ANY &Accuracy,
        const ANY &TraitsID,
        const ANY &InterpolatorID,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        long NDaysCpp;
        calcToScalar(NDaysCpp, NDays);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::vector<std::string> RateHelpersCpp;
	calcToVector(RateHelpersCpp, RateHelpers);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        std::vector<ObjectHandler::property_t> JumpsCpp;
	// FIXME:
	// calcToVector yields a vector of length 1 even if the sequence in Calc is
	// empty. Why, how can we fix it?
	// So I am temporyrily ignoring the input from Calc.
	/*
	std::cout << "calling  calcToVector(JumpsCpp, Jumps)" << std::endl;
	calcToVector(JumpsCpp, Jumps);
	std::cout << "JumpsCpp.size() " << JumpsCpp.size() << std::endl; 
	*/
        std::vector<ObjectHandler::property_t> JumpDatesCpp;
	// FIXME
	// see comment above
	/*
	std::cout << "calling calcToVector(JumpDatesCpp, JumpDates)" << std::endl;
        calcToVector(JumpDatesCpp, JumpDates);
	std::cout << "JumpDatesCpp.size() " << JumpDatesCpp.size() << std::endl; 
	*/

        double AccuracyCpp;
        calcToScalar(AccuracyCpp, Accuracy);

        std::string TraitsIDCpp;
        calcToScalar(TraitsIDCpp, TraitsID);

        std::string InterpolatorIDCpp;
        calcToScalar(InterpolatorIDCpp, InterpolatorID);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Natural NDaysLib;
        calcToScalar(NDaysLib, NDays);

        std::vector<boost::shared_ptr<QuantLib::RateHelper> > RateHelpersLibObjPtr =
            ObjectHandler::getLibraryObjectVector<QuantLibAddin::RateHelper, QuantLib::RateHelper>(RateHelpersCpp);

	std::vector<QuantLib::Handle<QuantLib::Quote> > JumpsLibObj
	  = ObjectHandler::vector::convert2<QuantLib::Handle<QuantLib::Quote> >(JumpsCpp, "Jumps");

	std::vector<QuantLib::Date> JumpDatesLib;
	calcToVector(JumpDatesLib, JumpDates);
	
        QuantLib::Real AccuracyLib;
        calcToScalar(AccuracyLib, Accuracy);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlPiecewiseYieldCurve(
                ObjectIdCpp,
                NDaysCpp,
                CalendarCpp,
                RateHelpersCpp,
                DayCounterCpp,
                JumpsCpp,
                JumpDatesCpp,
                AccuracyCpp,
                TraitsIDCpp,
                InterpolatorIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::PiecewiseYieldCurve(
                valueObject,
                NDaysLib,
                CalendarEnum,
                RateHelpersLibObjPtr,
                DayCounterEnum,
                JumpsLibObj,
                JumpDatesLib,
                AccuracyLib,
                TraitsIDCpp,
                InterpolatorIDCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value




        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlPiecewiseYieldCurve: " << e.what());
        THROW_RTE;
    }
}


