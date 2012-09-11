
/*  
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2006 Francois du Vignaud
 Copyright (C) 2006 Giorgio Facchinetti
 
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
#include <qlo/quotes.hpp>
#include <qlo/indexes/iborindex.hpp>
#include <qlo/indexes/swapindex.hpp>
#include <qlo/handleimpl.hpp>
#include <qlo/conversions/coercequote.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/quotes/derivedquote.hpp>
#include <ql/quotes/eurodollarfuturesquote.hpp>
#include <ql/quotes/forwardvaluequote.hpp>
#include <ql/quotes/futuresconvadjustmentquote.hpp>
#include <ql/quotes/lastfixingquote.hpp>
#include <ql/quotes/impliedstddevquote.hpp>
#include <ql/experimental/risk/sensitivityanalysis.hpp>
#include <ql/indexes/swapindex.hpp>
#include <qlo/capletvolstructure.hpp>
#include <qlo/baseinstruments.hpp>
#include <qlo/valueobjects/vo_quotes.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

double SAL_CALL CalcAddins_impl::qlQuoteValue(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

	// convert object IDs into library objects
	// RL: conversion to library object added manually, fix gensrc

        OH_GET_OBJECT(ObjectIdCoerce, ObjectIdCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::SimpleQuote> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::SimpleQuote,
                QuantLib::SimpleQuote>()(
                    ObjectIdCoerce);

        // invoke the member function

        QuantLib::Real returnValue = ObjectIdLibObjPtr->value();

        // convert and return the return value



        double returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlQuoteValue: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlRelinkableHandleQuote(
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
            new QuantLibAddin::ValueObjects::qlRelinkableHandleQuote(
                ObjectIdCpp,
                CurrentLinkCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::RelinkableHandleImpl<QuantLibAddin::Quote, QuantLib::Quote>(
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
        OH_LOG_MESSAGE("ERROR: qlRelinkableHandleQuote: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSimpleQuote(
        const STRING &ObjectId,
        const ANY &Value,
        double TickValue,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        double ValueCpp;
        calcToScalar(ValueCpp, Value);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Real ValueLib;
        calcToScalar(ValueLib, Value);

        QuantLib::Real TickValueLib;
        calcToScalar(TickValueLib, TickValue);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSimpleQuote(
                ObjectIdCpp,
                ValueCpp,
                TickValue,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::SimpleQuote(
                valueObject,
                ValueLib,
                TickValueLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSimpleQuote: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlSimpleQuoteReset(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

	// convert object IDs into library objects
	// RL: conversion to library object added manually, fix gensrc

        OH_GET_OBJECT(ObjectIdCoerce, ObjectIdCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::SimpleQuote> ObjectIdLibObjPtr =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::SimpleQuote,
                QuantLib::SimpleQuote>()(
                    ObjectIdCoerce);

        // invoke the member function

        ObjectIdLibObjPtr->reset();

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSimpleQuoteReset: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlSimpleQuoteSetValue(
        const STRING &ObjectId,
        const ANY &Value,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        double ValueCpp;
        calcToScalar(ValueCpp, Value);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Real ValueLib;
        calcToScalar(ValueLib, Value);

	// convert object IDs into library objects
	// RL: conversion to library object added manually, fix gensrc

        OH_GET_OBJECT(ObjectIdCoerce, ObjectIdCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::SimpleQuote> ObjectIdObjPtr =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::SimpleQuote,
                QuantLib::SimpleQuote>()(
                    ObjectIdCoerce);

        // invoke the member function

        QuantLib::Real returnValue = ObjectIdObjPtr->setValue(
                ValueLib);

        // convert and return the return value



        double returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSimpleQuoteSetValue: " << e.what());
        THROW_RTE;
    }
}


