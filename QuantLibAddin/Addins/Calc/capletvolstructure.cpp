
/*  
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 
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
#include <qlo/indexes/iborindex.hpp>
#include <qlo/capletvolstructure.hpp>
#include <qlo/quote.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/handleimpl.hpp>
#include <ql/termstructures/volatility/optionlet/optionletvolatilitystructure.hpp>
#include <ql/termstructures/volatility/optionlet/constantoptionletvol.hpp>
#include <ql/termstructures/volatility/optionlet/optionletstripper1.hpp>
#include <ql/termstructures/volatility/optionlet/optionletstripper2.hpp>
#include <ql/termstructures/volatility/capfloor/capfloortermvolcurve.hpp>
#include <ql/termstructures/volatility/capfloor/capfloortermvolsurface.hpp>
#include <ql/indexes/iborindex.hpp>
#include <qlo/valueobjects/vo_capletvolstructure.hpp>
#include <qlo/loop/loop_capletvolstructure.hpp>
#include <loop.hpp>
//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlConstantOptionletVolatility(
        const STRING &ObjectId,
        const ANY &NDays,
        const STRING &Calendar,
        const STRING &BusinessDayConvention,
        const STRING &Volatility,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        long NDaysCpp;
        calcToScalar(NDaysCpp, NDays);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string BusinessDayConventionCpp = ouStringToStlString(BusinessDayConvention);

        std::string VolatilityCpp = ouStringToStlString(Volatility);

        std::string DayCounterCpp;
        calcToScalar(DayCounterCpp, DayCounter);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT(VolatilityCoerce, VolatilityCpp, ObjectHandler::Object)
        QuantLib::Handle<QuantLib::Quote> VolatilityLibObj =
            QuantLibAddin::CoerceHandle<
                QuantLibAddin::Quote,
                QuantLib::Quote>()(
                    VolatilityCoerce);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention BusinessDayConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(BusinessDayConventionCpp);

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlConstantOptionletVolatility(
                ObjectIdCpp,
                NDaysCpp,
                CalendarCpp,
                BusinessDayConventionCpp,
                VolatilityCpp,
                DayCounterCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::ConstantOptionletVolatility(
                valueObject,
                NDaysCpp,
                CalendarEnum,
                BusinessDayConventionEnum,
                VolatilityLibObj,
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
        OH_LOG_MESSAGE("ERROR: qlConstantOptionletVolatility: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlRelinkableHandleOptionletVolatilityStructure(
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
            new QuantLibAddin::ValueObjects::qlRelinkableHandleOptionletVolatilityStructure(
                ObjectIdCpp,
                CurrentLinkCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::RelinkableHandleImpl<QuantLibAddin::OptionletVolatilityStructure, QuantLib::OptionletVolatilityStructure>(
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
        OH_LOG_MESSAGE("ERROR: qlRelinkableHandleOptionletVolatilityStructure: " << e.what());
        THROW_RTE;
    }
}


