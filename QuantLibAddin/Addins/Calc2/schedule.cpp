
/*  
 Copyright (C) 2006, 2007 Ferdinando Ametrano
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
#include <ql/time/schedule.hpp>
#include <qlo/schedule.hpp>
#include <qlo/valueobjects/vo_schedule.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlSchedule(
        const STRING &ObjectId,
        const ANY &EffectiveDate,
        const ANY &TerminationDate,
        const STRING &Tenor,
        const STRING &Calendar,
        const STRING &Convention,
        const STRING &TermDateConv,
        const STRING &GenRule,
        const ANY &EndOfMonth,
        const ANY &FirstDate,
        const ANY &NextToLastDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        ObjectHandler::property_t EffectiveDateCpp;
        calcToScalar(EffectiveDateCpp, EffectiveDate);

        ObjectHandler::property_t TerminationDateCpp;
        calcToScalar(TerminationDateCpp, TerminationDate);

        std::string TenorCpp = ouStringToStlString(Tenor);

        std::string CalendarCpp = ouStringToStlString(Calendar);

        std::string ConventionCpp = ouStringToStlString(Convention);

        std::string TermDateConvCpp = ouStringToStlString(TermDateConv);

        std::string GenRuleCpp = ouStringToStlString(GenRule);

        bool EndOfMonthCpp;
        calcToScalar(EndOfMonthCpp, EndOfMonth);

        long FirstDateCpp;
        calcToScalar(FirstDateCpp, FirstDate);

        long NextToLastDateCpp;
        calcToScalar(NextToLastDateCpp, NextToLastDate);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date EffectiveDateLib;
        calcToScalar(EffectiveDateLib, EffectiveDate);

        QuantLib::Date TerminationDateLib;
        calcToScalar(TerminationDateLib, TerminationDate);

        QuantLib::Period TenorLib;
        calcToScalar(TenorLib, Tenor);

        QuantLib::Date FirstDateLib;
        calcToScalar(FirstDateLib, FirstDate);

        QuantLib::Date NextToLastDateLib;
        calcToScalar(NextToLastDateLib, NextToLastDate);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        QuantLib::BusinessDayConvention ConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(ConventionCpp);

        QuantLib::BusinessDayConvention TermDateConvEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(TermDateConvCpp);

        QuantLib::DateGeneration::Rule GenRuleEnum =
            ObjectHandler::Create<QuantLib::DateGeneration::Rule>()(GenRuleCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSchedule(
                ObjectIdCpp,
                EffectiveDateCpp,
                TerminationDateCpp,
                TenorCpp,
                CalendarCpp,
                ConventionCpp,
                TermDateConvCpp,
                GenRuleCpp,
                EndOfMonthCpp,
                FirstDateCpp,
                NextToLastDateCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Schedule(
                valueObject,
                EffectiveDateLib,
                TerminationDateLib,
                TenorLib,
                CalendarEnum,
                ConventionEnum,
                TermDateConvEnum,
                GenRuleEnum,
                EndOfMonthCpp,
                FirstDateLib,
                NextToLastDateLib,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSchedule: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlScheduleSize(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Schedule, QuantLib::Schedule)

        // invoke the member function

        QuantLib::Size returnValue = ObjectIdLibObjPtr->size();

        // convert and return the return value



        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlScheduleSize: " << e.what());
        THROW_RTE;
    }
}


