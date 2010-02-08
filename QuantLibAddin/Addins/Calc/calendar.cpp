
/*  
 Copyright (C) 2006 Eric Ehlers
 
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
#include <ql/time/date.hpp>
#include <ql/time/calendar.hpp>
#include <qlo/loop/loop_calendar.hpp>
#include <loop.hpp>
//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

sal_Int32 SAL_CALL CalcAddins_impl::qlCalendarAddHoliday(
        const STRING &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp = ouStringToStlString(calendar);

        ObjectHandler::property_t DateCpp;
        calcToScalar(DateCpp, Date);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date DateLib;
        calcToScalar(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        calendarEnum.addHoliday(
                DateLib);

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCalendarAddHoliday: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlCalendarName(
        const STRING &calendar,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp = ouStringToStlString(calendar);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        std::string returnValue = calendarEnum.name();

        // convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCalendarName: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlCalendarRemoveHoliday(
        const STRING &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp = ouStringToStlString(calendar);

        ObjectHandler::property_t DateCpp;
        calcToScalar(DateCpp, Date);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date DateLib;
        calcToScalar(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        calendarEnum.removeHoliday(
                DateLib);

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCalendarRemoveHoliday: " << e.what());
        THROW_RTE;
    }
}


