
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.includes

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
#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarAddHoliday(
        const ANY &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        ObjectHandler::property_t DateCpp;
        calcToScalar(DateCpp, Date);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date DateLib;
        calcToScalar(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        static bool returnValue = true;
        calendarEnum.addHoliday(
                DateLib);

        // convert and return the return value



        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        STRING s = STRFROMASCII( std::string("VOID").c_str() );    
        retAnyVector[0] = CSS::uno::makeAny( s );
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarAddHoliday: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarAdjust(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &BusinessDayConvention,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        std::string BusinessDayConventionCpp;
        if(BusinessDayConvention.hasValue()) 
            calcToScalar(BusinessDayConventionCpp, BusinessDayConvention);
        else
            BusinessDayConventionCpp = "Following";

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DateLib;
        calcToVector(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        QuantLib::BusinessDayConvention BusinessDayConventionEnum =
            ObjectHandler::Create<QuantLib::BusinessDayConvention>()(BusinessDayConventionCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarAdjustBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarAdjustSignature)
                    &QuantLib::Calendar::adjust, 
                calendarEnum,
                _1,
                BusinessDayConventionEnum);
                    
        {
            returnValue.realloc(DateLib.size());
            for (unsigned int i=0; i<DateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarAdjustBind, QuantLib::Date, QuantLib::Date>
            (functionCall, bindObject, DateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarAdjust: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarBusinessDaysBetween(
        const ANY &calendar,
        const SEQSEQ(ANY) &FirstDate,
        const ANY &LastDate,
        const sal_Int32 IncludeFirst,
        const sal_Int32 IncludeLast,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        ObjectHandler::property_t LastDateCpp;
        calcToScalar(LastDateCpp, LastDate);

        bool IncludeFirstCpp;
        calcToScalar(IncludeFirstCpp, IncludeFirst);

        bool IncludeLastCpp;
        calcToScalar(IncludeLastCpp, IncludeLast);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> FirstDateLib;
        calcToVector(FirstDateLib, FirstDate);

        QuantLib::Date LastDateLib;
        calcToScalar(LastDateLib, LastDate);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarBusinessDaysBetweenBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarBusinessDaysBetweenSignature)
                    &QuantLib::Calendar::businessDaysBetween, 
                calendarEnum,
                _1,
                LastDateLib,
                IncludeFirstCpp,
                IncludeLastCpp);
                    
        {
            returnValue.realloc(FirstDateLib.size());
            for (unsigned int i=0; i<FirstDateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( FirstDateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarBusinessDaysBetweenBind, QuantLib::Date, long>
            (functionCall, bindObject, FirstDateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarBusinessDaysBetween: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarEndOfMonth(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DateLib;
        calcToVector(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarEndOfMonthBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarEndOfMonthSignature)
                    &QuantLib::Calendar::endOfMonth, 
                calendarEnum,
                _1);
                    
        {
            returnValue.realloc(DateLib.size());
            for (unsigned int i=0; i<DateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarEndOfMonthBind, QuantLib::Date, QuantLib::Date>
            (functionCall, bindObject, DateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarEndOfMonth: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarHolidayList(
        const ANY &Calendar,
        const ANY &FromDate,
        const ANY &ToDate,
        const sal_Int32 IncludeWeekEnds,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string CalendarCpp;
        calcToScalar(CalendarCpp, Calendar);

        ObjectHandler::property_t FromDateCpp;
        calcToScalar(FromDateCpp, FromDate);

        ObjectHandler::property_t ToDateCpp;
        calcToScalar(ToDateCpp, ToDate);

        bool IncludeWeekEndsCpp;
        calcToScalar(IncludeWeekEndsCpp, IncludeWeekEnds);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date FromDateLib;
        calcToScalar(FromDateLib, FromDate);

        QuantLib::Date ToDateLib;
        calcToScalar(ToDateLib, ToDate);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar CalendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(CalendarCpp);

        // invoke the utility function

        std::vector<QuantLib::Date> returnValue = QuantLib::Calendar::holidayList(
                CalendarEnum,
                FromDateLib,
                ToDateLib,
                IncludeWeekEndsCpp);

        // convert and return the return value



        SEQSEQ(ANY) returnValueCalc;
        vectorToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarHolidayList: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarIsBusinessDay(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DateLib;
        calcToVector(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarIsBusinessDayBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarIsBusinessDaySignature)
                    &QuantLib::Calendar::isBusinessDay, 
                calendarEnum,
                _1);
                    
        {
            returnValue.realloc(DateLib.size());
            for (unsigned int i=0; i<DateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarIsBusinessDayBind, QuantLib::Date, bool>
            (functionCall, bindObject, DateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarIsBusinessDay: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarIsEndOfMonth(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DateLib;
        calcToVector(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarIsEndOfMonthBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarIsEndOfMonthSignature)
                    &QuantLib::Calendar::isEndOfMonth, 
                calendarEnum,
                _1);
                    
        {
            returnValue.realloc(DateLib.size());
            for (unsigned int i=0; i<DateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarIsEndOfMonthBind, QuantLib::Date, bool>
            (functionCall, bindObject, DateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarIsEndOfMonth: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarIsHoliday(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DateLib;
        calcToVector(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // loop on the input parameter and populate the return vector

        SEQSEQ(ANY) returnValue;

        QuantLibAddin::qlCalendarIsHolidayBind bindObject = 
            boost::bind((QuantLibAddin::qlCalendarIsHolidaySignature)
                    &QuantLib::Calendar::isHoliday, 
                calendarEnum,
                _1);
                    
        {
            returnValue.realloc(DateLib.size());
            for (unsigned int i=0; i<DateLib.size(); ++i) {
                SEQ(ANY) s(1);
                scalarToCalc(s[0], bindObject( DateLib[i] ) );
                returnValue[i] = s;
            }
        }
     	  
        /* ObjectHandler::loop
            <QuantLibAddin::qlCalendarIsHolidayBind, QuantLib::Date, bool>
            (functionCall, bindObject, DateLib, returnValue); */



        return returnValue;
    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarIsHoliday: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarName(
        const ANY &calendar,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        std::string returnValue = calendarEnum.name();

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
            errorMsg << "ERROR: qlCalendarName: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlCalendarRemoveHoliday(
        const ANY &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string calendarCpp;
        calcToScalar(calendarCpp, calendar);

        ObjectHandler::property_t DateCpp;
        calcToScalar(DateCpp, Date);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date DateLib;
        calcToScalar(DateLib, Date);

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::Calendar calendarEnum =
            ObjectHandler::Create<QuantLib::Calendar>()(calendarCpp);

        // invoke the member function

        static bool returnValue = true;
        calendarEnum.removeHoliday(
                DateLib);

        // convert and return the return value



        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        STRING s = STRFROMASCII( std::string("VOID").c_str() );    
        retAnyVector[0] = CSS::uno::makeAny( s );
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlCalendarRemoveHoliday: " << e.what(); 
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


