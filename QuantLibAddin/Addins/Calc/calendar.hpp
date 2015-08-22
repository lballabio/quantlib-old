
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_calendar_hpp
#define ql_calc_calendar_hpp

    SEQSEQ(ANY) SAL_CALL qlCalendarAddHoliday(
        const ANY &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarAdjust(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &BusinessDayConvention,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarBusinessDaysBetween(
        const ANY &calendar,
        const SEQSEQ(ANY) &FirstDate,
        const ANY &LastDate,
        const sal_Int32 IncludeFirst,
        const sal_Int32 IncludeLast,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarEndOfMonth(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarHolidayList(
        const ANY &Calendar,
        const ANY &FromDate,
        const ANY &ToDate,
        const sal_Int32 IncludeWeekEnds,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarIsBusinessDay(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarIsEndOfMonth(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarIsHoliday(
        const ANY &calendar,
        const SEQSEQ(ANY) &Date,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarName(
        const ANY &calendar,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlCalendarRemoveHoliday(
        const ANY &calendar,
        const ANY &Date,
        const ANY &Trigger) throw(RuntimeException);



#endif

