
/*  
 Copyright (C) 2006, 2007, 2011, 2015 Ferdinando Ametrano
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_schedule_hpp
#define ql_calc_schedule_hpp

    SEQSEQ(ANY) SAL_CALL qlSchedule(
        const ANY &ObjectId,
        const ANY &EffectiveDate,
        const ANY &TerminationDate,
        const ANY &Tenor,
        const ANY &Calendar,
        const ANY &Convention,
        const ANY &TermDateConv,
        const ANY &GenRule,
        const sal_Int32 EndOfMonth,
        const ANY &FirstDate,
        const ANY &NextToLastDate,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleBDC(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleCalendar(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleDates(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleEmpty(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleEndDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleEndOfMonth(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleFromDateVector(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &EffectiveDate,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleIsRegular(
        const ANY &ObjectId,
        const ANY &Index,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleNextDate(
        const ANY &ObjectId,
        const ANY &RefDate,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSchedulePreviousDate(
        const ANY &ObjectId,
        const ANY &RefDate,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleRule(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleSize(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleStartDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleTenor(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleTerminationDateBDC(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlScheduleTruncated(
        const ANY &ObjectId,
        const ANY &OriginalSchedule,
        const ANY &TruncationDate,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);



#endif

