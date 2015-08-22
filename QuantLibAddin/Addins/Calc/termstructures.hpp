
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_termstructures_hpp
#define ql_calc_termstructures_hpp

    SEQSEQ(ANY) SAL_CALL qlDiscountCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &CurveDiscounts,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFlatForward(
        const ANY &ObjectId,
        const ANY &NDays,
        const ANY &Calendar,
        const ANY &Rate,
        const ANY &DayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlForwardCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &ForwardYields,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlForwardSpreadedTermStructure(
        const ANY &ObjectId,
        const ANY &BaseYieldCurve,
        const ANY &Spread,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlImpliedTermStructure(
        const ANY &ObjectId,
        const ANY &BaseYieldCurve,
        const ANY &ReferenceDate,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlInterpolatedYieldCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const SEQSEQ(ANY) &Data,
        const ANY &Calendar,
        const ANY &DayCounter,
        const SEQSEQ(ANY) &Jumps,
        const SEQSEQ(ANY) &JumpDates,
        const ANY &TraitsID,
        const ANY &InterpolatorID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRelinkableHandleYieldTermStructure(
        const ANY &ObjectId,
        const ANY &CurrentLink,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlTermStructureCalendar(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlTermStructureMaxDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlTermStructureReferenceDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlTermStructureSettlementDays(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlYieldTSDiscount(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &DfDates,
        const sal_Int32 AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlYieldTSZeroRate(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const ANY &ResultDayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const sal_Int32 AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlZeroCurve(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(ANY) &CurveYields,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);



#endif

