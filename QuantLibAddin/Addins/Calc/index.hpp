
/*  
 Copyright (C) 2006, 2007, 2008, 2009, 2010 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 
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

#ifndef ql_calc_index_hpp
#define ql_calc_index_hpp

    SEQSEQ(ANY) SAL_CALL qlBMAIndex(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlEonia(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlEuribor(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlEuribor365(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlEuriborSwap(
        const ANY &ObjectId,
        const ANY &FixingType,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlEuriborSwapIsdaFixA(
        const ANY &ObjectId,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlIborIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &BDayConvention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const ANY &FwdCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlIndexAddFixings(
        const ANY &ObjectId,
        const SEQSEQ(ANY) &FixingDates,
        const SEQSEQ(ANY) &FixingValues,
        const sal_Int32 ForceOverwrite,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlIndexClearFixings(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlIndexName(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlLibor(
        const ANY &ObjectId,
        const ANY &Currency,
        const ANY &Tenor,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlLiborSwap(
        const ANY &ObjectId,
        const ANY &Currency,
        const ANY &FixingType,
        const ANY &Tenor,
        const ANY &FwdCurve,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlOvernightIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &DayCounter,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSonia(
        const ANY &ObjectId,
        const ANY &YieldCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwapIndex(
        const ANY &ObjectId,
        const ANY &FamilyName,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Currency,
        const ANY &Calendar,
        const ANY &FixedLegTenor,
        const ANY &FixedLegBDC,
        const ANY &FixedLegDayCounter,
        const ANY &IborIndex,
        const ANY &DiscCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);



#endif

