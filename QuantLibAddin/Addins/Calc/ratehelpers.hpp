
/*  
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007, 2008, 2009, 2015 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2015 Maddalena Zanzi
 
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

#ifndef ql_calc_ratehelpers_hpp
#define ql_calc_ratehelpers_hpp

    SEQSEQ(ANY) SAL_CALL qlBondHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &Bond,
        const sal_Int32 UseCleanPrice,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlDatedOISRateHelper(
        const ANY &ObjectId,
        const ANY &StartDate,
        const ANY &EndDate,
        const ANY &FixedRate,
        const ANY &ONIndex,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlDepositRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &IborIndex,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlDepositRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &Tenor,
        const ANY &FixingDays,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFixedRateBondHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &SettlementDays,
        const ANY &FaceAmount,
        const ANY &ScheduleID,
        const SEQSEQ(ANY) &Coupons,
        const ANY &DayCounter,
        const ANY &PaymentBDC,
        const ANY &Redemption,
        const ANY &IssueDate,
        const ANY &PaymentCalendar,
        const ANY &ExCouponPeriod,
        const ANY &ExCouponCalendar,
        const ANY &ExCouponBDC,
        const sal_Int32 ExCouponEndOfMonth,
        const sal_Int32 UseCleanPrice,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFraRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &PeriodToStart,
        const ANY &IborIndex,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFraRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &PeriodToStart,
        const ANY &LengthInMonths,
        const ANY &FixingDays,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFuturesRateHelper(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &IborIndex,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFuturesRateHelper2(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &LengthInMonths,
        const ANY &Calendar,
        const ANY &Convention,
        const sal_Int32 EndOfMonth,
        const ANY &DayCounter,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlFuturesRateHelper3(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &FuturesType,
        const ANY &FuturesDate,
        const ANY &EndDate,
        const ANY &DayCounter,
        const ANY &ConvexityAdjQuote,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlOISRateHelper(
        const ANY &ObjectId,
        const ANY &SettlDays,
        const ANY &Tenor,
        const ANY &FixedRate,
        const ANY &ONIndex,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperEarliestDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperImpliedQuote(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperLatestDate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperQuoteIsValid(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperQuoteName(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlRateHelperQuoteValue(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwapRateHelper(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &SwapIndex,
        const ANY &Spread,
        const ANY &ForwardStart,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwapRateHelper2(
        const ANY &ObjectId,
        const ANY &Rate,
        const ANY &SettlDays,
        const ANY &Tenor,
        const ANY &Calendar,
        const ANY &FixedLegFrequency,
        const ANY &FixedLegConvention,
        const ANY &FixedLegDayCounter,
        const ANY &IborIndex,
        const ANY &Spread,
        const ANY &ForwardStart,
        const ANY &DiscountingCurve,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);



#endif

