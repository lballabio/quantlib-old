
/*  
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007, 2008, 2009 Ferdinando Ametrano
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
//      gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_ratehelpers_hpp
#define ql_calc_ratehelpers_hpp

    STRING SAL_CALL qlBondHelper(
        const STRING &ObjectId,
        const STRING &CleanPrice,
        const STRING &Bond,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlDatedOISRateHelper(
        const STRING &ObjectId,
        const ANY &StartDate,
        const ANY &EndDate,
        const STRING &FixedRate,
        const STRING &ONIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlDepositRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &IborIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlDepositRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &Tenor,
        sal_Int32 FixingDays,
        const STRING &Calendar,
        const STRING &Convention,
        sal_Int32 EndOfMonth,
        const STRING &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlFixedRateBondHelper(
        const STRING &ObjectId,
        const STRING &CleanPrice,
        sal_Int32 SettlementDays,
        const ANY &FaceAmount,
        const STRING &ScheduleID,
        const SEQSEQ(double) &Coupons,
        const STRING &DayCounter,
        const ANY &PaymentBDC,
        const ANY &Redemption,
        const ANY &IssueDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlFraRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &PeriodToStart,
        const STRING &IborIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlFraRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &PeriodToStart,
        sal_Int32 LengthInMonths,
        sal_Int32 FixingDays,
        const STRING &Calendar,
        const STRING &Convention,
        sal_Int32 EndOfMonth,
        const STRING &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlOISRateHelper(
        const STRING &ObjectId,
        sal_Int32 SettlDays,
        const STRING &Tenor,
        const STRING &FixedRate,
        const STRING &ONIndex,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlSwapRateHelper(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &SwapIndex,
        const STRING &Spread,
        const STRING &ForwardStart,
        const ANY &DiscountingCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlSwapRateHelper2(
        const STRING &ObjectId,
        const STRING &Rate,
        const STRING &Tenor,
        const STRING &Calendar,
        const STRING &FixedLegFrequency,
        const STRING &FixedLegConvention,
        const STRING &FixedLegDayCounter,
        const STRING &IborIndex,
        const STRING &Spread,
        const STRING &ForwardStart,
        const ANY &DiscountingCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    double SAL_CALL qlSwapRateHelperSpread(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);



#endif

