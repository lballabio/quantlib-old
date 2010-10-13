
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
//      gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_termstructures_hpp
#define ql_calc_termstructures_hpp

    STRING SAL_CALL qlDiscountCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &CurveDiscounts,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlFlatForward(
        const STRING &ObjectId,
        const ANY &NDays,
        const ANY &Calendar,
        const STRING &Rate,
        const ANY &DayCounter,
        const ANY &Compounding,
        const ANY &Frequency,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlForwardCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &ForwardYields,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlForwardSpreadedTermStructure(
        const STRING &ObjectId,
        const STRING &BaseYieldCurve,
        const STRING &Spread,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlImpliedTermStructure(
        const STRING &ObjectId,
        const STRING &BaseYieldCurve,
        const ANY &ReferenceDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlRelinkableHandleYieldTermStructure(
        const STRING &ObjectId,
        const ANY &CurrentLink,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlTermStructureCalendar(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    sal_Int32 SAL_CALL qlTermStructureMaxDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    sal_Int32 SAL_CALL qlTermStructureReferenceDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    sal_Int32 SAL_CALL qlTermStructureSettlementDays(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    double SAL_CALL qlYieldTSParRate(
        const STRING &ObjectId,
        sal_Int32 Tenor,
        const ANY &StartDate,
        const STRING &ResultDayCounter,
        const ANY &Frequency,
        const ANY &AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException);

    double SAL_CALL qlYieldTSParRate2(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &Dates,
        const STRING &ResultDayCounter,
        const ANY &Frequency,
        const ANY &AllowExtrapolation,
        const ANY &Trigger) throw(RuntimeException);

    STRING SAL_CALL qlZeroCurve(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &CurveDates,
        const SEQSEQ(double) &CurveYields,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);



#endif

