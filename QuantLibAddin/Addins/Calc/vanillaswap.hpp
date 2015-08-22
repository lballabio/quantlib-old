
/*  
 Copyright (C) 2006, 2007, 2011, 2015 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
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

#ifndef ql_calc_vanillaswap_hpp
#define ql_calc_vanillaswap_hpp

    SEQSEQ(ANY) SAL_CALL qlMakeIMMSwap(
        const ANY &ObjectId,
        const ANY &SwapTenor,
        const ANY &IborIndex,
        const ANY &FixedRate,
        const ANY &FirstImmDate,
        const ANY &FixDayCounter,
        const ANY &Spread,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlMakeVanillaSwap(
        const ANY &ObjectId,
        const ANY &SettlDays,
        const ANY &SwapTenor,
        const ANY &IborIndex,
        const ANY &FixedRate,
        const ANY &ForwardStart,
        const ANY &FixDayCounter,
        const ANY &Spread,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwap(
        const ANY &ObjectId,
        const ANY &PayerReceiver,
        const ANY &Nominal,
        const ANY &FixSchedule,
        const ANY &FixedRate,
        const ANY &FixDayCounter,
        const ANY &FloatingLegSchedule,
        const ANY &IborIndex,
        const ANY &Spread,
        const ANY &FloatingLegDayCounter,
        const ANY &PaymentConvention,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFairRate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFairSpread(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFixedLegNPV(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFixedRate(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFloatingLegBPS(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapFloatingLegNPV(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapNominal(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlVanillaSwapSpread(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);



#endif

