
/*  
 Copyright (C) 2006, 2007, 2008, 2014 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco
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

#ifndef ql_calc_swaption_hpp
#define ql_calc_swaption_hpp

    SEQSEQ(ANY) SAL_CALL qlMakeSwaption(
        const ANY &ObjectId,
        const ANY &SwapIndex,
        const ANY &OptionTenor,
        const ANY &Strike,
        const ANY &PricingEngineID,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwaption(
        const ANY &ObjectId,
        const ANY &VanillaSwap,
        const ANY &Exercise,
        const ANY &SettlementType,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwaptionImpliedVolatility(
        const ANY &ObjectId,
        const ANY &Price,
        const ANY &YieldCurve,
        const ANY &Guess,
        const ANY &Accuracy,
        const ANY &MaxIter,
        const ANY &MinVol,
        const ANY &MaxVol,
        const ANY &Displacement,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwaptionSettlementType(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    SEQSEQ(ANY) SAL_CALL qlSwaptionType(
        const ANY &ObjectId,
        const ANY &Trigger) throw(RuntimeException);



#endif

