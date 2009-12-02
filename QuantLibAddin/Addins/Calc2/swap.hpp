
/*  
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Katiuscia Manzoni
 
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

#ifndef ql_calc_swap_hpp
#define ql_calc_swap_hpp

    STRING SAL_CALL qlMakeCms(
        const STRING &ObjectId,
        const STRING &SwapTenor,
        const STRING &SwapIndex,
        const STRING &IborIndex,
        const ANY &IborSpread,
        const STRING &ForwardStart,
        const STRING &CmsCouponPricer,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlSwap(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &LegIDs,
        const SEQSEQ(sal_Int32) &Payer,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    double SAL_CALL qlSwapLegBPS(
        const STRING &ObjectId,
        sal_Int32 LegNumber,
        const ANY &Trigger) throw(RuntimeException);

    double SAL_CALL qlSwapLegNPV(
        const STRING &ObjectId,
        sal_Int32 LegNumber,
        const ANY &Trigger) throw(RuntimeException);



#endif

