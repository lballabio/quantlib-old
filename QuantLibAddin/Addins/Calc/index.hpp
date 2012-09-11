
/*  
 Copyright (C) 2006, 2007, 2008, 2009 Ferdinando Ametrano
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
//      gensrc/gensrc/stubs/stub.calc.category

#ifndef ql_calc_index_hpp
#define ql_calc_index_hpp

    STRING SAL_CALL qlBMAIndex(
        const STRING &ObjectId,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlEonia(
        const STRING &ObjectId,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlEuribor(
        const STRING &ObjectId,
        const STRING &Tenor,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlEuribor365(
        const STRING &ObjectId,
        const STRING &Tenor,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlIborIndex(
        const STRING &ObjectId,
        const STRING &FamilyName,
        const STRING &Tenor,
        sal_Int32 FixingDays,
        const STRING &Currency,
        const STRING &Calendar,
        const STRING &BDayConvention,
        sal_Int32 EndOfMonth,
        const STRING &DayCounter,
        const ANY &FwdCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlLibor(
        const STRING &ObjectId,
        const STRING &Currency,
        const STRING &Tenor,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlOvernightIndex(
        const STRING &ObjectId,
        const STRING &FamilyName,
        sal_Int32 FixingDays,
        const STRING &Currency,
        const STRING &Calendar,
        const STRING &DayCounter,
        const ANY &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);



#endif

