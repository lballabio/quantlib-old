
/*  
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2007 Eric Ehlers
 
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

#ifndef ql_calc_pricingengines_hpp
#define ql_calc_pricingengines_hpp

    STRING SAL_CALL qlAnalyticCapFloorEngine(
        const STRING &ObjectId,
        const STRING &HandleModel,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlBinomialPricingEngine(
        const STRING &ObjectId,
        const STRING &EngineID,
        const STRING &ProcessID,
        sal_Int32 TimeSteps,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlBlackCapFloorEngine(
        const STRING &ObjectId,
        const STRING &YieldCurve,
        const STRING &VolTS,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlBlackSwaptionEngine(
        const STRING &ObjectId,
        const STRING &YieldCurve,
        const STRING &VolTS,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlBlackSwaptionEngine2(
        const STRING &ObjectId,
        const STRING &YieldCurve,
        const STRING &Vol,
        const ANY &DayCounter,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlBondEngine(
        const STRING &ObjectId,
        const STRING &YieldCurve,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlDiscountingSwapEngine(
        const STRING &ObjectId,
        const STRING &YieldCurve,
        const ANY &IncludeSettlDate,
        const ANY &SettlementDate,
        const ANY &NpvDate,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlPricingEngine(
        const STRING &ObjectId,
        const STRING &EngineID,
        const STRING &ProcessID,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);



#endif

