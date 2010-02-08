
/*  
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2006 Francois du Vignaud
 Copyright (C) 2006 Giorgio Facchinetti
 
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

#ifndef ql_calc_quotes_hpp
#define ql_calc_quotes_hpp

    double SAL_CALL qlQuoteValue(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    STRING SAL_CALL qlRelinkableHandleQuote(
        const STRING &ObjectId,
        const ANY &CurrentLink,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    STRING SAL_CALL qlSimpleQuote(
        const STRING &ObjectId,
        const ANY &Value,
        double TickValue,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException);

    sal_Int32 SAL_CALL qlSimpleQuoteReset(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException);

    double SAL_CALL qlSimpleQuoteSetValue(
        const STRING &ObjectId,
        const ANY &Value,
        const ANY &Trigger) throw(RuntimeException);



#endif

