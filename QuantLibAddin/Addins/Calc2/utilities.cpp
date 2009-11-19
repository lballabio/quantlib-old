
/*  
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 
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

// This file was generated automatically by gensrc.py.
// Editing this file manually is not recommended.

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/Enumerations/Factories/all.hpp>
#include <qlo/Conversions/all.hpp>
#include <qlo/utilities.hpp>
#include <ql/models/marketmodels/utilities.hpp>

#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlAddinVersion(const ANY &Trigger) 
  throw (RuntimeException) {
    try {

        // invoke the utility function

        std::string returnValue = QuantLibAddin::qlAddinVersion();

        // convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlAddinVersion: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlVersion(const ANY &Trigger) 
  throw (RuntimeException) {
    try {

        // invoke the utility function

        std::string returnValue = QuantLibAddin::qlVersion();

        // convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVersion: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::ohVersion(const ANY &Trigger) 
    throw (RuntimeException) {
    try {
        // invoke the utility function
        //std::string returnValue = "fake object handler version";
        std::string returnValue = OBJHANDLER_VERSION;
        // convert and return the return value
        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlVersion: " << e.what());
        THROW_RTE;
    }
}


