
/*  
 Copyright (C) 2004, 2005 Ferdinando Ametrano
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 Copyright (C) 2005, 2006 Plamen Neykov
 Copyright (C) 2004 StatPro Italia srl
 
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
#include <oh/utilities.hpp>

#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::ohSetLogFile(
        const STRING &LogFileName,
        const ANY &LogLevel,
        const ANY &Trigger) throw (RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string LogFileNameCpp = ouStringToStlString(LogFileName);

        long LogLevelCpp;
        calcToScalar(LogLevelCpp, LogLevel);

        // invoke the utility function

	// RL: setLogFile -> logSetFile 
        std::string returnValue = ObjectHandler::logSetFile(
                LogFileNameCpp,
                LogLevelCpp);

        // convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: ohSetLogFile: " << e.what());
        THROW_RTE;
    }
}


