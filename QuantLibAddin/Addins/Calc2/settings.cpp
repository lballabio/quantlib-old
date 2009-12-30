
/*  
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 
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
//      gensrc/gensrc/stubs/stub.calc.includes

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/all.hpp>
#include <qlo/conversions/all.hpp>
#include <qlo/settings.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

sal_Int32 SAL_CALL CalcAddins_impl::qlSettingsEvaluationDate(
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // invoke the utility function

        QuantLib::Date returnValue = QuantLib::Settings::instance().evaluationDate();

        // convert and return the return value



        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSettingsEvaluationDate: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlSettingsSetEvaluationDate(
        const ANY &EvalDate,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        ObjectHandler::property_t EvalDateCpp;
        calcToScalar(EvalDateCpp, EvalDate);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date EvalDateLib;
        calcToScalar(EvalDateLib, EvalDate);

        // invoke the utility function

        QuantLibAddin::qlSettingsSetEvaluationDate(
                EvalDateLib);

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSettingsSetEvaluationDate: " << e.what());
        THROW_RTE;
    }
}


