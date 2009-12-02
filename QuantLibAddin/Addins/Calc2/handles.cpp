
/*  
 Copyright (C) 2006, 2007 Eric Ehlers
 
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

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlHandleCurrentLink(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdObjPtr, ObjectIdCpp, QuantLibAddin::Handle)

        // invoke the member function

        std::string returnValue = ObjectIdObjPtr->currentLink();

        // convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlHandleCurrentLink: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlHandleEmpty(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdObjPtr, ObjectIdCpp, QuantLibAddin::Handle)

        // invoke the member function

        bool returnValue = ObjectIdObjPtr->empty();

        // convert and return the return value



        sal_Int32 returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlHandleEmpty: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlRelinkableHandleLinkTo(
        const STRING &ObjectId,
        const ANY &CurrentLink,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string CurrentLinkCpp;
        calcToScalar(CurrentLinkCpp, CurrentLink);

        // convert object IDs into library objects

        OH_GET_OBJECT(ObjectIdObjPtr, ObjectIdCpp, QuantLibAddin::RelinkableHandle)

        // invoke the member function

        ObjectIdObjPtr->linkTo(
                CurrentLinkCpp);

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlRelinkableHandleLinkTo: " << e.what());
        THROW_RTE;
    }
}


