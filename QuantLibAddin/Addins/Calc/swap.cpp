
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
//      gensrc/gensrc/stubs/stub.calc.includes

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/all.hpp>
#include <qlo/conversions/all.hpp>
#include <qlo/swap.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/conundrumpricer.hpp>
#include <qlo/indexes/iborindex.hpp>
#include <qlo/indexes/swapindex.hpp>
#include <ql/instruments/swap.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <qlo/valueobjects/vo_swap.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlMakeCms(
        const STRING &ObjectId,
        const STRING &SwapTenor,
        const STRING &SwapIndex,
        const STRING &IborIndex,
        const ANY &IborSpread,
        const STRING &ForwardStart,
        const STRING &CmsCouponPricer,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string SwapTenorCpp = ouStringToStlString(SwapTenor);

        std::string SwapIndexCpp = ouStringToStlString(SwapIndex);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        double IborSpreadCpp;
        calcToScalar(IborSpreadCpp, IborSpread);

        std::string ForwardStartCpp = ouStringToStlString(ForwardStart);

        std::string CmsCouponPricerCpp = ouStringToStlString(CmsCouponPricer);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period SwapTenorLib;
        calcToScalar(SwapTenorLib, SwapTenor);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_REFERENCE(SwapIndexLibObjPtr, SwapIndexCpp,
            QuantLibAddin::SwapIndex, QuantLib::SwapIndex)

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_REFERENCE(CmsCouponPricerLibObjPtr, CmsCouponPricerCpp,
            QuantLibAddin::CmsCouponPricer, QuantLib::CmsCouponPricer)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeCms(
                ObjectIdCpp,
                SwapTenorCpp,
                SwapIndexCpp,
                IborIndexCpp,
                IborSpreadCpp,
                ForwardStartCpp,
                CmsCouponPricerCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Swap(
                valueObject,
                SwapTenorLib,
                SwapIndexLibObjPtr,
                IborIndexLibObjPtr,
                IborSpreadCpp,
                ForwardStartLib,
                CmsCouponPricerLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlMakeCms: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlSwap(
        const STRING &ObjectId,
        const SEQSEQ(ANY) &LegIDs,
        const SEQSEQ(sal_Int32) &Payer,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::vector<std::string> LegIDsCpp;
        calcToVector(LegIDsCpp, LegIDs);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        std::vector<std::string> LegIDsObjPtr;
        calcToVector(LegIDsObjPtr, LegIDs);

        std::vector<bool> Payer;
        calcToVector(Payer, Payer);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlSwap(
                ObjectIdCpp,
                LegIDsCpp,
                PayerCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::Swap(
                valueObject,
                LegIDsCpp,
                PayerCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwap: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlSwapLegBPS(
        const STRING &ObjectId,
        sal_Int32 LegNumber,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Swap, QuantLib::Swap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->legBPS(
                LegNumber);

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwapLegBPS: " << e.what());
        THROW_RTE;
    }
}

double SAL_CALL CalcAddins_impl::qlSwapLegNPV(
        const STRING &ObjectId,
        sal_Int32 LegNumber,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::Swap, QuantLib::Swap)

        // invoke the member function

        double returnValue = ObjectIdLibObjPtr->legNPV(
                LegNumber);

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlSwapLegNPV: " << e.what());
        THROW_RTE;
    }
}


