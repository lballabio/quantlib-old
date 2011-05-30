
/*  
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 
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
#include <oh/enumerations/typefactory.hpp>
#include <qlo/indexes/iborindex.hpp>
#include <qlo/capfloor.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/pricingengines.hpp>
#include <qlo/termstructures.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/indexes/iborindex.hpp>
#include <qlo/valueobjects/vo_capfloor.hpp>

//#include <Addins/Calc/qladdin.hpp>
//#include <Addins/Calc/calcutils.hpp>
//#include <Addins/Calc/conversions.hpp>
#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

STRING SAL_CALL CalcAddins_impl::qlCapFloor(
        const STRING &ObjectId,
        const STRING &OptionType,
        const STRING &LegID,
        const SEQSEQ(double) &Strikes,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string OptionTypeCpp = ouStringToStlString(OptionType);

        std::string LegIDCpp = ouStringToStlString(LegID);

        std::vector<double> StrikesCpp;
        calcToVector(StrikesCpp, Strikes);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_OBJECT(LegIDObjPtr, LegIDCpp, QuantLibAddin::Leg)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::CapFloor::Type OptionTypeEnum =
            ObjectHandler::Create<QuantLib::CapFloor::Type>()(OptionTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlCapFloor(
                ObjectIdCpp,
                OptionTypeCpp,
                LegIDCpp,
                StrikesCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::CapFloor(
                valueObject,
                OptionTypeEnum,
                LegIDObjPtr,
                StrikesCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCapFloor: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlCapFloorMaturityDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::CapFloor, QuantLib::CapFloor)

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->maturityDate();

        // convert and return the return value



        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCapFloorMaturityDate: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlCapFloorStartDate(
        const STRING &ObjectId,
        const ANY &Trigger) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObjPtr, ObjectIdCpp,
            QuantLibAddin::CapFloor, QuantLib::CapFloor)

        // invoke the member function

        QuantLib::Date returnValue = ObjectIdLibObjPtr->startDate();

        // convert and return the return value



        long returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlCapFloorStartDate: " << e.what());
        THROW_RTE;
    }
}

STRING SAL_CALL CalcAddins_impl::qlMakeCapFloor(
        const STRING &ObjectId,
        const STRING &OptionType,
        const STRING &Length,
        const STRING &IborIndex,
        const ANY &Strike,
        const STRING &ForwardStart,
        const STRING &PricingEngineID,
        const ANY &Permanent,
        const ANY &Trigger,
        sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string OptionTypeCpp = ouStringToStlString(OptionType);

        std::string LengthCpp = ouStringToStlString(Length);

        std::string IborIndexCpp = ouStringToStlString(IborIndex);

        double StrikeCpp;
        calcToScalar(StrikeCpp, Strike);

        std::string ForwardStartCpp = ouStringToStlString(ForwardStart);

        std::string PricingEngineIDCpp = ouStringToStlString(PricingEngineID);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Period LengthLib;
        calcToScalar(LengthLib, Length);

        QuantLib::Period ForwardStartLib;
        calcToScalar(ForwardStartLib, ForwardStart);

        // convert object IDs into library objects

        OH_GET_REFERENCE(IborIndexLibObjPtr, IborIndexCpp,
            QuantLibAddin::IborIndex, QuantLib::IborIndex)

        OH_GET_REFERENCE(PricingEngineIDLibObjPtr, PricingEngineIDCpp,
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::CapFloor::Type OptionTypeEnum =
            ObjectHandler::Create<QuantLib::CapFloor::Type>()(OptionTypeCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlMakeCapFloor(
                ObjectIdCpp,
                OptionTypeCpp,
                LengthCpp,
                IborIndexCpp,
                StrikeCpp,
                ForwardStartCpp,
                PricingEngineIDCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::CapFloor(
                valueObject,
                OptionTypeEnum,
                LengthLib,
                IborIndexLibObjPtr,
                StrikeCpp,
                ForwardStartLib,
                PricingEngineIDLibObjPtr,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite);

        // Convert and return the return value



        STRING returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);
        return returnValueCalc;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlMakeCapFloor: " << e.what());
        THROW_RTE;
    }
}


