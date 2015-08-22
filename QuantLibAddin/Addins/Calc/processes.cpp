
/*  
 Copyright (C) 2004, 2005 Eric Ehlers
 
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.includes

#include <oh/utilities.hpp>
#include <oh/ohdefines.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/all.hpp>
#include <qlo/conversions/all.hpp>
#include <oh/enumerations/typefactory.hpp>
#include <qlo/processes.hpp>
#include <qlo/volatilities.hpp>
#include <qlo/valueobjects/vo_processes.hpp>

#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlGeneralizedBlackScholesProcess(
        const ANY &ObjectId,
        const ANY &BlackVolID,
        const ANY &Underlying,
        const ANY &DayCounter,
        const ANY &SettlementDate,
        const ANY &RiskFreeRate,
        const ANY &DividendYield,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string BlackVolIDCpp;
        calcToScalar(BlackVolIDCpp, BlackVolID);

        double UnderlyingCpp;
        calcToScalar(UnderlyingCpp, Underlying);

        std::string DayCounterCpp;
        if(DayCounter.hasValue()) 
            calcToScalar(DayCounterCpp, DayCounter);
        else
            DayCounterCpp = "Actual/365 (Fixed)";

        ObjectHandler::property_t SettlementDateCpp;
        calcToScalar(SettlementDateCpp, SettlementDate);

        double RiskFreeRateCpp;
        calcToScalar(RiskFreeRateCpp, RiskFreeRate);

        double DividendYieldCpp;
        calcToScalar(DividendYieldCpp, DividendYield);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        QuantLib::Date SettlementDateLib;
        calcToScalar(SettlementDateLib, SettlementDate);

        // convert object IDs into library objects

        OH_GET_REFERENCE(BlackVolIDLibObjPtr, BlackVolIDCpp,
            QuantLibAddin::BlackVolTermStructure, QuantLib::BlackVolTermStructure)

        // convert input datatypes to QuantLib enumerated datatypes

        QuantLib::DayCounter DayCounterEnum =
            ObjectHandler::Create<QuantLib::DayCounter>()(DayCounterCpp);

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlGeneralizedBlackScholesProcess(
                ObjectIdCpp,
                BlackVolIDCpp,
                UnderlyingCpp,
                DayCounterCpp,
                SettlementDateCpp,
                RiskFreeRateCpp,
                DividendYieldCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::GeneralizedBlackScholesProcess(
                valueObject,
                BlackVolIDLibObjPtr,
                UnderlyingCpp,
                DayCounterEnum,
                SettlementDateLib,
                RiskFreeRateCpp,
                DividendYieldCpp,
                PermanentCpp));

        // Store the Object in the Repository

        std::string returnValue =
            ObjectHandler::Repository::instance().storeObject(ObjectIdCpp, object, Overwrite, valueObject);

        // Convert and return the return value



        ANY returnValueCalc;
        scalarToCalc(returnValueCalc, returnValue);

        SEQSEQ(ANY) retAnyArray;
        retAnyArray.realloc(1);
        SEQ(ANY) retAnyVector(1);
        retAnyVector[0] = returnValueCalc;
        retAnyArray[0] = retAnyVector;        
        return retAnyArray;

    } catch (const std::exception &e) {
        do { 
            std::ostringstream errorMsg; 
            errorMsg << "ERROR: qlGeneralizedBlackScholesProcess: " << e.what(); 
            OH_LOG_MESSAGE(errorMsg.str());
        
            SEQSEQ(ANY) retAnyArray;
            retAnyArray.realloc(1);
            SEQ(ANY) retAnyVector(1);
            STRING s = STRFROMASCII( errorMsg.str().c_str() );    
            retAnyVector[0] = CSS::uno::makeAny( s );
            retAnyArray[0] = retAnyVector;	    
            return retAnyArray;
        } while (false);
    }
}


