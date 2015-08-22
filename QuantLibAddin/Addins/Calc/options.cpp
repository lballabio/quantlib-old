
/*  
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 
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
#include <qlo/options.hpp>
#include <qlo/payoffs.hpp>
#include <qlo/exercise.hpp>
#include <ql/instruments/oneassetoption.hpp>
#include <qlo/valueobjects/vo_options.hpp>

#include <qladdin.hpp>
#include <conversions.hpp>

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlDividendVanillaOption(
        const ANY &ObjectId,
        const ANY &Payoff,
        const ANY &Exercise,
        const SEQSEQ(ANY) &DividendDates,
        const SEQSEQ(ANY) &Dividends,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PayoffCpp;
        calcToScalar(PayoffCpp, Payoff);

        std::string ExerciseCpp;
        calcToScalar(ExerciseCpp, Exercise);

        std::vector<ObjectHandler::property_t> DividendDatesCpp;
        calcToVector(DividendDatesCpp, DividendDates);

        std::vector<double> DividendsCpp;
        calcToVector(DividendsCpp, Dividends);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert input datatypes to QuantLib datatypes

        std::vector<QuantLib::Date> DividendDatesLib;
        calcToVector(DividendDatesLib, DividendDates);

        std::vector<QuantLib::Real> DividendsLib;
        calcToVector(DividendsLib, Dividends);

        // convert object IDs into library objects

        OH_GET_REFERENCE(PayoffLibObjPtr, PayoffCpp,
            QuantLibAddin::StrikedTypePayoff, QuantLib::StrikedTypePayoff)

        OH_GET_REFERENCE(ExerciseLibObjPtr, ExerciseCpp,
            QuantLibAddin::Exercise, QuantLib::Exercise)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlDividendVanillaOption(
                ObjectIdCpp,
                PayoffCpp,
                ExerciseCpp,
                DividendDatesCpp,
                DividendsCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::DividendVanillaOption(
                valueObject,
                PayoffLibObjPtr,
                ExerciseLibObjPtr,
                DividendDatesLib,
                DividendsLib,
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
            errorMsg << "ERROR: qlDividendVanillaOption: " << e.what(); 
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

SEQSEQ(ANY) SAL_CALL CalcAddins_impl::qlVanillaOption(
        const ANY &ObjectId,
        const ANY &Payoff,
        const ANY &Exercise,
        const sal_Int32 Permanent,
        const ANY &Trigger,
        const sal_Int32 Overwrite) throw(RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp;
        calcToScalar(ObjectIdCpp, ObjectId);

        std::string PayoffCpp;
        calcToScalar(PayoffCpp, Payoff);

        std::string ExerciseCpp;
        calcToScalar(ExerciseCpp, Exercise);

        bool PermanentCpp;
        calcToScalar(PermanentCpp, Permanent);

        // convert object IDs into library objects

        OH_GET_REFERENCE(PayoffLibObjPtr, PayoffCpp,
            QuantLibAddin::StrikedTypePayoff, QuantLib::StrikedTypePayoff)

        OH_GET_REFERENCE(ExerciseLibObjPtr, ExerciseCpp,
            QuantLibAddin::Exercise, QuantLib::Exercise)

        // Construct the Value Object

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new QuantLibAddin::ValueObjects::qlVanillaOption(
                ObjectIdCpp,
                PayoffCpp,
                ExerciseCpp,
                PermanentCpp));

        // Construct the Object
        
        boost::shared_ptr<ObjectHandler::Object> object(
            new QuantLibAddin::VanillaOption(
                valueObject,
                PayoffLibObjPtr,
                ExerciseLibObjPtr,
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
            errorMsg << "ERROR: qlVanillaOption: " << e.what(); 
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


