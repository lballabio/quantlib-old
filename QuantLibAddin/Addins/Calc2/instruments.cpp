
/*  
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Walter Penschke
 
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
#include <qlo/baseinstruments.hpp>
#include <qlo/pricingengines.hpp>

#include <calcaddins.hpp>
#include <calcutils.hpp>
#include <conversions.hpp>

double SAL_CALL CalcAddins_impl::qlInstrumentNPV(
        const STRING &ObjectId,
        const ANY &Trigger) throw (RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObj, ObjectIdCpp,
            QuantLibAddin::Instrument, QuantLib::Instrument)

        // invoke the member function

        double returnValue = ObjectIdLibObj->NPV();

        // convert and return the return value



        return returnValue;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlInstrumentNPV: " << e.what());
        THROW_RTE;
    }
}

sal_Int32 SAL_CALL CalcAddins_impl::qlInstrumentSetPricingEngine(
        const STRING &ObjectId,
        const STRING &PricingEngine,
        const ANY &Trigger) throw (RuntimeException) {
    try {

        // convert input datatypes to C++ datatypes

        std::string ObjectIdCpp = ouStringToStlString(ObjectId);

        std::string PricingEngineCpp = ouStringToStlString(PricingEngine);

        // convert object IDs into library objects

        OH_GET_REFERENCE(ObjectIdLibObj, ObjectIdCpp,
            QuantLibAddin::Instrument, QuantLib::Instrument)

        OH_GET_REFERENCE(PricingEngineLibObj, PricingEngineCpp,
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        // invoke the member function

        ObjectIdLibObj->setPricingEngine(
                PricingEngineLibObj);

        // convert and return the return value



        return 1;

    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("ERROR: qlInstrumentSetPricingEngine: " << e.what());
        THROW_RTE;
    }
}


