
/*
 Copyright (C) 2007 Eric Ehlers

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

#include <Addins/Cpp/addincpp.hpp>
//#include <Clients/Cpp/set_fixings.hpp>
#include <ql/qldefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#include <ql/auto_link.hpp>
#endif
#include <iostream>

#define OH_NULL ObjectHandler::Variant()

using namespace QuantLibAddinCpp;

int main() {

    try {

        // Initialize the environment

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L, OH_NULL);
        ohSetConsole(1, 4L, OH_NULL);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion(OH_NULL));
        LOG_MESSAGE("ObjectHandler version = " << ohVersion(OH_NULL));

        // Set the evaluation date to 12 July 2007

        qlSettingsSetEvaluationDate(39275L, OH_NULL);

        // Initialize some values

        std::string yieldCurveID = "EUR_YC";

        // Deserialize the objects

        ohObjectLoad(std::string("."), std::string("qlxl_demo_market.xml"), false, OH_NULL, OH_NULL);
        ohObjectLoad(std::string("."), std::string("qlxl_demo_swap.xml"), false, OH_NULL, OH_NULL);

        // Enable extrapolation for the yield curve

        qlExtrapolatorEnableExtrapolation(yieldCurveID, true, OH_NULL);

        // Attach relinkable handles to term structures

        qlRelinkableHandleLinkTo("EuriborYC3M", yieldCurveID, OH_NULL);
        qlRelinkableHandleLinkTo("EuriborYC6M", yieldCurveID, OH_NULL);

        // Check that the yield curve bootstraps OK

        //std::vector<ObjectHandler::Variant> referenceDates;
        //referenceDates.push_back(qlTermStructureReferenceDate(yieldCurveID));
        //std::vector<double> refDiscounts = qlYieldTSDiscount(yieldCurveID, referenceDates, OH_NULL);
        //LOG_MESSAGE("yts reference discount = " << refDiscounts[0]);

        //std::vector<ObjectHandler::Variant> maxDates;
        //maxDates.push_back(qlTermStructureMaxDate(yieldCurveID));
        //std::vector<double> maxDiscounts = qlYieldTSDiscount(yieldCurveID, maxDates, OH_NULL);
        //LOG_MESSAGE("yts max discount       = " << maxDiscounts[0]);

        // Set the index fixings and verify a couple of them

        //setFixings();

        //std::vector<ObjectHandler::Variant> fixingDates1;
        //fixingDates1.push_back(39220L);     // 18-May-2007
        //std::vector<double> fixing1 = qlIndexFixing("EuriborSwapFIXA5Y", fixingDates1, OH_NULL);
        //LOG_MESSAGE("EuriborSwapFIXA5Y 18-May-2007 " << fixing1[0]);

        //std::vector<ObjectHandler::Variant> fixingDates2;
        //fixingDates1.push_back(38853L);     // 16-May-2006
        //std::vector<double> fixing2 = qlIndexFixing("EURIBOR6M", fixingDates2, OH_NULL);
        //LOG_MESSAGE("EURIBOR6M         16-May-2006 " << fixing2[0]);

        // Set the pricing engine

        qlInstrumentSetPricingEngine("swap", "engine", OH_NULL);

        // Output the PV of the deal

        LOG_MESSAGE("SWAP PV = " << qlInstrumentNPV("swap", OH_NULL));

        // Example of serializing to/from a buffer

        LOG_MESSAGE("Example of serializing to/from a buffer:");

        qlSimpleQuote("quote1", 1.23, 0, false, OH_NULL, false);
        std::vector<std::string> idList;
        idList.push_back("quote1");
        std::string xml = ohObjectSaveString(idList, OH_NULL, OH_NULL);
        LOG_MESSAGE("XML = " << std::endl << xml);

        std::vector<std::string> idList2 = ohObjectLoadString(xml, true, OH_NULL);
        ohLogObject(idList2[0], OH_NULL);

        LOG_MESSAGE("End example program.");

        return 0;

    } catch (const std::exception &e) {
        LOG_ERROR("Error: " << e.what());
        return 1;
    } catch (...) {
        LOG_ERROR("Unknown error");
        return 1;
    }

}

