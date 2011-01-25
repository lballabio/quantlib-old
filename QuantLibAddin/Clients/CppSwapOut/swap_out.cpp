
/*
 Copyright (C) 2007 Luigi Ballabio
 Copyright (C) 2007, 2011 Eric Ehlers

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
#include <ql/qldefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#include <ql/auto_link.hpp>
#endif
#include <iostream>

using namespace QuantLibAddinCpp;

#define OH_NULL ObjectHandler::property_t()

#ifdef LOG_MESSAGE
#undef LOG_MESSAGE
#endif
#ifdef LOG_ERROR
#undef LOG_ERROR
#endif

#define LOG_MESSAGE(msg) std::cout << msg << std::endl
#define LOG_ERROR(msg) std::cerr << msg << std::endl

int main() {

    try {

        // Initialize the environment

        initializeAddin();

        ohLogSetFile("qlademo.log", 4L, OH_NULL);
        ohLogSetConsole(1, 4L, OH_NULL);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion(OH_NULL));
        LOG_MESSAGE("ObjectHandler version = " << ohVersion(OH_NULL));

        // Set the evaluation date to 1 January 2011

        long evaluationDate = 40546;    // 1-Jan-2011
        qlSettingsSetEvaluationDate(evaluationDate, OH_NULL);

        // Create the market data objects

        std::vector<std::string> marketObjects;

        std::string flatForward = qlFlatForward(
            "FlatForward",
            OH_NULL,
            "NullCalendar",
            0.044,
            OH_NULL,
            "Continuous",
            "Annual",
            false, OH_NULL, false);
        marketObjects.push_back(flatForward);

        std::string swaptionVTSConstant = qlConstantSwaptionVolatility(
            "SwaptionVTSConstant",
            OH_NULL,
            "TARGET",
            "f",
            0.15,
            OH_NULL,
            false, OH_NULL, false);
        marketObjects.push_back(swaptionVTSConstant);

        std::string euribor = qlEuribor(
            "Euribor",
            "6M",
            flatForward,
            false, OH_NULL, false);
        marketObjects.push_back(euribor);

        //Calendar calendar = TARGET;
        //long effectiveDate = qlCalendarAdvance(calendar, evaluationDate, "2d");
        //long terminationDate = qlCalendarAdvance(calendar, effectiveDate, "10Y", "Unadjusted");
        long effectiveDate = 40548;     // 05-Jan-2011
        long terminationDate = 44201;   // 05-Jan-2021

        std::string schedule1 = qlSchedule(
            "Schedule1",
            effectiveDate,
            terminationDate,
            "1Y",
            "TARGET",
            "Modified Following",
            "Modified Following",
            "Backward",
            OH_NULL,
            OH_NULL,
            OH_NULL,
            false, OH_NULL, false);
        marketObjects.push_back(schedule1);

        std::string schedule2 = qlSchedule(
            "Schedule2",
            effectiveDate,
            terminationDate,
            "6M",
            "TARGET",
            "Modified Following",
            "Modified Following",
            "Backward",
            OH_NULL,
            OH_NULL,
            OH_NULL,
            false, OH_NULL, false);
        marketObjects.push_back(schedule2);

        // Create the trade objects

        std::vector<std::string> tradeObjects;

        std::string engine = qlDiscountingSwapEngine(
            "DiscountingSwapEngine",
            flatForward,
            false,
            OH_NULL,
            OH_NULL,
            false, OH_NULL, false);
        tradeObjects.push_back(engine);

        std::vector<double> emptyVector;
        std::vector<double> nominals;
        nominals.push_back(1000000);
        std::vector<long> fixingDays;

        std::string iborLeg = qlIborLeg(
            "IborLeg",
            "Following",
            nominals,
            schedule2,
            fixingDays,
            OH_NULL,
            "Actual/360",
            emptyVector,
            emptyVector,
            euribor,
            emptyVector,
            emptyVector,
            false, OH_NULL, false);
        tradeObjects.push_back(iborLeg);

        std::vector<double> coupons;
        coupons.push_back(0.05);

        std::string fixedRateLeg = qlFixedRateLeg(
            "FixedRateLeg",
            "Following",
            nominals,
            schedule1,
            coupons,
            "30/360 (Bond Basis)",
            false, OH_NULL, false);
        tradeObjects.push_back(fixedRateLeg);

        std::string vanillaSwap = qlVanillaSwap(
            "VanillaSwap",
            "Payer",
            1000000.0,
            schedule1,
            0.05,
            "30/360 (Bond Basis)",
            schedule2,
            euribor,
            OH_NULL,
            "Actual/360",
            OH_NULL,
            false, OH_NULL, false);
        tradeObjects.push_back(vanillaSwap);

        // Set the pricing engine

        qlInstrumentSetPricingEngine(vanillaSwap, engine, OH_NULL);

        // Enable extrapolation for the yield curve

        //qlExtrapolatorEnableExtrapolation(flatForward, true, OH_NULL);
        //qlExtrapolatorEnableExtrapolation(swaptionVTSConstant, true, OH_NULL);

        // Output the PV of the deal

        LOG_MESSAGE("SWAP PV = " << qlInstrumentNPV(vanillaSwap, OH_NULL));

        // Serialize the objects

        ohObjectSave(marketObjects, "MarketData.xml", true, OH_NULL, true);
        ohObjectSave(tradeObjects, "VanillaSwap.xml", true, OH_NULL, true);

        // Example of serializing to/from a buffer

        LOG_MESSAGE("Example of serializing to/from a buffer:");

        qlSimpleQuote("quote1", 1.23, 0, false, OH_NULL, false);
        std::vector<std::string> idList;
        idList.push_back("quote1");
        std::string xml = ohObjectSaveString(idList, OH_NULL, OH_NULL);
        LOG_MESSAGE("XML = " << std::endl << xml);

        std::vector<std::string> idList2 = ohObjectLoadString(xml, true, OH_NULL);
        ohRepositoryLogObject(idList2[0], OH_NULL);

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

