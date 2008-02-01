
/*
 Copyright (C) 2007 Luigi Ballabio
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
#include <ql/qldefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#include <ql/auto_link.hpp>
#endif
#include <iostream>

#define OH_NULL ObjectHandler::Variant()

using namespace QuantLibAddinCpp;

#define LENGTH(a) (sizeof(a)/sizeof(a[0]))

struct Datum {
    const char* tenor;
    double rate;
    const char* period;
    const char* convention;
    long fixingdays;
};

int main() {

    try {

        // Initialize the environment

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L, OH_NULL);
        ohSetConsole(1, 4L, OH_NULL);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion(OH_NULL));
        LOG_MESSAGE("ObjectHandler version = " << ohVersion(OH_NULL));

        // Set the evaluation date to 5 December 2007

        qlSettingsSetEvaluationDate(39421L, OH_NULL);

        std::vector<std::string> marketObjects;
        std::vector<std::string> tradeObjects;

        // Create the market data objects

        std::string EuriborYC3M =
            qlRelinkableHandleYieldTermStructure("EuriborYC3M", "", false, OH_NULL, false);
        marketObjects.push_back(EuriborYC3M);

        std::string EuriborYC6M =
            qlRelinkableHandleYieldTermStructure("EuriborYC6M", "", false, OH_NULL, false);
        marketObjects.push_back(EuriborYC6M);

        char *tenors[] = {
            "1W",
            "2W",
            "3W",
            "1M",
            "2M",
            "3M",
            "4M",
            "5M",
            "6M",
            "7M",
            "8M",
            "9M",
            "10M",
            "11M",
            "1Y"
        };

        for (unsigned int i=0; i<LENGTH(tenors); ++i) {
            std::ostringstream objectId;
            objectId << "Euribor" << tenors[i];
            std::string quoteId = qlEuribor(objectId.str(),
                                            tenors[i],
                                            EuriborYC3M,
                                            false, OH_NULL, false);
            marketObjects.push_back(quoteId);
        }

        std::string ts_handle =
            qlRelinkableHandleYieldTermStructure("ts_handle", "", false, OH_NULL, false);
        marketObjects.push_back(ts_handle);

        std::string RH_EURIBOR6M =
            qlIborIndex("RH_EURIBOR6M",
                "EURIBOR",
                "6M",
                2,
                "EUR",
                "Target",
                "Modified Following",
                1,
                "Actual/360",
                ts_handle,
                false,
                OH_NULL,
                false);
        marketObjects.push_back(RH_EURIBOR6M);

        std::string HW_volatilityQuote = qlSimpleQuote("HW_volatilityQuote",
            0.0055895659999999998, 0.0, false, OH_NULL, false);
        marketObjects.push_back(HW_volatilityQuote);

        std::string HW_meanReversionQuote = qlSimpleQuote("HW_meanReversionQuote",
            0.029999999999999999, 0.0, false, OH_NULL, false);
        marketObjects.push_back(HW_meanReversionQuote);

        Datum depositData[] = {
            { "ON", 0.0373, "1D", "Following", 0 },
            { "TN", 0.0373, "1D", "Following", 1 },
            { "SN", 0.0373, "1D", "Following", 2 },
            { "SW", 0.037900000000000003, "1W", "Following", 2 },
            { "2W", 0.039399999999999998, "2W", "Following", 2 },
            { "3W", 0.040099999999999997, "3W", "Following", 2 },
            { "1M", 0.040500000000000001, "1M", "Modified Following", 2 },
            { "2M", 0.040899999999999999, "2M", "Modified Following", 2 },
            { "3M", 0.041100000000000005, "3M", "Modified Following", 2 }
        };

        Datum futureData[] = {
            { "U7", 95.837500000000006, "", "", 0 },
            { "V7", 95.775000000000006, "", "", 0 },
            { "X7", 95.760000000000005, "", "", 0 },
            { "Z7", 95.632499999999993, "", "", 0 },
            { "H8", 95.442499999999995, "", "", 0 },
            { "M8", 95.352500000000006, "", "", 0 },
            { "U8", 95.327500000000001, "", "", 0 },
            { "Z8", 95.342500000000001, "", "", 0 },
            { "H9", 95.372500000000002, "", "", 0 },
            { "M9", 95.412499999999994, "", "", 0 }
        };

        Datum swapData[] = {
            { "3Y", 0.046869999999999995, "", "", 0 },
            { "4Y", 0.046899999999999997, "", "", 0 },
            { "5Y", 0.046959999999999995, "", "", 0 },
            { "6Y", 0.047019999999999999, "", "", 0 },
            { "7Y", 0.047100000000000003, "", "", 0 },
            { "8Y", 0.047219999999999998, "", "", 0 },
            { "9Y", 0.047379999999999999, "", "", 0 },
            { "10Y", 0.047550000000000002, "", "", 0 },
            { "11Y", 0.047710000000000009, "", "", 0 },
            { "12Y", 0.047860000000000007, "", "", 0 },
            { "13Y", 0.048010000000000004, "", "", 0 },
            { "14Y", 0.048140000000000002, "", "", 0 },
            { "15Y", 0.048260000000000004, "", "", 0 },
            { "16Y", 0.048349999999999997, "", "", 0 },
            { "17Y", 0.048419999999999991, "", "", 0 },
            { "18Y", 0.048460000000000003, "", "", 0 },
            { "19Y", 0.048489999999999991, "", "", 0 },
            { "20Y", 0.048509999999999998, "", "", 0 },
            { "21Y", 0.048519999999999994, "", "", 0 },
            { "22Y", 0.048519999999999994, "", "", 0 },
            { "23Y", 0.048509999999999998, "", "", 0 },
            { "24Y", 0.048489999999999991, "", "", 0 },
            { "25Y", 0.048460000000000003, "", "", 0 },
            { "26Y", 0.048430000000000001, "", "", 0 },
            { "27Y", 0.048389999999999989, "", "", 0 },
            { "28Y", 0.048339999999999994, "", "", 0 },
            { "29Y", 0.048300000000000003, "", "", 0 },
            { "30Y", 0.048249999999999994, "", "", 0 },
            { "35Y", 0.048039999999999999, "", "", 0 },
            { "40Y", 0.047829999999999998, "", "", 0 },
            { "50Y", 0.04741999999999999, "", "", 0 }
        };

        std::vector<std::string> rateHelpers;

        for (unsigned int i=0; i<LENGTH(depositData); ++i) {
            std::ostringstream objectId;
            objectId << "EUR" << depositData[i].tenor << "D_Quote";
            std::string quoteId = qlSimpleQuote(objectId.str(),
                depositData[i].rate, 0.0, false, OH_NULL, false);
            marketObjects.push_back(quoteId);
        }

        for (unsigned int i=0; i<LENGTH(futureData); ++i) {
            std::ostringstream objectId;
            objectId << "EURFUT" << futureData[i].tenor << "_Quote";
            std::string quoteId = qlSimpleQuote(objectId.str(),
                futureData[i].rate, 0.0, false, OH_NULL, false);
            marketObjects.push_back(quoteId);
        }

        for (unsigned int i=0; i<LENGTH(swapData); ++i) {
            std::ostringstream objectId;
            objectId << "EURAB6E" << swapData[i].tenor << "S_Quote";
            std::string quoteId = qlSimpleQuote(objectId.str(),
                swapData[i].rate, 0.0, false, OH_NULL, false);
            marketObjects.push_back(quoteId);
        }

        for (unsigned int i=0; i<LENGTH(futureData); ++i) {
            std::ostringstream objectId, quoteId;
            objectId << "EURFUT" << futureData[i].tenor << "ConvAdj_Quote";
            quoteId << "EURFUT" << futureData[i].tenor << "_Quote";
            std::string futuresConvAdjQuoteId = qlFuturesConvAdjustmentQuote(objectId.str(),
                "Euribor3M",
                futureData[i].tenor,
                quoteId.str(),
                HW_volatilityQuote,
                HW_meanReversionQuote,
                false, OH_NULL, false);
            marketObjects.push_back(futuresConvAdjQuoteId);
        }

        for (unsigned int i=0; i<LENGTH(depositData); ++i) {
            std::ostringstream objectId, quoteId;
            objectId << "EUR" << depositData[i].tenor << "D";
            quoteId << objectId.str() << "_Quote";
            std::string rateHelperId = qlDepositRateHelper2(
                objectId.str(),
                quoteId.str(),
                depositData[i].period,
                depositData[i].fixingdays,
                "Target",
                depositData[i].convention,
                false,
                "Actual/360",
                false, OH_NULL, false);
            marketObjects.push_back(rateHelperId);
            rateHelpers.push_back(rateHelperId);
        }

        for (unsigned int i=0; i<LENGTH(futureData); ++i) {
            std::ostringstream objectId, quoteId, convQuoteId;
            objectId << "EURFUT" << futureData[i].tenor;
            quoteId << objectId.str() << "_Quote";
            convQuoteId << objectId.str()<< "ConvAdj_Quote";
            std::string futRateHelperId = qlFuturesRateHelper2(
                objectId.str(),
                quoteId.str(),
                std::string(futureData[i].tenor),
                3L,
                "Target",
                "Modified Following",
                false,
                "Actual/360",
                convQuoteId.str(),
                false, OH_NULL, false);
            marketObjects.push_back(futRateHelperId);
            rateHelpers.push_back(futRateHelperId);
        }

        for (unsigned int i=0; i<LENGTH(swapData); ++i) {
            std::ostringstream objectId, quoteId;
            objectId << "EURAB6E" << swapData[i].tenor << "S";
            quoteId << objectId.str() << "_Quote";
            std::string swapId = qlSwapRateHelper2(
                objectId.str(),
                quoteId.str(),
                swapData[i].tenor,
                "Target",
                "Annual",
                "Unadjusted",
                "30/360 (Bond Basis)",
                "RH_EURIBOR6M",
                0L,
                "0D",
                false, OH_NULL, false);
            marketObjects.push_back(swapId);
            rateHelpers.push_back(swapId);
        }

        std::string EUR_YC = qlPiecewiseYieldCurve("EUR_YC",
            0L,
            "Target",
            rateHelpers,
            "Actual/365 (Fixed)",
            "ZeroYield",
            "Linear",
            1.0e-12,
            false, OH_NULL, false);
        marketObjects.push_back(EUR_YC);

        std::string EUR_YC_HANDLE =
            qlRelinkableHandleYieldTermStructure("EUR_YC_HANDLE", EUR_YC, false, OH_NULL, false);
        marketObjects.push_back(EUR_YC_HANDLE);

        // Create the trade objects

        std::string sched_fix = qlSchedule(
            "sched_fix",
            39423L,
            42793L,
            "1Y",
            "Target",
            "Unadjusted",
            "Modified Following",
            "Backward",
            false, OH_NULL, OH_NULL,
            false, OH_NULL, false);
        tradeObjects.push_back(sched_fix);

        std::string sched_flt = qlSchedule(
            "sched_flt",
            39423L,
            42793L,
            "6M",
            "Target",
            "Modified Following",
            "Modified Following",
            "Backward",
            false, OH_NULL, OH_NULL,
            false, OH_NULL, false);
        tradeObjects.push_back(sched_flt);

        std::string engine = qlDiscountingSwapEngine(
                "engine",
                EUR_YC,
                false, OH_NULL, false);
        tradeObjects.push_back(engine);

        std::string swap = qlVanillaSwap(
                "swap",
                "Payer",
                1000000.,
                "sched_fix",
                0.042490062211088024,
                "30/360 (Bond Basis)",
                "sched_flt",
                "EURIBOR6M",
                0.,
                "Actual/360",
                false, OH_NULL, false);
        tradeObjects.push_back(swap);

        // Serialize the objects

        ohObjectSave(marketObjects, "qlxl_swap_market.xml", true, OH_NULL);
        ohObjectSave(tradeObjects, "qlxl_swap_trade.xml", true, OH_NULL);

        // Enable extrapolation for the yield curve

        qlExtrapolatorEnableExtrapolation(EUR_YC, true, OH_NULL);

        // Attach relinkable handles to term structures

        qlRelinkableHandleLinkTo(EuriborYC3M, EUR_YC, OH_NULL);
        qlRelinkableHandleLinkTo(EuriborYC6M, EUR_YC, OH_NULL);

        // Set the pricing engine

        qlInstrumentSetPricingEngine(swap, engine, OH_NULL);

        // Output the PV of the deal

        LOG_MESSAGE("SWAP PV = " << qlInstrumentNPV(swap, OH_NULL));

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

