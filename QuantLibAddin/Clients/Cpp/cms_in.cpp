
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
#include <Clients/Cpp/set_fixings.hpp>
#include <ql/qldefines.hpp>
#if defined BOOST_MSVC
#include <oh/auto_link.hpp>
#include <ql/auto_link.hpp>
#endif

#define OH_NULL ObjectHandler::Variant()

using namespace QuantLibAddinCpp;

int main() {

    try {

        // Initialize the environment

        initializeAddin();

        ohSetLogFile("qlademo.log", 4L);
        ohSetConsole(1, 4L);
        LOG_MESSAGE("Begin example program.");
        LOG_MESSAGE("QuantLibAddin version = " << qlAddinVersion());
        LOG_MESSAGE("ObjectHandler version = " << ohVersion());

        // Set the evaluation date to 12 July 2007

        qlSettingsSetEvaluationDate(39275L);

        // Initialize some values

        std::string yieldCurveID = "EUR_YC";

        // Deserialize the objects

        ohObjectLoad2("qlxl_demo_market.xml");
        ohObjectLoad2("qlxl_demo_cms.xml");

        // Enable extrapolation for the yield curve

        qlExtrapolatorEnableExtrapolation(yieldCurveID, true);

        // Check that the yield curve bootstraps OK

        std::vector<ObjectHandler::Variant> referenceDates;
        referenceDates.push_back(qlTermStructureReferenceDate(yieldCurveID));
        std::vector<double> refDiscounts = qlYieldTSDiscount(yieldCurveID, referenceDates, OH_NULL);
        LOG_MESSAGE("yts reference discount = " << refDiscounts[0]);

        std::vector<ObjectHandler::Variant> maxDates;
        maxDates.push_back(qlTermStructureMaxDate(yieldCurveID));
        std::vector<double> maxDiscounts = qlYieldTSDiscount(yieldCurveID, maxDates, OH_NULL);
        LOG_MESSAGE("yts max discount       = " << maxDiscounts[0]);

        // Set the index fixings and verify a couple of them

        setFixings();

        std::vector<ObjectHandler::Variant> fixingDates1;
        fixingDates1.push_back(39220L);     // 18-May-2007
        std::vector<double> fixing1 = qlIndexFixing("EuriborSwapFIXA5Y", fixingDates1, OH_NULL);
        LOG_MESSAGE("EuriborSwapFIXA5Y 18-May-2007 " << fixing1[0]);

        std::vector<ObjectHandler::Variant> fixingDates2;
        fixingDates1.push_back(38853L);     // 16-May-2006
        std::vector<double> fixing2 = qlIndexFixing("EURIBOR6M", fixingDates2, OH_NULL);
        LOG_MESSAGE("EURIBOR6M         16-May-2006 " << fixing2[0]);

        // Set the indexes to reference the yield curve

        qlSetEuriborTermStructure("EUR_YC_HANDLE");

        // Set the pricers for the legs of the deal

        qlLegSetCouponPricer("leg0", "cms_pricer");
        qlLegSetCouponPricer("leg1", "ibor_pricer");

        // Output the PV of the deal

        LOG_MESSAGE("CMS PV = " << qlInstrumentNPV("cms"));

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

