
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

#include <Addins/C++/addincpp.hpp>
#include <Clients/C++/set_fixings.hpp>
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
        LOG_MESSAGE(qlAddinVersion());
        LOG_MESSAGE(ohVersion());

        // initialize some values

        std::string yieldCurveID = "EUR_YC";

        // Deserialize the objects

        ohObjectLoad2("qlxl_demo_market.xml");
        ohObjectLoad2("qlxl_demo_cms.xml");

        // Check that the yield curve bootstraps OK

        qlExtrapolatorEnableExtrapolation(yieldCurveID, true);
        long referenceDate = qlTermStructureReferenceDate(yieldCurveID);
        long maxDate = qlTermStructureMaxDate(yieldCurveID);
        LOG_MESSAGE("yts reference discount = " << qlYieldTSDiscount2(yieldCurveID, referenceDate, OH_NULL));
        LOG_MESSAGE("yts max discount       = " << qlYieldTSDiscount2(yieldCurveID, maxDate, OH_NULL));

        // Set the index fixings and check a couple of them

        setFixings();
        LOG_MESSAGE("EuriborSwapFIXA5Y 18-May-2007 " << qlIndexFixing2("EuriborSwapFIXA5Y", 39220L, OH_NULL));
        LOG_MESSAGE("EURIBOR6M         16-May-2006 " << qlIndexFixing2("EURIBOR6M", 38853L, OH_NULL));

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
        LOG_ERROR("unknown error");
        return 1;
    }

}

