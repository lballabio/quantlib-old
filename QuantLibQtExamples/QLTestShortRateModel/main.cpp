/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007, 2009 StatPro Italia srl

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

#include "customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif

namespace {

    struct CalibrationData {
        Integer start;
        Integer length;
        Volatility volatility;
    };

}


void testCachedHullWhite() {
    LARGE_TITLE("Testing Hull-White calibration against cached values...");

    SavedSettings backup;
    IndexHistoryCleaner cleaner;

    Date today(15, February, 2002);
    Date settlement(19, February, 2002);
    Settings::instance().evaluationDate() = today;
    Handle<YieldTermStructure> termStructure(flatRate(settlement,0.04875825,
                                                      Actual365Fixed()));
    boost::shared_ptr<HullWhite> model(new HullWhite(termStructure));
    CalibrationData data[] = {{ 1, 5, 0.1148 },
                              { 2, 4, 0.1108 },
                              { 3, 3, 0.1070 },
                              { 4, 2, 0.1021 },
                              { 5, 1, 0.1000 }};
    boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

    boost::shared_ptr<PricingEngine> engine(
                                         new JamshidianSwaptionEngine(model));

    std::vector<boost::shared_ptr<CalibrationHelper> > swaptions;
    for (Size i=0; i<LENGTH(data); i++) {
        boost::shared_ptr<Quote> vol(new SimpleQuote(data[i].volatility));
        boost::shared_ptr<CalibrationHelper> helper(
                             new SwaptionHelper(Period(data[i].start, Years),
                                                Period(data[i].length, Years),
                                                Handle<Quote>(vol),
                                                index,
                                                Period(1, Years), Thirty360(),
                                                Actual360(), termStructure));
        helper->setPricingEngine(engine);
        swaptions.push_back(helper);
    }

    // Set up the optimization problem
    // Real simplexLambda = 0.1;
    // Simplex optimizationMethod(simplexLambda);
    LevenbergMarquardt optimizationMethod(1.0e-8,1.0e-8,1.0e-8);
    EndCriteria endCriteria(10000, 100, 1e-6, 1e-8, 1e-8);

    //Optimize
    model->calibrate(swaptions, optimizationMethod, endCriteria);
    EndCriteria::Type ecType = model->endCriteria();

    // Check and print out results
    #if defined(QL_USE_INDEXED_COUPON)
    Real cachedA = 0.0488199, cachedSigma = 0.00593579;
    #else
    Real cachedA = 0.0488565, cachedSigma = 0.00593662;
    #endif
    Real tolerance = 1.0e-6;

    Array xMinCalculated = model->params();
    Real yMinCalculated = model->value(xMinCalculated, swaptions);

    Array xMinExpected(2);
    xMinExpected[0]= cachedA;
    xMinExpected[1]= cachedSigma;
    Real yMinExpected = model->value(xMinExpected, swaptions);
    //if (std::fabs(xMinCalculated[0]-cachedA) > tolerance
    //    || std::fabs(xMinCalculated[1]-cachedSigma) > tolerance) {
        BOOST_ERROR("Failed to reproduce cached calibration results:\n"
                    << "calculated: a = " << xMinCalculated[0] << ", "
                    << "sigma = " << xMinCalculated[1] << ", "
                    << "f(a) = " << yMinCalculated << ",\n"
                    << "expected:   a = " << xMinExpected[0] << ", "
                    << "sigma = " << xMinExpected[1] << ", "
                    << "f(a) = " << yMinExpected << ",\n"
                    << "difference: a = " << xMinCalculated[0]-xMinExpected[0] << ", "
                    << "sigma = " << xMinCalculated[1]-xMinExpected[1] << ", "
                    << "f(a) = " << yMinCalculated - yMinExpected << ",\n"
                    << "end criteria = " << ecType );
    //}
}


void testSwaps() {
    LARGE_TITLE("Testing Hull-White swap pricing against known values...");

    SavedSettings backup;
    IndexHistoryCleaner cleaner;

    Date today = Settings::instance().evaluationDate();
    Calendar calendar = TARGET();
    today = calendar.adjust(today);
    Settings::instance().evaluationDate() = today;

    Date settlement = calendar.advance(today,2,Days);

    Date dates[] = {
        settlement,
        calendar.advance(settlement,1,Weeks),
        calendar.advance(settlement,1,Months),
        calendar.advance(settlement,3,Months),
        calendar.advance(settlement,6,Months),
        calendar.advance(settlement,9,Months),
        calendar.advance(settlement,1,Years),
        calendar.advance(settlement,2,Years),
        calendar.advance(settlement,3,Years),
        calendar.advance(settlement,5,Years),
        calendar.advance(settlement,10,Years),
        calendar.advance(settlement,15,Years)
    };
    DiscountFactor discounts[] = {
        1.0,
        0.999258,
        0.996704,
        0.990809,
        0.981798,
        0.972570,
        0.963430,
        0.929532,
        0.889267,
        0.803693,
        0.596903,
        0.433022
    };

    Handle<YieldTermStructure> termStructure(
       boost::shared_ptr<YieldTermStructure>(
           new DiscountCurve(
               std::vector<Date>(dates,dates+LENGTH(dates)),
               std::vector<DiscountFactor>(discounts,
                                           discounts+LENGTH(discounts)),
               Actual365Fixed())));

    boost::shared_ptr<HullWhite> model(new HullWhite(termStructure));

    Integer start[] = { -3, 0, 3 };
    Integer length[] = { 2, 5, 10 };
    Rate rates[] = { 0.02, 0.04, 0.06 };
    boost::shared_ptr<IborIndex> euribor(new Euribor6M(termStructure));

    boost::shared_ptr<PricingEngine> engine(
                                        new TreeVanillaSwapEngine(model,120));

    #if defined(QL_USE_INDEXED_COUPON)
    Real tolerance = 4.0e-3;
    #else
    Real tolerance = 1.0e-8;
    #endif

    for (Size i=0; i<LENGTH(start); i++) {

        Date startDate = calendar.advance(settlement,start[i],Months);
        if (startDate < today) {
            Date fixingDate = calendar.advance(startDate,-2,Days);
            TimeSeries<Real> pastFixings;
            pastFixings[fixingDate] = 0.03;
            IndexManager::instance().setHistory(euribor->name(),
                                                pastFixings);
        }

        for (Size j=0; j<LENGTH(length); j++) {

            Date maturity = calendar.advance(startDate,length[i],Years);
            Schedule fixedSchedule(startDate, maturity, Period(Annual),
                                   calendar, Unadjusted, Unadjusted,
                                   DateGeneration::Forward, false);
            Schedule floatSchedule(startDate, maturity, Period(Semiannual),
                                   calendar, Following, Following,
                                   DateGeneration::Forward, false);
            for (Size k=0; k<LENGTH(rates); k++) {

                VanillaSwap swap(VanillaSwap::Payer, 1000000.0,
                                 fixedSchedule, rates[k], Thirty360(),
                                 floatSchedule, euribor, 0.0, Actual360());
                swap.setPricingEngine(boost::shared_ptr<PricingEngine>(
                                   new DiscountingSwapEngine(termStructure)));
                Real expected = swap.NPV();
                swap.setPricingEngine(engine);
                Real calculated = swap.NPV();

                Real error = std::fabs((expected-calculated)/expected);
                if (error > tolerance) {
                    BOOST_ERROR("Failed to reproduce swap NPV:"
                                << QL_FIXED << std::setprecision(9)
                                << "\n    calculated: " << calculated
                                << "\n    expected:   " << expected
                                << QL_SCIENTIFIC
                                << "\n    rel. error: " << error);
                }
                std::cout << swap.NPV() << "\t";
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }
}

void testFuturesConvexityBias() {
    LARGE_TITLE("Testing Hull-White futures convexity bias...");

    // G. Kirikos, D. Novak, "Convexity Conundrums", Risk Magazine, March 1997
    // http://www.powerfinance.com/convexity/
    Real futureQuote = 94.0;
    Real a = 0.03;
    Real sigma = 0.015;
    Time t = 5.0;
    Time T = 5.25;

    Rate expectedForward = 0.0573037;
    Real tolerance       = 0.0000001;

    Rate futureImpliedRate = (100.0-futureQuote)/100.0;
    Rate calculatedForward =
        futureImpliedRate - HullWhite::convexityBias(futureQuote,t,T,sigma,a);

    Real error = std::fabs(calculatedForward-expectedForward);

    if (error > tolerance) {
        BOOST_ERROR("Failed to reproduce convexity bias:"
                    << "\ncalculated: " << calculatedForward
                    << "\n  expected: " << expectedForward
                    << QL_SCIENTIFIC
                    << "\n     error: " << error
                    << "\n tolerance: " << tolerance);
    }
}

int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;

        //---------------------------------------------------------------------------

        testCachedHullWhite();
        testSwaps();
        testFuturesConvexityBias();


        //---------------------------------------------------------------------------

        // End test
        Real seconds = timer.elapsed();
        Integer hours = int(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = int(seconds/60);
        seconds -= minutes * 60;
        std::cout << " \nRun completed in ";
        if (hours > 0)
            std::cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            std::cout << minutes << " m ";
        std::cout << std::fixed << std::setprecision(0)
                  << seconds << " s\n" << std::endl;
        return 0;

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}
