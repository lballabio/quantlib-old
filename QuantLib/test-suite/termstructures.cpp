/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2003 RiskMap srl
 Copyright (C) 2015 Peter Caspers

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

#include "termstructures.hpp"
#include "utilities.hpp"
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/termstructures/yield/piecewiseyieldcurve.hpp>
#include <ql/termstructures/yield/impliedtermstructure.hpp>
#include <ql/termstructures/yield/forwardspreadedtermstructure.hpp>
#include <ql/termstructures/yield/zerospreadedtermstructure.hpp>
#include <ql/experimental/yield/clonedyieldtermstructure.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <ql/math/comparison.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/currency.hpp>
#include <ql/utilities/dataformatters.hpp>

#include <boost/make_shared.hpp>

using namespace QuantLib;
using namespace boost::unit_test_framework;

namespace {

    struct Datum {
        Integer n;
        TimeUnit units;
        Rate rate;
    };

    struct CommonVars {
        // common data
        Calendar calendar;
        Natural settlementDays;
        boost::shared_ptr<YieldTermStructure> termStructure;
        boost::shared_ptr<YieldTermStructure> dummyTermStructure;
        boost::shared_ptr<YieldTermStructure> floatingTermStructure;

        // cleanup
        SavedSettings backup;

        // setup
        CommonVars() {
            calendar = TARGET();
            settlementDays = 2;
            Date today = calendar.adjust(Date::todaysDate());
            Settings::instance().evaluationDate() = today;
            Date settlement = calendar.advance(today,settlementDays,Days);
            Datum depositData[] = {
                { 1, Months, 4.581 },
                { 2, Months, 4.573 },
                { 3, Months, 4.557 },
                { 6, Months, 4.496 },
                { 9, Months, 4.490 }
            };
            Datum swapData[] = {
                {  1, Years, 4.54 },
                {  5, Years, 4.99 },
                { 10, Years, 5.47 },
                { 20, Years, 5.89 },
                { 30, Years, 5.96 }
            };
            Size deposits = LENGTH(depositData),
                swaps = LENGTH(swapData);

            std::vector<boost::shared_ptr<RateHelper> > instruments(
                                                              deposits+swaps);
            for (Size i=0; i<deposits; i++) {
                instruments[i] = boost::shared_ptr<RateHelper>(new
                    DepositRateHelper(depositData[i].rate/100,
                                      depositData[i].n*depositData[i].units,
                                      settlementDays, calendar,
                                      ModifiedFollowing, true,
                                      Actual360()));
            }
            boost::shared_ptr<IborIndex> index(new IborIndex("dummy",
                                                             6*Months,
                                                             settlementDays,
                                                             Currency(),
                                                             calendar,
                                                             ModifiedFollowing,
                                                             false,
                                                             Actual360()));
            for (Size i=0; i<swaps; ++i) {
                instruments[i+deposits] = boost::shared_ptr<RateHelper>(new
                    SwapRateHelper(swapData[i].rate/100,
                                   swapData[i].n*swapData[i].units,
                                   calendar,
                                   Annual, Unadjusted, Thirty360(),
                                   index));
            }
            termStructure = boost::shared_ptr<YieldTermStructure>(new
                PiecewiseYieldCurve<Discount,LogLinear>(settlement,
                                                        instruments, Actual360()));
            dummyTermStructure = boost::shared_ptr<YieldTermStructure>(new
                PiecewiseYieldCurve<Discount,LogLinear>(settlement,
                                                        instruments, Actual360()));
            floatingTermStructure = boost::shared_ptr<YieldTermStructure>(
                new PiecewiseYieldCurve<Discount, LogLinear>(
                    settlementDays, calendar, instruments, Actual360()));
        }
    };

}


void TermStructureTest::testReferenceChange() {

    BOOST_TEST_MESSAGE("Testing term structure against evaluation date change...");

    CommonVars vars;

    boost::shared_ptr<SimpleQuote> flatRate (new SimpleQuote);
    Handle<Quote> flatRateHandle(flatRate);
    vars.termStructure = boost::shared_ptr<YieldTermStructure>(
                          new FlatForward(vars.settlementDays, NullCalendar(),
                                          flatRateHandle, Actual360()));
    Date today = Settings::instance().evaluationDate();
    flatRate->setValue(.03);
    Integer days[] = { 10, 30, 60, 120, 360, 720 };
    Size i;

    std::vector<DiscountFactor> expected(LENGTH(days));
    for (i=0; i<LENGTH(days); i++)
        expected[i] = vars.termStructure->discount(today+days[i]);

    Settings::instance().evaluationDate() = today+30;
    std::vector<DiscountFactor> calculated(LENGTH(days));
    for (i=0; i<LENGTH(days); i++)
        calculated[i] = vars.termStructure->discount(today+30+days[i]);

    for (i=0; i<LENGTH(days); i++) {
        if (!close(expected[i],calculated[i]))
            BOOST_ERROR("\n  Discount at " << days[i] << " days:\n"
                        << std::setprecision(12)
                        << "    before date change: " << expected[i] << "\n"
                        << "    after date change:  " << calculated[i]);
    }
}


void TermStructureTest::testImplied() {

    BOOST_TEST_MESSAGE("Testing consistency of implied term structure...");

    CommonVars vars;

    Real tolerance = 1.0e-10;
    Date today = Settings::instance().evaluationDate();
    Date newToday = today + 3*Years;
    Date newSettlement = vars.calendar.advance(newToday,
                                               vars.settlementDays,Days);
    Date testDate = newSettlement + 5*Years;
    boost::shared_ptr<YieldTermStructure> implied(
        new ImpliedTermStructure(Handle<YieldTermStructure>(vars.termStructure),
                                 newSettlement));
    DiscountFactor baseDiscount = vars.termStructure->discount(newSettlement);
    DiscountFactor discount = vars.termStructure->discount(testDate);
    DiscountFactor impliedDiscount = implied->discount(testDate);
    if (std::fabs(discount - baseDiscount*impliedDiscount) > tolerance)
        BOOST_ERROR(
            "unable to reproduce discount from implied curve\n"
            << QL_FIXED << std::setprecision(10)
            << "    calculated: " << baseDiscount*impliedDiscount << "\n"
            << "    expected:   " << discount);
}

void TermStructureTest::testImpliedObs() {

    BOOST_TEST_MESSAGE("Testing observability of implied term structure...");

    CommonVars vars;

    Date today = Settings::instance().evaluationDate();
    Date newToday = today + 3*Years;
    Date newSettlement = vars.calendar.advance(newToday,
                                               vars.settlementDays,Days);
    RelinkableHandle<YieldTermStructure> h;
    boost::shared_ptr<YieldTermStructure> implied(
                                  new ImpliedTermStructure(h, newSettlement));
    Flag flag;
    flag.registerWith(implied);
    h.linkTo(vars.termStructure);
    if (!flag.isUp())
        BOOST_ERROR("Observer was not notified of term structure change");
}

void TermStructureTest::testFSpreaded() {

    BOOST_TEST_MESSAGE("Testing consistency of forward-spreaded term structure...");

    CommonVars vars;

    Real tolerance = 1.0e-10;
    boost::shared_ptr<Quote> me(new SimpleQuote(0.01));
    Handle<Quote> mh(me);
    boost::shared_ptr<YieldTermStructure> spreaded(
        new ForwardSpreadedTermStructure(
            Handle<YieldTermStructure>(vars.termStructure),mh));
    Date testDate = vars.termStructure->referenceDate() + 5*Years;
    DayCounter tsdc  = vars.termStructure->dayCounter();
    DayCounter sprdc = spreaded->dayCounter();
    Rate forward = vars.termStructure->forwardRate(testDate, testDate, tsdc,
                                                   Continuous, NoFrequency);
    Rate spreadedForward = spreaded->forwardRate(testDate, testDate, sprdc,
                                                 Continuous, NoFrequency);
    if (std::fabs(forward - (spreadedForward-me->value())) > tolerance)
        BOOST_ERROR(
            "unable to reproduce forward from spreaded curve\n"
            << std::setprecision(10)
            << "    calculated: "
            << io::rate(spreadedForward-me->value()) << "\n"
            << "    expected:   " << io::rate(forward));
}

void TermStructureTest::testFSpreadedObs() {

    BOOST_TEST_MESSAGE("Testing observability of forward-spreaded "
                       "term structure...");

    CommonVars vars;

    boost::shared_ptr<SimpleQuote> me(new SimpleQuote(0.01));
    Handle<Quote> mh(me);
    RelinkableHandle<YieldTermStructure> h; //(vars.dummyTermStructure);
    boost::shared_ptr<YieldTermStructure> spreaded(
        new ForwardSpreadedTermStructure(h,mh));
    Flag flag;
    flag.registerWith(spreaded);
    h.linkTo(vars.termStructure);
    if (!flag.isUp())
        BOOST_ERROR("Observer was not notified of term structure change");
    flag.lower();
    me->setValue(0.005);
    if (!flag.isUp())
        BOOST_ERROR("Observer was not notified of spread change");
}

void TermStructureTest::testZSpreaded() {

    BOOST_TEST_MESSAGE("Testing consistency of zero-spreaded term structure...");

    CommonVars vars;

    Real tolerance = 1.0e-10;
    boost::shared_ptr<Quote> me(new SimpleQuote(0.01));
    Handle<Quote> mh(me);
    boost::shared_ptr<YieldTermStructure> spreaded(
        new ZeroSpreadedTermStructure(
            Handle<YieldTermStructure>(vars.termStructure),mh));
    Date testDate = vars.termStructure->referenceDate() + 5*Years;
    DayCounter rfdc  = vars.termStructure->dayCounter();
    Rate zero = vars.termStructure->zeroRate(testDate, rfdc,
                                             Continuous, NoFrequency);
    Rate spreadedZero = spreaded->zeroRate(testDate, rfdc,
                                           Continuous, NoFrequency);
    if (std::fabs(zero - (spreadedZero-me->value())) > tolerance)
        BOOST_ERROR(
            "unable to reproduce zero yield from spreaded curve\n"
            << std::setprecision(10)
            << "    calculated: " << io::rate(spreadedZero-me->value()) << "\n"
            << "    expected:   " << io::rate(zero));
}

void TermStructureTest::testZSpreadedObs() {

    BOOST_TEST_MESSAGE("Testing observability of zero-spreaded term structure...");

    CommonVars vars;

    boost::shared_ptr<SimpleQuote> me(new SimpleQuote(0.01));
    Handle<Quote> mh(me);
    RelinkableHandle<YieldTermStructure> h(vars.dummyTermStructure);

    boost::shared_ptr<YieldTermStructure> spreaded(
        new ZeroSpreadedTermStructure(h,mh));
    Flag flag;
    flag.registerWith(spreaded);
    h.linkTo(vars.termStructure);
    if (!flag.isUp())
        BOOST_ERROR("Observer was not notified of term structure change");
    flag.lower();
    me->setValue(0.005);
    if (!flag.isUp())
        BOOST_ERROR("Observer was not notified of spread change");
}

void TermStructureTest::testCreateWithNullUnderlying() {
    BOOST_TEST_MESSAGE(
        "Testing that a zero-spreaded curve can be created with "
        "a null underlying curve...");

    CommonVars vars;

    Handle<Quote> spread(boost::shared_ptr<Quote>(new SimpleQuote(0.01)));
    RelinkableHandle<YieldTermStructure> underlying;
    // this shouldn't throw
    boost::shared_ptr<YieldTermStructure> spreaded(
        new ZeroSpreadedTermStructure(underlying,spread));
    // if we do this, the curve can work.
    underlying.linkTo(vars.termStructure);
    // check that we can use it
    spreaded->referenceDate();
}

void TermStructureTest::testLinkToNullUnderlying() {
    BOOST_TEST_MESSAGE(
        "Testing that an underlying curve can be relinked to "
        "a null underlying curve...");

    CommonVars vars;

    Handle<Quote> spread(boost::shared_ptr<Quote>(new SimpleQuote(0.01)));
    RelinkableHandle<YieldTermStructure> underlying(vars.termStructure);
    boost::shared_ptr<YieldTermStructure> spreaded(
        new ZeroSpreadedTermStructure(underlying,spread));
    // check that we can use it
    spreaded->referenceDate();
    // if we do this, the curve can't work anymore. But it shouldn't
    // throw as long as we don't try to use it.
    underlying.linkTo(boost::shared_ptr<YieldTermStructure>());
}

void TermStructureTest::testClonedYieldTermStructure() {

    BOOST_TEST_MESSAGE("Testing cloned yield term structure");

    CommonVars vars;

    Date today = Settings::instance().evaluationDate();
    Date settlement = vars.calendar.advance(today, vars.settlementDays * Days);
    Date shiftedToday = vars.calendar.adjust(today + 100 * Days);
    Date shiftedSettlement =
        vars.calendar.advance(shiftedToday, vars.settlementDays * Days);

    // link to fixed reference date termstructure,
    // freeze reference date of the copy

    ClonedYieldTermStructure y1(vars.termStructure);

    if (y1.referenceDate() != vars.termStructure->referenceDate()) {
        BOOST_ERROR("cloned yts (fixed) has different reference date ("
                    << y1.referenceDate() << ") than source ("
                    << vars.termStructure->referenceDate());
    }

    Real calculated = y1.discount(Date(7, Jul, 2037), true);
    Real expected = vars.termStructure->discount(Date(7, Jul, 2037), true);
    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (fixed) has different discount on 07-07-2037 ("
                    << calculated << ") than source (" << expected
                    << ") error is " << (calculated - expected));
    }

    calculated = y1.discount(Date(7, Jul, 2100), true);
    expected = vars.termStructure->discount(Date(7, Jul, 2100), true);
    if (std::fabs(calculated - expected) > 1E-12) {
        BOOST_ERROR("cloned yts (fixed) has different discount on 07-07-2100 ("
                    << calculated << ") than source (" << expected
                    << ") error is " << (calculated - expected));
    }

    Settings::instance().evaluationDate() = shiftedToday;

    if (y1.referenceDate() != vars.termStructure->referenceDate()) {
        BOOST_ERROR("cloned yts (fixed) changes reference date ("
                    << y1.referenceDate() << "), source has "
                    << vars.termStructure->referenceDate());
    }

    Settings::instance().evaluationDate() = today;

    // link to fixed reference date termstructure,
    // make the copy floating with constant zero yields

    ClonedYieldTermStructure y2(vars.termStructure,
                                ClonedYieldTermStructure::ConstantZeroYields,
                                ClonedYieldTermStructure::None, vars.calendar);

    calculated = y2.discount(settlement + 3000, true);
    expected = vars.termStructure->discount(settlement + 3000, true);
    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, constant zero yields) has different "
                    "discount on 05-Apr-2025 ("
                    << calculated
                    << ") than source ("
                    << expected
                    << ") error is "
                    << (calculated - expected));
    }

    Date maxExp = vars.termStructure->maxDate();

    if (y2.maxDate() != maxExp) {
        BOOST_ERROR(
            "cloned yts (float, constant zero yields) has wrong maxDate ("
            << y2.maxDate() << "), expected " << maxExp);
    }

    Settings::instance().evaluationDate() = shiftedToday;

    calculated = y2.discount(shiftedSettlement + 3000, true);

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, constant zero yields) has unexpected "
                    "discount after shift of evaluation date ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    if (y2.maxDate() != maxExp + (shiftedSettlement - settlement)) {
        BOOST_ERROR("cloned yts (float, constant zero yields) has wrong "
                    "maxDate after shift of evaluation date ("
                    << y2.maxDate()
                    << "), expected "
                    << (maxExp + (shiftedSettlement - settlement)));
    }

    Settings::instance().evaluationDate() = today;

    // link to fixed reference date termstructure,
    // make the copy floating with forward forward zero yields

    ClonedYieldTermStructure y3(vars.termStructure,
                                ClonedYieldTermStructure::ForwardForward,
                                ClonedYieldTermStructure::None, vars.calendar);

    expected = vars.termStructure->discount(settlement + 3000);
    calculated = y3.discount(settlement + 3000, true);

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, forward-forward) has unexpected "
                    "discount on 05-04-2025 ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    if (y3.maxDate() != maxExp) {
        BOOST_ERROR("cloned yts (float, forrward-forward) has wrong maxDate ("
                    << y3.maxDate() << "), expected " << maxExp);
    }

    // we have to calculate the expected forward discount before
    // the date shift, because the date shift triggers a bootstrap
    // since the rate helpers are relative date rate helpers
    expected = vars.termStructure->discount(shiftedSettlement + 3000, true) /
               vars.termStructure->discount(shiftedSettlement, true);

    Settings::instance().evaluationDate() = shiftedToday;

    calculated = y3.discount(shiftedSettlement + 3000, true);

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, forward-forward) has unexpected "
                    "discount on 05-04-2025 + shift ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    if (y3.maxDate() != maxExp) {
        BOOST_ERROR("cloned yts (float, forrward-forward) has wrong maxDate "
                    "after shift of evaluation date ("
                    << y3.maxDate()
                    << "), expected "
                    << maxExp);
    }

    Settings::instance().evaluationDate() = today;

    // link to floating reference date termstructure,
    // make the copy fixed

    ClonedYieldTermStructure y4(vars.floatingTermStructure);

    calculated = y4.discount(Date(1, Mar, 2017));
    expected = vars.floatingTermStructure->discount(Date(1, Mar, 2017), true);

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (fixed, linked to float) has unexpected "
                    "discount on 01-03-2017 ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    Settings::instance().evaluationDate() = Date(4, Apr, 2017);

    calculated = y4.discount(Date(1, Mar, 2017), true);

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (fixed, linked to float) has unexpected "
                    "discount on 01-03-2017 after shift of evaluation date ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    Settings::instance().evaluationDate() = today;

    // link to flat termstructure, check max date

    boost::shared_ptr<YieldTermStructure> flat =
        boost::make_shared<FlatForward>(0, vars.calendar, 0.03,
                                        Actual365Fixed());

    ClonedYieldTermStructure y5(flat,
                                ClonedYieldTermStructure::ConstantZeroYields);

    expected = 0.03;
    calculated =
        y5.zeroRate(Date(1, Jul, 2025), Actual365Fixed(), Continuous).rate();

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, constant zero yields, linked to flat "
                    "yts) has unexpected "
                    "discount on 01-07-2025 ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    if (y5.maxDate() != flat->maxDate()) {
        BOOST_ERROR(
            "cloned yts (float, constant zero yields, linked to flat yts) "
            "has unexpected max date ("
            << y5.maxDate()
            << ") expected "
            << flat->maxDate());
    }

    Settings::instance().evaluationDate() = Date(1, Jan, 2022);

    calculated =
        y5.zeroRate(Date(1, Jul, 2025), Actual365Fixed(), Continuous).rate();

    if (!close_enough(calculated, expected)) {
        BOOST_ERROR("cloned yts (float, constant zero yields, linked to flat "
                    "yts) has unexpected "
                    "discount on 01-07-2025 after shift of evaluation date ("
                    << calculated
                    << "), expected "
                    << expected
                    << " error is "
                    << (calculated - expected));
    }

    if (y5.maxDate() != flat->maxDate()) {
        BOOST_ERROR(
            "cloned yts (float, constant zero yields, linked to flat yts) "
            "has unexpected max date after shift of evaluation date ("
            << y5.maxDate()
            << ") expected "
            << flat->maxDate());
    }
}

test_suite* TermStructureTest::suite() {
    test_suite* suite = BOOST_TEST_SUITE("Term structure tests");
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testReferenceChange));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testImplied));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testImpliedObs));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testFSpreaded));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testFSpreadedObs));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testZSpreaded));
    suite->add(QUANTLIB_TEST_CASE(&TermStructureTest::testZSpreadedObs));
    suite->add(QUANTLIB_TEST_CASE(
                         &TermStructureTest::testCreateWithNullUnderlying));
    suite->add(QUANTLIB_TEST_CASE(
                             &TermStructureTest::testLinkToNullUnderlying));
    suite->add(QUANTLIB_TEST_CASE(
                             &TermStructureTest::testClonedYieldTermStructure));
    return suite;
}

 
