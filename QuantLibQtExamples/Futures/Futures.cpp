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

// the only header you need to use QuantLib
#include <ql/quantlib.hpp>
#include <../customutilities.hpp>

#ifdef BOOST_MSVC
/* Uncomment the following lines to unmask floating-point
   exceptions. Warning: unpredictable results can arise...

   See http://www.wilmott.com/messageview.cfm?catid=10&threadid=9481
   Is there anyone with a definitive word about this?
*/
// #include <float.h>
// namespace { unsigned int u = _controlfp(_EM_INEXACT, _MCW_EM); }
#endif

#include <boost/timer.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif


int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;

        /*********************
         ***  MARKET DATA  ***
         *********************/
        Calendar calendar = TARGET();
        Date settlementDate(22, September, 2004);
        // must be a business day
        settlementDate = calendar.adjust(settlementDate);

        Integer fixingDays = 2;
        Date todaysDate = calendar.advance(settlementDate, -fixingDays, Days);
        // nothing to do with Date::todaysDate
        Settings::instance().evaluationDate() = todaysDate;


        todaysDate = Settings::instance().evaluationDate();
        std::cout << "Today: " << todaysDate.weekday()
                  << ", " << todaysDate << std::endl;

        std::cout << "Settlement date: " << settlementDate.weekday()
                  << ", " << settlementDate << std::endl;

        // futures
        Real fut1Quote=96.2875;
        Real fut2Quote=96.7875;
        Real fut3Quote=96.9875;
        Real fut4Quote=96.6875;
        Real fut5Quote=96.4875;
        Real fut6Quote=96.3875;
        Real fut7Quote=96.2875;
        Real fut8Quote=96.0875;


        /********************
         ***    QUOTES    ***
         ********************/
        // futures
        boost::shared_ptr<Quote> fut1Price(new SimpleQuote(fut1Quote));
        boost::shared_ptr<Quote> fut2Price(new SimpleQuote(fut2Quote));
        boost::shared_ptr<Quote> fut3Price(new SimpleQuote(fut3Quote));
        boost::shared_ptr<Quote> fut4Price(new SimpleQuote(fut4Quote));
        boost::shared_ptr<Quote> fut5Price(new SimpleQuote(fut5Quote));
        boost::shared_ptr<Quote> fut6Price(new SimpleQuote(fut6Quote));
        boost::shared_ptr<Quote> fut7Price(new SimpleQuote(fut7Quote));
        boost::shared_ptr<Quote> fut8Price(new SimpleQuote(fut8Quote));


        /*********************
         ***  RATE HELPERS ***
         *********************/
        DayCounter depositDayCounter = Actual360();

        // setup futures
        // Rate convexityAdjustment = 0.0;
        Integer futMonths = 3;
        Date imm = IMM::nextDate(settlementDate);
        boost::shared_ptr<RateHelper> fut1(new FuturesRateHelper(
            Handle<Quote>(fut1Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut2(new FuturesRateHelper(
            Handle<Quote>(fut2Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut3(new FuturesRateHelper(
            Handle<Quote>(fut3Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut4(new FuturesRateHelper(
            Handle<Quote>(fut4Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut5(new FuturesRateHelper(
            Handle<Quote>(fut5Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut6(new FuturesRateHelper(
            Handle<Quote>(fut6Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut7(new FuturesRateHelper(
            Handle<Quote>(fut7Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));
        imm = IMM::nextDate(imm+1);
        boost::shared_ptr<RateHelper> fut8(new FuturesRateHelper(
            Handle<Quote>(fut8Price),
            imm,
            futMonths, calendar, ModifiedFollowing,
            true, depositDayCounter));


        /*********************
         **  CURVE BUILDING **
         *********************/
        DayCounter termStructureDayCounter = ActualActual(ActualActual::ISDA);
        double tolerance = 1.0e-15;

        // A futures curve
        std::vector<boost::shared_ptr<RateHelper> > futsInstruments;
        futsInstruments.push_back(fut1);
        futsInstruments.push_back(fut2);
        futsInstruments.push_back(fut3);
        futsInstruments.push_back(fut4);
        futsInstruments.push_back(fut5);
        futsInstruments.push_back(fut6);
        futsInstruments.push_back(fut7);
        futsInstruments.push_back(fut8);
        boost::shared_ptr<YieldTermStructure> futsTermStructure(
            new PiecewiseYieldCurve<Discount,LogLinear>(
                                       settlementDate, futsInstruments,
                                       termStructureDayCounter,
                                       tolerance));

        // Term structures that will be used for pricing:
        // the one used for discounting cash flows
        RelinkableHandle<YieldTermStructure> discountingTermStructure;
        // the one used for forward rate forecasting
        RelinkableHandle<YieldTermStructure> forecastingTermStructure;


        /*********************
        * SWAPS TO BE PRICED *
        **********************/

        // constant nominal 1,000,000 Euro
        Real nominal = 1000000.0;
        // fixed leg
        Frequency fixedLegFrequency = Annual;
        BusinessDayConvention fixedLegConvention = Unadjusted;
        BusinessDayConvention floatingLegConvention = ModifiedFollowing;
        DayCounter fixedLegDayCounter = Thirty360(Thirty360::European);
        Rate fixedRate = 0.04;
        DayCounter floatingLegDayCounter = Actual360();

        // floating leg
        Frequency floatingLegFrequency = Semiannual;
        boost::shared_ptr<IborIndex> euriborIndex(
                                     new Euribor6M(forecastingTermStructure));
        Spread spread = 0.0;

        Integer lenghtInYears = 2; //5
        VanillaSwap::Type swapType = VanillaSwap::Payer;

        Date maturity = settlementDate + lenghtInYears*Years;
        Schedule fixedSchedule(settlementDate, maturity,
                               Period(fixedLegFrequency),
                               calendar, fixedLegConvention,
                               fixedLegConvention,
                               DateGeneration::Forward, false);
        Schedule floatSchedule(settlementDate, maturity,
                               Period(floatingLegFrequency),
                               calendar, floatingLegConvention,
                               floatingLegConvention,
                               DateGeneration::Forward, false);
        VanillaSwap spot3YearSwap(swapType, nominal,
            fixedSchedule, fixedRate, fixedLegDayCounter,
            floatSchedule, euriborIndex, spread,
            floatingLegDayCounter);

        Date fwdStart = calendar.advance(settlementDate, 1, Years);
        Date fwdMaturity = fwdStart + lenghtInYears*Years;
        Schedule fwdFixedSchedule(fwdStart, fwdMaturity,
                                  Period(fixedLegFrequency),
                                  calendar, fixedLegConvention,
                                  fixedLegConvention,
                                  DateGeneration::Forward, false);
        Schedule fwdFloatSchedule(fwdStart, fwdMaturity,
                                  Period(floatingLegFrequency),
                                  calendar, floatingLegConvention,
                                  floatingLegConvention,
                                  DateGeneration::Forward, false);
        VanillaSwap oneYearForward5YearSwap(swapType, nominal,
            fwdFixedSchedule, fixedRate, fixedLegDayCounter,
            fwdFloatSchedule, euriborIndex, spread,
            floatingLegDayCounter);


        /***************
        * SWAP PRICING *
        ****************/

        // utilities for reporting
        std::vector<std::string> headers(4);
        headers[0] = "term structure";
        headers[1] = "net present value";
        headers[2] = "fair spread";
        headers[3] = "fair fixed rate";
        std::string separator = " | ";
        Size width = headers[0].size() + separator.size()
                   + headers[1].size() + separator.size()
                   + headers[2].size() + separator.size()
                   + headers[3].size() + separator.size() - 1;
        std::string rule(width, '-'), dblrule(width, '=');
        std::string tab(8, ' ');

        // calculations
        std::cout << dblrule << std::endl;
        std::cout <<  "5-year market swap-rate | fut8Price = "
                  << std::setprecision(6) << fut8Price->value()
                  << std::endl;
        std::cout << dblrule << std::endl;

        std::cout << tab << "3-years swap paying "
                  << io::rate(fixedRate) << std::endl;
        std::cout << headers[0] << separator
                  << headers[1] << separator
                  << headers[2] << separator
                  << headers[3] << separator << std::endl;
        std::cout << rule << std::endl;

        Real NPV;
        Rate fairRate;
        Spread fairSpread;

        boost::shared_ptr<PricingEngine> swapEngine(
                         new DiscountingSwapEngine(discountingTermStructure));

        spot3YearSwap.setPricingEngine(swapEngine);
        oneYearForward5YearSwap.setPricingEngine(swapEngine);

        // Of course, you're not forced to really use different curves
        forecastingTermStructure.linkTo(futsTermStructure);
        discountingTermStructure.linkTo(futsTermStructure);

        NPV = spot3YearSwap.NPV();
        fairSpread = spot3YearSwap.fairSpread();
        fairRate = spot3YearSwap.fairRate();

        std::cout << std::setw(headers[0].size())
                  << "Futures TS" << separator;
        std::cout << std::setw(headers[1].size())
                  << std::fixed << std::setprecision(2) << NPV << separator;
        std::cout << std::setw(headers[2].size())
                  << io::rate(fairSpread) << separator;
        std::cout << std::setw(headers[3].size())
                  << io::rate(fairRate) << separator;
        std::cout << std::endl;

        /*
          T-bill futures:
           r = %rate/100 = 1-IMM/100
           IMM Index (IMM) = 100 - %rate = 100(1-r)
           V = 1,000,000(1-(%rate/100)(90/360))
             = 1,000,000(1-r/4)

           Eurodollar spot:    discount interest rates N(1+r(90/360))
           Eurodollar futures: add-on interest rates N(1-r(90/360))
          */
        Real fut8Rate = 1.0-fut8Quote/100.0;
        std::cout << "fut8Quote: " << fut8Quote << std::endl;
        std::cout << "fut8Rate: " << io::rate(fut8Rate) << std::endl;


        // let's check that the fut8Quote has been correctly re-priced
        QL_REQUIRE(std::fabs(fairRate-fut8Rate)<1e-8,
                   "5-years swap mispriced by "
                   << io::rate(std::fabs(fairRate-fut8Rate)));






        /**************************************
         ***  Futures Convexity Adjustment  ***
         **************************************/
        LARGE_TITLE("Futures Convexity Adjustment");

        boost::shared_ptr<IborIndex> index;
        /*
        boost::shared_ptr<Quote> quote(
                    new FuturesConvAdjustmentQuote(index, futuresDate, futuresQuote,
                                                   volatility, meanReversion));*/
        /*
        FuturesConvAdjustmentQuote(const boost::shared_ptr<IborIndex>& index,
                                   const Date& futuresDate,
                                   const Handle<Quote>& futuresQuote,
                                   const Handle<Quote>& volatility,
                                   const Handle<Quote>& meanReversion);
          */
        //std::cout << "value():         " << quote.value() << std::endl;
        //std::cout << "futuresValue():  " << quote.futuresValue() << std::endl;
        //std::cout << "volatility():    " << quote.volatility() << std::endl;
        //std::cout << "meanReversion(): " << quote.meanReversion() << std::endl;
        //std::cout << "immDate():       " << quote.immDate() << std::endl;

        std::vector<boost::shared_ptr<IborIndex> > iborIndexes;
        std::vector<boost::shared_ptr<RateHelper> > rateHelpers;
        // Create FuturesRateHelper
        Date immDate(Day(21), Month(3), Year(2012));
        Natural lengthInMonths(3);
        Handle<Quote> convAdj(new SimpleQuote(0.05));
        std::vector<boost::shared_ptr<SimpleQuote> > iborQuotes;
        std::vector<boost::shared_ptr<IborIndex> >::const_iterator ibor;
        for (ibor=iborIndexes.begin(); ibor!=iborIndexes.end(); ++ibor) {
            boost::shared_ptr<SimpleQuote> quote(new SimpleQuote);
            iborQuotes.push_back(quote);
            Handle<Quote> quoteHandle(quote);
            rateHelpers.push_back(boost::shared_ptr<RateHelper> (new
                FuturesRateHelper(quoteHandle,
                                  immDate,//(*ibor)->tenor(),
                                  lengthInMonths,//(*ibor)->fixingDays(),
                                  (*ibor)->fixingCalendar(),
                                  (*ibor)->businessDayConvention(),
                                  (*ibor)->endOfMonth(),
                                  (*ibor)->dayCounter(),
                                  convAdj)));
        }

        std::cout << Date() << std::endl;
        std::cout << IMM::nextDate(immDate) << std::endl;
        std::cout << IMM::code(immDate) << std::endl;

        //FuturesRateHelper helper;
        /*
        FuturesRateHelper::FuturesRateHelper(const Handle<Quote>& price,
                                         const Date& immDate,
                                         Natural lengthInMonths,
                                         const Calendar& calendar,
                                         BusinessDayConvention convention,
                                         bool endOfMonth,
                                         const DayCounter& dayCounter,
                                         const Handle<Quote>& convAdj)
          */
        //std::cout << "convexityAdjustment(): " << helper.convexityAdjustment() << std::endl;





        /***************************
         ***  Commodity Futures  ***
         ***************************/
        LARGE_TITLE("Commodity Futures");


        /***************************
         ***  Currency Futures  ***
         ***************************/
        LARGE_TITLE("Currency Futures");



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
