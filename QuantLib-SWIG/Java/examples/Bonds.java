
/*
 Copyright (C) 2014 Wondersys Srl

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

package examples;

import java.util.ArrayList;

import org.quantlib.Actual360;
import org.quantlib.Actual365Fixed;
import org.quantlib.ActualActual;
import org.quantlib.BlackIborCouponPricer;
import org.quantlib.BusinessDayConvention;
import org.quantlib.Calendar;
import org.quantlib.Compounding;
import org.quantlib.ConstantOptionletVolatility;
import org.quantlib.Date;
import org.quantlib.DateGeneration;
import org.quantlib.DayCounter;
import org.quantlib.DepositRateHelper;
import org.quantlib.DiscountingBondEngine;
import org.quantlib.DoubleVector;
import org.quantlib.Euribor6M;
import org.quantlib.FixedRateBond;
import org.quantlib.FixedRateBondHelper;
import org.quantlib.FloatingRateBond;
import org.quantlib.FloatingRateCouponPricer;
import org.quantlib.Frequency;
import org.quantlib.IborCouponPricer;
import org.quantlib.IborIndex;
import org.quantlib.Month;
import org.quantlib.OptionletVolatilityStructureHandle;
import org.quantlib.Period;
import org.quantlib.PiecewiseFlatForward;
import org.quantlib.PiecewiseLinearForward;
import org.quantlib.PricingEngine;
import org.quantlib.QuantLib;
import org.quantlib.QuantLibJNI;
import org.quantlib.QuoteHandle;
import org.quantlib.QuoteVector;
import org.quantlib.RateHelper;
import org.quantlib.RateHelperVector;
import org.quantlib.QuoteHandleVector;
import org.quantlib.RelinkableYieldTermStructureHandle;
import org.quantlib.Schedule;
import org.quantlib.Settings;
import org.quantlib.SimpleQuote;
import org.quantlib.SwapRateHelper;
import org.quantlib.TARGET;
import org.quantlib.Thirty360;
import org.quantlib.TimeUnit;
import org.quantlib.USDLibor;
import org.quantlib.UnitedStates;
import org.quantlib.YieldTermStructure;
import org.quantlib.YieldTermStructureHandle;
import org.quantlib.ZeroCouponBond;

public class Bonds {
    static {
        try {
            System.loadLibrary("QuantLibJNI");
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }
    public static void main(String[] args) throws Exception {

        // MARKET DATA
        Calendar cal = new TARGET();
        Date settlementDate = new Date(18, Month.September, 2008);
        // must be a business day
        settlementDate = cal.adjust(settlementDate);

        int fixingDays = 3;
        int settlementdays = 3;

        Date todayDate = cal.advance(settlementDate,
                                     -fixingDays, TimeUnit.Days);

        Settings.instance().setEvaluationDate(todayDate);

        System.out.println("Today: " + todayDate.weekday()
                           + ", " + todayDate);
        System.out.println("Settlement date: " + settlementDate.weekday()
                           + ", " + settlementDate);

        // Building of the bonds discounting yield curve

        // RATE HELPERS

        // RateHelpers are built from the above quotes together with
        // other instrument dependant infos. Quotes are passed in
        // relinkable handles which could be relinked to some other
        // data source later.

        // Common data

        // ZC rates for the short end

        QuoteHandle zc3mRate = new QuoteHandle(new SimpleQuote(0.0096));
        QuoteHandle zc6mRate = new QuoteHandle(new SimpleQuote(0.0145));
        QuoteHandle zc1yRate = new QuoteHandle(new SimpleQuote(0.0194));

        DayCounter zcBondsDayCounter = new Actual365Fixed();

        DepositRateHelper zc3m =
            new DepositRateHelper(zc3mRate, new Period(3, TimeUnit.Months),
                                  fixingDays, cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, zcBondsDayCounter);

        DepositRateHelper zc6m =
            new DepositRateHelper(zc6mRate, new Period(6, TimeUnit.Months),
                                  fixingDays, cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, zcBondsDayCounter);

        DepositRateHelper zc1y =
            new DepositRateHelper(zc1yRate, new Period(1, TimeUnit.Years),
                                  fixingDays, cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, zcBondsDayCounter);

        // setup bonds
        double redemption = 100.0;
        final int numberOfBonds = 5;

        Date issueDates[] = {
            new Date(15, Month.March, 2005),
            new Date(15, Month.June, 2005),
            new Date(30, Month.June, 2006),
            new Date(15, Month.November, 2002),
            new Date(15, Month.May, 1987),
        };

        Date maturities[] = {
            new Date(31, Month.August, 2010),
            new Date(31, Month.August, 2011),
            new Date(31, Month.August, 2013),
            new Date(15, Month.August, 2018),
            new Date(15, Month.May, 2038),
        };

        double couponRates[] = {
            0.02375,
            0.04625,
            0.03125,
            0.04000,
            0.04500
        };

        SimpleQuote marketQuotes[] = {
            new SimpleQuote(100.390625),
            new SimpleQuote(106.21875),
            new SimpleQuote(100.59375),
            new SimpleQuote(101.6875),
            new SimpleQuote(102.140625)
        };

        QuoteHandleVector quoteHandle = new QuoteHandleVector();
        for (int i=0; i<numberOfBonds; i++){
            quoteHandle.add(new QuoteHandle(marketQuotes[i]));
        }

        ArrayList<FixedRateBondHelper> bondHelpers =
            new ArrayList<FixedRateBondHelper>();
        for (int i=0; i<numberOfBonds;i++){
            Schedule schedule =
                new Schedule(issueDates[i],
                             maturities[i],
                             new Period(Frequency.Semiannual),
                             new UnitedStates(UnitedStates.Market.GovernmentBond),
                             BusinessDayConvention.Unadjusted,
                             BusinessDayConvention.Unadjusted,
                             DateGeneration.Rule.Backward,
                             false);
            DoubleVector couponsVector = new DoubleVector();
            couponsVector.add(couponRates[i]);

            DayCounter dayCountConvBond =
                new ActualActual(ActualActual.Convention.Bond);

            FixedRateBondHelper bondHelper =
                new FixedRateBondHelper(quoteHandle.get(i),
                                        settlementdays,
                                        100.0,
                                        schedule,
                                        couponsVector,
                                        dayCountConvBond,
                                        BusinessDayConvention.Unadjusted,
                                        redemption,
                                        issueDates[i]);
            bondHelpers.add(bondHelper);
        }

        // CURVE BUILDING

        // Any DayCounter would be fine
        // ActualActual::ISDA ensures that 30 years is 30.0
        DayCounter termStructureDayCounter =
            new ActualActual(ActualActual.Convention.ISDA);

        // A depo-bond curve
        RateHelperVector bondInstruments = new RateHelperVector();

        // Adding the ZC bonds to the curve for the short end
        bondInstruments.add(zc3m);
        bondInstruments.add(zc6m);
        bondInstruments.add(zc1y);

        // Adding the Fixed rate bonds to the curve for the long end
        for (int i=0; i<numberOfBonds;i++){
            bondInstruments.add(bondHelpers.get(i));
        }

        YieldTermStructure bondDiscountingTermStructure =
            new PiecewiseFlatForward(settlementDate,bondInstruments,
                                     termStructureDayCounter);

        // Building of the Libor forecasting curve

        // SimpleQuote stores a value which can be manually changed;
        // other Quote subclasses could read the value from a database
        // or some kind of data feed

        // deposits
        QuoteHandle d1wQuoteHandle = new QuoteHandle(new SimpleQuote(0.043375));
        QuoteHandle d1mQuoteHandle = new QuoteHandle(new SimpleQuote(0.031875));
        QuoteHandle d3mQuoteHandle = new QuoteHandle(new SimpleQuote(0.0320375));
        QuoteHandle d6mQuoteHandle = new QuoteHandle(new SimpleQuote(0.03385));
        QuoteHandle d9mQuoteHandle = new QuoteHandle(new SimpleQuote(0.0338125));
        QuoteHandle d1yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0335125));
        //swaps
        QuoteHandle s2yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0295));
        QuoteHandle s3yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0323));
        QuoteHandle s5yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0359));
        QuoteHandle s10yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0412));
        QuoteHandle s15yQuoteHandle = new QuoteHandle(new SimpleQuote(0.0433));

        // RATE HELPERS

        // RateHelpers are built from the above quotes together with
        // other instrument dependant infos. Quotes are passed in
        // relinkable handles which could be relinked to some other
        // data source later

        // deposits
        DayCounter depositDayCounter = new Actual360();

        RateHelper d1w =
            new DepositRateHelper(d1wQuoteHandle,
                                  new Period(1, TimeUnit.Weeks),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);
        RateHelper d1m =
            new DepositRateHelper(d1mQuoteHandle,
                                  new Period(1, TimeUnit.Months),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);
        RateHelper d3m =
            new DepositRateHelper(d3mQuoteHandle,
                                  new Period(3, TimeUnit.Months),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);
        RateHelper d6m =
            new DepositRateHelper(d6mQuoteHandle,
                                  new Period(6, TimeUnit.Months),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);
        RateHelper d9m =
            new DepositRateHelper(d9mQuoteHandle,
                                  new Period(9, TimeUnit.Months),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);
        RateHelper d1y =
            new DepositRateHelper(d1yQuoteHandle,
                                  new Period(1, TimeUnit.Years),
                                  fixingDays,
                                  cal,
                                  BusinessDayConvention.ModifiedFollowing,
                                  true, depositDayCounter);

        // setup swaps
        Frequency swFixedLegFrequency = Frequency.Annual;
        BusinessDayConvention swFixedLegConvention =
            BusinessDayConvention.Unadjusted;
        DayCounter swFixedLegDayCounter =
            new Thirty360(Thirty360.Convention.European);
        IborIndex swFloatingLegIndex = new Euribor6M();

        Period forwardStart  = new Period(1, TimeUnit.Days);
        QuoteHandle spread = new QuoteHandle();
        RateHelper s2y =
            new SwapRateHelper(s2yQuoteHandle,
                               new Period(2, TimeUnit.Years),
                               cal,
                               swFixedLegFrequency,
                               swFixedLegConvention,
                               swFixedLegDayCounter,
                               swFloatingLegIndex,
                               spread, forwardStart);
        RateHelper s3y =
            new SwapRateHelper(s3yQuoteHandle,
                               new Period(3, TimeUnit.Years),
                               cal,
                               swFixedLegFrequency,
                               swFixedLegConvention,
                               swFixedLegDayCounter,
                               swFloatingLegIndex,
                               spread, forwardStart);
        RateHelper s5y =
            new SwapRateHelper(s5yQuoteHandle,
                               new Period(5, TimeUnit.Years),
                               cal,
                               swFixedLegFrequency,
                               swFixedLegConvention,
                               swFixedLegDayCounter,
                               swFloatingLegIndex,
                               spread, forwardStart);
        RateHelper s10y =
            new SwapRateHelper(s10yQuoteHandle,
                               new Period(10, TimeUnit.Years),
                               cal,
                               swFixedLegFrequency,
                               swFixedLegConvention,
                               swFixedLegDayCounter,
                               swFloatingLegIndex,
                               spread, forwardStart);
        RateHelper s15y =
            new SwapRateHelper(s15yQuoteHandle,
                               new Period(15, TimeUnit.Years),
                               cal,
                               swFixedLegFrequency,
                               swFixedLegConvention,
                               swFixedLegDayCounter,
                               swFloatingLegIndex,
                               spread, forwardStart);

        // CURVE BUILDING

        // A depo-swap curve
        RateHelperVector depoSwapInstruments = new RateHelperVector();
        depoSwapInstruments.add(d1w);
        depoSwapInstruments.add(d1m);
        depoSwapInstruments.add(d3m);
        depoSwapInstruments.add(d6m);
        depoSwapInstruments.add(d9m);
        depoSwapInstruments.add(d1y);
        depoSwapInstruments.add(s2y);
        depoSwapInstruments.add(s3y);
        depoSwapInstruments.add(s5y);
        depoSwapInstruments.add(s10y);
        depoSwapInstruments.add(s15y);

        YieldTermStructure depoSwapTermStructure =
            new PiecewiseFlatForward(
                settlementDate, depoSwapInstruments,
                termStructureDayCounter);

        // Term structures that will be used for pricing
        // the one used for discounting cash flows
        RelinkableYieldTermStructureHandle discountingTermStructure = new RelinkableYieldTermStructureHandle();

        RelinkableYieldTermStructureHandle forecastingTermStructure = new RelinkableYieldTermStructureHandle();

        // BONDS TO BE PRICED

        // Common data

        double faceAmount = 100.0;

        // Price engine
        PricingEngine bondEngine =
            new DiscountingBondEngine(discountingTermStructure);

        // Zero coupon bond

        ZeroCouponBond zeroCouponBond =
            new ZeroCouponBond(
                settlementdays,
                new UnitedStates(UnitedStates.Market.GovernmentBond),
                faceAmount,
                new Date(15, Month.August, 2013),
                BusinessDayConvention.Following,
                116.92,
                new Date(15, Month.August, 2003));

        zeroCouponBond.setPricingEngine(bondEngine);

        // Fixed 4.5% US Treasury Note
        Schedule fixedRateBondSchedule =
            new Schedule(
                new Date(15, Month.May, 2007),
                new Date(15, Month.May, 2017),
                new Period(Frequency.Semiannual),
                new UnitedStates(UnitedStates.Market.GovernmentBond),
                BusinessDayConvention.Unadjusted,
                BusinessDayConvention.Unadjusted,
                DateGeneration.Rule.Backward,
                false);

        DoubleVector rateVector = new DoubleVector();
        rateVector.add(0.045);
        FixedRateBond fixedRateBond =
            new FixedRateBond(
                settlementdays,
                faceAmount,
                fixedRateBondSchedule,
                rateVector,
                new ActualActual(ActualActual.Convention.Bond),
                BusinessDayConvention.ModifiedFollowing,
                100.0,
                new Date(15, Month.May, 2007));

        fixedRateBond.setPricingEngine(bondEngine);

        RelinkableYieldTermStructureHandle liborTermStructure =
            new RelinkableYieldTermStructureHandle();

        IborIndex libor3m = new USDLibor(new Period(3, TimeUnit.Months),
                                         liborTermStructure);
        libor3m.addFixing(new Date(17, Month.July, 2008), 0.0278625);

        Schedule floatingBondSchedule =
            new Schedule(
                new Date(21, Month.October, 2005),
                new Date(21, Month.October, 2010),
                new Period(Frequency.Quarterly),
                new UnitedStates(UnitedStates.Market.NYSE),
                BusinessDayConvention.Unadjusted,
                BusinessDayConvention.Unadjusted,
                DateGeneration.Rule.Backward,
                true);

        DoubleVector gearings = new DoubleVector();
        gearings.add(1.0);

        DoubleVector spreads = new DoubleVector();
        spreads.add(0.001);

        DoubleVector caps = new DoubleVector();
        DoubleVector floors = new DoubleVector();

        FloatingRateBond floatingRateBond =
            new FloatingRateBond(
                settlementdays,
                faceAmount,
                floatingBondSchedule,
                libor3m,
                new Actual360(),
                BusinessDayConvention.ModifiedFollowing,
                2,
                gearings,
                spreads,
                caps,
                floors,
                true,
                100.0,
                new Date(21, Month.October, 2005));
        floatingRateBond.setPricingEngine(bondEngine);

        IborCouponPricer pricer = new BlackIborCouponPricer();
        OptionletVolatilityStructureHandle volatility =
            new OptionletVolatilityStructureHandle(
                new ConstantOptionletVolatility(
                    settlementdays,
                    cal,
                    BusinessDayConvention.ModifiedFollowing,
                    0.0,
                    new Actual365Fixed()));

        pricer.setCapletVolatility(volatility);
        QuantLib.setCouponPricer(floatingRateBond.cashflows(), pricer);

        // Yield curve bootstrapping
        discountingTermStructure.linkTo(bondDiscountingTermStructure);
        forecastingTermStructure.linkTo(depoSwapTermStructure);

        // We are using the depo & swap curve to estimate the future
        // Libor rates
        liborTermStructure.linkTo(depoSwapTermStructure);

        // output results to screen
        System.out.printf("\n%18s%10s%10s%10s\n",
                          "", "ZC", "Fixed", "Floating");

        String fmt = "%18s%10.2f%10.2f%10.2f\n";
        System.out.printf(fmt, "Net present value",
                          zeroCouponBond.NPV(),
                          fixedRateBond.NPV(),
                          floatingRateBond.NPV());
        System.out.printf(fmt, "Clean price",
                          zeroCouponBond.cleanPrice(),
                          fixedRateBond.cleanPrice(),
                          floatingRateBond.cleanPrice());
        System.out.printf(fmt, "Dirty price",
                          zeroCouponBond.dirtyPrice(),
                          fixedRateBond.dirtyPrice(),
                          floatingRateBond.dirtyPrice());
        System.out.printf("%18s%8.2f %%%8.2f %%%8.2f %%\n", "Yield",
                          100*zeroCouponBond.yield(new Actual360(),
                                                   Compounding.Compounded,
                                                   Frequency.Annual),
                          100*fixedRateBond.yield(new Actual360(),
                                                  Compounding.Compounded,
                                                  Frequency.Annual),
                          100*floatingRateBond.yield(new Actual360(),
                                                     Compounding.Compounded,
                                                     Frequency.Annual));

        System.out.println("\nSample indirect computations (for the floating rate bond): ");
        System.out.printf("Yield to Clean Price: %.2f\n",
                          floatingRateBond.cleanPrice(
                              floatingRateBond.yield(new Actual360(),
                                                     Compounding.Compounded,
                                                     Frequency.Annual),
                              new Actual360(), Compounding.Compounded,
                              Frequency.Annual, settlementDate));

        System.out.printf("Clean Price to Yield: %.2f %%\n",
                          100*floatingRateBond.yield(
                                floatingRateBond.cleanPrice(),
                                new Actual360(), Compounding.Compounded,
                                Frequency.Annual, settlementDate));

        System.out.println("Done");
    }
}
