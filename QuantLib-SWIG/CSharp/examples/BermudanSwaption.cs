/*
 Copyright (C) 2005 Dominic Thuillier

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

using System;
using QuantLib;

namespace BermudanSwaption
{
	class Run
	{
		private const int numRows = 5;
		private const int numCols = 5;

		private static int[] swapLenghts = { 1, 2, 3, 4, 5 };
		private static double[] swaptionVols = {
			0.1490, 0.1340, 0.1228, 0.1189, 0.1148,
			0.1290, 0.1201, 0.1146, 0.1108, 0.1040,
			0.1149, 0.1112, 0.1070, 0.1010, 0.0957,
			0.1047, 0.1021, 0.0980, 0.0951, 0.1270,
			0.1000, 0.0950, 0.0900, 0.1230, 0.1160 };

		private static void calibrateModel(
			ShortRateModel model,
			CalibrationHelperVector helpers,
			double lambda ) 
		{
			Simplex om = new Simplex( lambda, 1e-9 );
			om.setEndCriteria( new EndCriteria(10000, 1e-7) );
			model.calibrate(helpers, om);

			// Output the implied Black volatilities
			for (int i=0; i<numRows; i++) 
			{
				int j = numCols - i -1; // 1x5, 2x4, 3x3, 4x2, 5x1
				int k = i*numCols + j;
				double npv = helpers[i].modelValue();
				double implied = helpers[i].impliedVolatility(
					npv,
					1e-4,
					1000,
					0.05,
					0.50 );
				double diff = implied - swaptionVols[k];

				Console.WriteLine( "{0}x{1}: model {2}, market {3} ({4})",
					i+1, swapLenghts[j], implied, swaptionVols[k], diff );
			}
		}

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
			Date todaysDate = new Date(15, Month.February, 2002);
			Calendar calendar = new TARGET();
			Date settlementDate = new Date(19, Month.February, 2002);
			Settings.instance().setEvaluationDate( todaysDate );

			// flat yield term structure impling 1x5 swap at 5%
			Quote flatRate = new SimpleQuote(0.04875825);
			FlatForward myTermStructure = new FlatForward(
				settlementDate,
				new QuoteHandle( flatRate ),
				new Actual365Fixed() );
			YieldTermStructureHandle rhTermStructure =
                new YieldTermStructureHandle();
			rhTermStructure.linkTo( myTermStructure );

			// Define the ATM/OTM/ITM swaps
			Frequency fixedLegFrequency = Frequency.Annual;
			BusinessDayConvention fixedLegConvention =
                BusinessDayConvention.Unadjusted;
			BusinessDayConvention floatingLegConvention =
                BusinessDayConvention.ModifiedFollowing;
			DayCounter fixedLegDayCounter =
                new Thirty360( Thirty360.Convention.European );
			Frequency floatingLegFrequency = Frequency.Semiannual;
			bool payFixedRate = true;
			int fixingDays = 2;
			double dummyFixedRate = 0.03;
			Xibor indexSixMonths =
                new Euribor( 6, TimeUnit.Months, rhTermStructure );

			Date startDate = calendar.advance(settlementDate,1,TimeUnit.Years,
				floatingLegConvention);
			Date maturity = calendar.advance(startDate,5,TimeUnit.Years,
				floatingLegConvention);
			Schedule fixedSchedule = new Schedule(calendar,startDate,maturity,
			fixedLegFrequency,fixedLegConvention);
			Schedule floatSchedule = new Schedule(calendar,startDate,maturity,
			floatingLegFrequency,floatingLegConvention);
			SimpleSwap swap = new SimpleSwap(
					   payFixedRate, 1000.0,
					   fixedSchedule, dummyFixedRate, fixedLegDayCounter,
					   floatSchedule, indexSixMonths, fixingDays, 0.0,
					   rhTermStructure);
			double fixedATMRate = swap.fairRate();
			double fixedOTMRate = fixedATMRate * 1.2;
			double fixedITMRate = fixedATMRate * 0.8;

			SimpleSwap atmSwap = new SimpleSwap(
					   payFixedRate, 1000.0,
					   fixedSchedule, fixedATMRate, fixedLegDayCounter,
					   floatSchedule, indexSixMonths, fixingDays, 0.0,
					   rhTermStructure );
			SimpleSwap otmSwap = new SimpleSwap(
					   payFixedRate, 1000.0,
					   fixedSchedule, fixedOTMRate, fixedLegDayCounter,
					   floatSchedule, indexSixMonths, fixingDays, 0.0,
					   rhTermStructure);
			SimpleSwap itmSwap = new SimpleSwap(
					   payFixedRate, 1000.0,
					   fixedSchedule, fixedITMRate, fixedLegDayCounter,
					   floatSchedule, indexSixMonths, fixingDays, 0.0,
					   rhTermStructure);

			// defining the swaptions to be used in model calibration
			PeriodVector swaptionMaturities = new PeriodVector();
			swaptionMaturities.Add( new Period(1, TimeUnit.Years) );
			swaptionMaturities.Add( new Period(2, TimeUnit.Years) );
			swaptionMaturities.Add( new Period(3, TimeUnit.Years) );
			swaptionMaturities.Add( new Period(4, TimeUnit.Years) );
			swaptionMaturities.Add( new Period(5, TimeUnit.Years) );

			CalibrationHelperVector swaptions = new CalibrationHelperVector();

			// List of times that have to be included in the timegrid
			DoubleVector times = new DoubleVector();

	        for ( int i=0; i<numRows; i++) {
				int j = numCols - i -1; // 1x5, 2x4, 3x3, 4x2, 5x1
				int k = i*numCols + j;
				Quote vol = new SimpleQuote( swaptionVols[k] );
				SwaptionHelper helper = new SwaptionHelper(
								swaptionMaturities[i],
                               new Period(swapLenghts[j], TimeUnit.Years),
                               new QuoteHandle(vol),
                               indexSixMonths,
                               indexSixMonths.frequency(),
                               indexSixMonths.dayCounter(),
                               rhTermStructure );
				swaptions.Add( helper );
				times.AddRange( helper.times() );
			}

	        // Building time-grid
		    TimeGrid grid = new TimeGrid( times, 30);

			// defining the models
			// G2 modelG2 = new G2(rhTermStructure));
			HullWhite modelHW = new HullWhite( rhTermStructure );
			HullWhite modelHW2 = new HullWhite( rhTermStructure );
			BlackKarasinski modelBK = new BlackKarasinski( rhTermStructure );

			// model calibrations

//			Console.WriteLine( "G2 (analytic formulae) calibration" );
//			for (int i=0; i<swaptions.Count; i++)
//				swaptions[i].setPricingEngine( new G2SwaptionEngine( modelG2, 6.0, 16 ) );
//
//			calibrateModel( modelG2, swaptions, 0.05);
//			Console.WriteLine( "calibrated to:" );
//			Console.WriteLine( "a     = " + modelG2.parameters()[0] );
//			Console.WriteLine( "sigma = " + modelG2.parameters()[1] );
//			Console.WriteLine( "b     = " + modelG2.parameters()[2] );
//			Console.WriteLine( "eta   = " + modelG2.parameters()[3] );
//			Console.WriteLine( "rho   = " + modelG2.parameters()[4] );

			Console.WriteLine( "Hull-White (analytic formulae) calibration" );
			for (int i=0; i<swaptions.Count; i++)
				swaptions[i].setPricingEngine(
                                       new JamshidianSwaptionEngine(modelHW));

			calibrateModel( modelHW, swaptions, 0.05);
//			Console.WriteLine( "calibrated to:" );
//            Console.WriteLine( "a = " + modelHW.parameters()[0] );
//            Console.WriteLine( "sigma = " + modelHW.parameters()[1] );


			Console.WriteLine( "Hull-White (numerical) calibration" );
			for (int i=0; i<swaptions.Count; i++)
				swaptions[i].setPricingEngine(
                                       new TreeSwaptionEngine(modelHW2,grid));

			calibrateModel(modelHW2, swaptions, 0.05);
//        std::cout << "calibrated to:\n"
//                  << "a = " << modelHW2->params()[0] << ", "
//                  << "sigma = " << modelHW2->params()[1]
//                  << std::endl << std::endl;


			Console.WriteLine( "Black-Karasinski (numerical) calibration" );
			for (int i=0; i<swaptions.Count; i++)
				swaptions[i].setPricingEngine(
                                        new TreeSwaptionEngine(modelBK,grid));

			calibrateModel(modelBK, swaptions, 0.05);
//        std::cout << "calibrated to:\n"
//                  << "a = " << modelBK->params()[0] << ", "
//                  << "sigma = " << modelBK->params()[1]
//                  << std::endl << std::endl;

			// ATM Bermudan swaption pricing

			Console.WriteLine( "Payer bermudan swaption struck at {0} (ATM)",
                               fixedATMRate );

	        DateVector bermudanDates = new DateVector();
			Schedule schedule = new Schedule(calendar,startDate,maturity,
				Frequency.Quarterly,BusinessDayConvention.Following);

			for (uint i=0; i<schedule.size(); i++)
				bermudanDates.Add( schedule.date( i ) );
			Exercise bermudaExercise = new BermudanExercise( bermudanDates );

			Swaption bermudanSwaption =
                new Swaption( atmSwap, bermudaExercise, rhTermStructure,
                              new TreeSwaptionEngine(modelHW, 50));
			Console.WriteLine( "HW: " + bermudanSwaption.NPV() );

			bermudanSwaption.setPricingEngine(
                                       new TreeSwaptionEngine(modelHW2, 50));
			Console.WriteLine( "HW (num): " + bermudanSwaption.NPV() );

			Console.WriteLine( "Press return to exit" );
			Console.ReadLine();
		}
	}
}
