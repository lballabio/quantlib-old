/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

#include "gsr.hpp"

#include <ql/pricingengines/swaption/treeswaptionengine.hpp> // just for test

using namespace QuantLib;
using namespace boost::unit_test_framework;

void GsrTest::testGsrProcess() {
	
	BOOST_MESSAGE("Testing GsrProcess");

	Date refDate = Settings::instance().evaluationDate();

	// constant reversion, constant volatility, test conditional expectation and variance against existing HullWhiteForwardProcess
	// technically we test two representations of the same constant reversion and volatility structure, namely with and without step dates

	Real tol = 1E-8;

	Real reversion = 0.01;
	Real modelvol = 0.01;

	Handle<YieldTermStructure> yts0( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),0.00,Actual365Fixed())));

	std::vector<Date> stepDates0;
	std::vector<Real> vols0(1,modelvol);
	std::vector<Real> reversions0(1,reversion);

	std::vector<Date> stepDates1;
	for(Size i=1;i<60;i++) stepDates1.push_back(refDate+(i*6*Months));
	std::vector<Real> vols1(stepDates1.size()+1,modelvol);
	std::vector<Real> reversions1(stepDates1.size()+1,reversion);

	Real T = 10.0;
	do {

		boost::shared_ptr<Gsr> model(new Gsr(yts0,stepDates0,vols0,reversions0,T));
		boost::shared_ptr<GsrProcess> gsrProcess = model->stateProcess();
		boost::shared_ptr<Gsr> model2(new Gsr(yts0,stepDates1,vols1,reversions1,T));
		boost::shared_ptr<GsrProcess> gsrProcess2 = model2->stateProcess();

		boost::shared_ptr<HullWhiteForwardProcess> hwProcess(new HullWhiteForwardProcess(yts0,reversion,modelvol));
		hwProcess->setForwardMeasureTime(T);

		Real w,t,xw,hwVal,gsrVal,gsr2Val;
		
		t=0.5;
		do {
			w=0.0;
			do {
				xw = -0.1;
				do {
					hwVal = hwProcess->expectation(w,xw,t-w);
					gsrVal = gsrProcess->expectation(w,xw,t-w);
					gsr2Val = gsrProcess2->expectation(w,xw,t-w);
					if( fabs( hwVal - gsrVal ) > tol )
						BOOST_ERROR("Expectation E^{T=" << T << "}(x(" << t << ") | x(" << w << ") = " << xw << " is different in HullWhiteProcess(" << hwVal << ") and GsrProcess (" << gsrVal << ")");
					if( fabs( hwVal - gsr2Val ) > tol )
						BOOST_ERROR("Expectation E^{T=" << T << "}(x(" << t << ") | x(" << w << ") = " << xw << " is different in HullWhiteProcess(" << hwVal << ") and GsrProcess2 (" << gsr2Val << ")");
					hwVal = hwProcess->variance(w,xw,t-w);
					gsrVal = gsrProcess->variance(w,xw,t-w);
					gsr2Val = gsrProcess2->variance(w,xw,t-w);
					if( fabs( hwVal - gsrVal ) > tol )
						BOOST_ERROR("Variance V((x(" << t << ") | x(" << w << ") = " << xw << " is different in HullWhiteProcess(" << hwVal << ") and GsrProcess (" << gsrVal << ")");
					if( fabs( hwVal - gsr2Val ) > tol )
						BOOST_ERROR("Variance V((x(" << t << ") | x(" << w << ") = " << xw << " is different in HullWhiteProcess(" << hwVal << ") and GsrProcess2 (" << gsr2Val << ")");
					xw +=0.01;
				} while(xw <= 0.1);
				w += t / 5.0;
			} while(w <= t-0.1);
			t += T / 20.0;
		} while(t <= T-0.1);
		T +=10.0;
	} while(T <= 30.0);

	// time dependent reversion and volatility (test cases to be added)

	Array times(2);
	Array vols(3);
	Array reversions(3);

	times[0] = 1.0; times[1] = 2.0;
	vols[0] = 0.2; vols[1] = 0.3; vols[2] = 0.4;
	reversions[0] = 0.50; reversions[1] = 0.80; reversions[2] = 1.30;
	
	GsrProcess p(times,vols,reversions);
	p.setForwardMeasureTime(10.0);

	// add more test cases here ...

}

void GsrTest::testGsrModel() {

	BOOST_MESSAGE("Testing GsrModel");

	Date refDate = Settings::instance().evaluationDate();

	Real modelvol = 0.01;
	Real reversion = 0.01;

	std::vector<Date> stepDates; // no step dates
	std::vector<Real> vols(1,modelvol);
	std::vector<Real> reversions(1,reversion);

	std::vector<Date> stepDates1; // artificial step dates (should yield the same result)
	for(Size i=1;i<60;i++) stepDates1.push_back(refDate+(i*6*Months));
	std::vector<Real> vols1(stepDates1.size()+1,modelvol);
	std::vector<Real> reversions1(stepDates1.size()+1,reversion);

	Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),0.03,Actual365Fixed())));
	boost::shared_ptr<Gsr> model(new Gsr(yts,stepDates,vols,reversions,50.0));
	boost::shared_ptr<Gsr> model2(new Gsr(yts,stepDates1,vols1,reversions1,50.0));
	boost::shared_ptr<HullWhite> hw(new HullWhite(yts,reversion,modelvol));

	// test zerobond prices against existing HullWhite model
	// technically we test two representations of the same constant reversion and volatility structure, namely with and without step dates

	Real tol0 = 1E-8;

	Real w,t,xw;

	w=0.1;
	do {
		t=w+0.1;
		do {
			xw=-0.10;
			do {
				Real yw=(xw-model->stateProcess()->expectation(0.0,0.0,w))/ model->stateProcess()->stdDeviation(0.0,0.0,w);
				Real rw=xw+0.03; // instantaneous forward is 0.03
				Real gsrVal = model->zerobond(t,w,yw);
				Real gsr2Val = model2->zerobond(t,w,yw);
				Real hwVal = hw->discountBond(w,t,rw);
				if(fabs(gsrVal-hwVal) > tol0)
					BOOST_ERROR("Zerobond P(" << w << "," << t << " | x=" << xw << " / y=" << yw << ") is different in HullWhite (" << hwVal << ") and Gsr (" << gsrVal <<")");
				if(fabs(gsr2Val-hwVal) > tol0)
					BOOST_ERROR("Zerobond P(" << w << "," << t << " | x=" << xw << " / y=" << yw << ") is different in HullWhite (" << hwVal << ") and Gsr2 (" << gsr2Val <<")");
				xw+=0.01;
			} while(xw <= 0.10);
			t+=2.5;
		} while(t <= 50.0);
		w+=5.0;
	} while(w <= 50.0);

	// price put and call and test parity, test against existing hull white jamshidian engine
	
	Date expiry = TARGET().advance(refDate,5*Years);
	Period tenor = 10*Years;
	boost::shared_ptr<SwapIndex> swpIdx(new EuriborSwapIsdaFixA(tenor,yts));
	Real forward = swpIdx->fixing(expiry);

	boost::shared_ptr<VanillaSwap> underlying = swpIdx->underlyingSwap(expiry);
	boost::shared_ptr<VanillaSwap> underlyingFixed = MakeVanillaSwap(10*Years,swpIdx->iborIndex(),forward)
		        .withEffectiveDate(swpIdx->valueDate(expiry))
                .withFixedLegCalendar(swpIdx->fixingCalendar())
                .withFixedLegDayCount(swpIdx->dayCounter())
                .withFixedLegTenor(swpIdx->fixedLegTenor())
                .withFixedLegConvention(swpIdx->fixedLegConvention())
                .withFixedLegTerminationDateConvention(swpIdx->fixedLegConvention());
	boost::shared_ptr<Exercise> exercise(new EuropeanExercise(expiry));
	boost::shared_ptr<Swaption> stdswaption(new Swaption(underlyingFixed,exercise));
	boost::shared_ptr<NonstandardSwaption> nonstdswaption(new NonstandardSwaption(*stdswaption));

	Real call = model->swaptionPrice(Option::Call,expiry,tenor,forward,swpIdx,Null<Date>(),0.0,64,7.0,true,false);
	Real put = model->swaptionPrice(Option::Put,expiry,tenor,forward,swpIdx,Null<Date>(),0.0,64,7.0,true,false);
	Real annuity = model->swapAnnuity(expiry,tenor,swpIdx);

	stdswaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new JamshidianSwaptionEngine(hw,yts)));
	Real HwJamNpv = stdswaption->NPV();

	nonstdswaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new GsrNonstandardSwaptionEngine(model,64,7.0,true,false)));
	stdswaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new GsrSwaptionEngine(model,64,7.0,true,false)));
	Real GsrNonStdNpv = nonstdswaption->NPV();
	Real GsrStdNpv = stdswaption->NPV();
	stdswaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new GsrJamshidianSwaptionEngine(model)));
	Real GsrJamNpv = stdswaption->NPV();

	if( fabs(HwJamNpv - put) > 0.00005 ) BOOST_ERROR("Jamshidian HW NPV (" << HwJamNpv << ") deviates from Gsr built in NPV (" << put << ")");
	if( fabs(HwJamNpv - GsrNonStdNpv) > 0.00005 ) BOOST_ERROR("Jamshidian HW NPV (" << HwJamNpv << ") deviates from GsrNonstandardSwaptionEngine NPV (" << GsrNonStdNpv << ")");
	if( fabs(HwJamNpv - GsrStdNpv) > 0.00005 ) BOOST_ERROR("Jamshidian HW NPV (" << HwJamNpv << ") deviates from GsrSwaptionEngine NPV (" << GsrStdNpv << ")");

	if( fabs(HwJamNpv - GsrJamNpv) > 0.00001 ) BOOST_ERROR("Jamshidian HW NPV (" << HwJamNpv << ") deviates from Gsr Jamshidian NPV (" << GsrJamNpv << ")");

	if( fabs((call-put)/annuity) > 0.00001 ) BOOST_ERROR("Put call parity violated (" << (call-put)/annuity << ")");

}


// not a real test case yet ...
void GsrTest::testNonstandardSwaption() {

	BOOST_MESSAGE("Testing Nonstandard Swaption");

	Date refDate = Settings::instance().evaluationDate();

	// market data

	Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),0.03,Actual365Fixed())));
	boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(new ConstantSwaptionVolatility(0,TARGET(),ModifiedFollowing,0.20,Actual365Fixed()));

	boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months,yts));
	boost::shared_ptr<SwapIndex> standardSwapBase(new EuriborSwapIsdaFixA(10*Years,yts));

	// instrument (non standard swaption)
	
	std::vector<Real> fixedNominal(10), floatingNominal(20), fixedRate(10);
	for(Size i=0;i<10;i++) {
		fixedNominal[i] = (i>0 ? fixedNominal[i-1] : 100000000.0 )*1.075;
		floatingNominal[2*i] = floatingNominal[2*i+1] = fixedNominal[i];
		fixedRate[i] = 0.035+0.0030*i;
	}

	Date effective = TARGET().advance(refDate,2*Days);
	Date maturity = TARGET().advance(effective,10*Years);
	Schedule fixedSchedule(effective,maturity,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,DateGeneration::Forward,false);
	Schedule floatingSchedule(effective,maturity,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,DateGeneration::Forward,false);

	std::vector<Date> exerciseDates;
	for(Size i=1;i<10;i++) exerciseDates.push_back(TARGET().advance(fixedSchedule[i],-2*Days));

	boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(NonstandardSwap::Payer,fixedNominal,floatingNominal,fixedSchedule,fixedRate,Thirty360(),floatingSchedule,iborIndex,0.0,Actual360()));
	//boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));
	boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDates[2]));
	boost::shared_ptr<NonstandardSwaption> swaption(new NonstandardSwaption(underlying,exercise));

	// model

	exerciseDates.pop_back();
	std::vector<Date> stepDates(exerciseDates);
	std::vector<Real> vols(exerciseDates.size()+1,0.01);
	std::vector<Real> reversions(exerciseDates.size()+1,0.03);

	std::vector<Date> zeroStepDates;
	std::vector<Real> singlevol(1,0.01);

	//boost::shared_ptr<Gsr> model(new Gsr(yts,stepDates,vols,reversions,50.0));
	boost::shared_ptr<Gsr> model(new Gsr(yts,zeroStepDates,singlevol,reversions[0],20.0));
	boost::shared_ptr<HullWhite> hw(new HullWhite(yts,0.03,0.01));

	boost::shared_ptr<GsrNonstandardSwaptionEngine> engine(new GsrNonstandardSwaptionEngine(model));
	boost::shared_ptr<GsrSwaptionEngine> engine2(new GsrSwaptionEngine(model,64,7.0));
	boost::shared_ptr<GsrJamshidianSwaptionEngine> engine3(new GsrJamshidianSwaptionEngine(model));
	
	//compare treeswaption engine with gsr engines (make that a test case later)
	//swaption2->setPricingEngine(boost::shared_ptr<TreeSwaptionEngine>(new TreeSwaptionEngine(hw,512)));
	//Real hwNpv = swaption2->NPV();
	//swaption2->setPricingEngine(engine2);
	//Real gsrNpv = swaption2->NPV();
	//swaption3->setPricingEngine(engine3);
	//Real gsrNpv2 = swaption3->NPV();
	//BOOST_MESSAGE("hwNPV = " << hwNpv << " gsr1=" << gsrNpv << " gsr2= " << gsrNpv2);


	// calibrate to non standard swaption ...

	swaption->setPricingEngine(engine);

	for(Size counter=0;counter<1;counter++) {

		std::cout << "Iteration # " << counter << ", Model parameters: ";
		for(Size i=0;i<model->params().size();i++) {
			std::cout << model->params()[i] << ";";
		}
		std::cout << std::endl;
		std::vector<boost::shared_ptr<CalibrationHelper>> basket = swaption->calibrationBasket(standardSwapBase,swaptionVol);

		std::cout << "Calibration Basket:" << std::endl;
		std::cout << "expiry;maturityDate;expiryTime;maturityTime;nominal;rate;marketvol" << std::endl;
		for(Size j=0;j<basket.size();j++) {
			// debug
			boost::shared_ptr<SwaptionHelper> helper = boost::dynamic_pointer_cast<SwaptionHelper>(basket[j]);
			Date endDate = helper->underlyingSwap()->fixedSchedule().dates().back();
			Real nominal = helper->underlyingSwap()->nominal();
			Real vol = helper->volatility();
			Real rate = helper->underlyingSwap()->fixedRate();
			Date expiry = helper->swaption()->exercise()->date(0);
			Real expiryTime = model->termStructure()->timeFromReference(expiry);
			Real endTime = model->termStructure()->timeFromReference(endDate);
			std::cout << expiry << ";" << endDate << ";" << expiryTime << ";" << endTime << ";" << nominal << ";" << rate << ";" << vol << std::endl;
			// end debug
		}

		for(Size i=0;i<basket.size();i++) basket[i]->setPricingEngine(engine2);
		LevenbergMarquardt lm;
		EndCriteria ec(2000,200,1E-8,1E-8,1E-8);
		
		model->calibrateIterative(basket,lm,ec);
		std::cout << "Model parameters after recalibration: ";
		for(Size i=0;i<model->params().size();i++) {
			std::cout << model->params()[i] << ";";
		}

		//std::cout << "standard swaption: " << basket[0]->modelValue() << " nonstandard swaption: " << swaption->NPV() << std::endl;

	}

	
			
}

// not a real test case yet ...
void GsrTest::testDummy() {

	BOOST_MESSAGE("Testing Something in Gsr ...");

	Date saveDate = Settings::instance().evaluationDate();

	Settings::instance().evaluationDate() = Date(27,March,2013);

	Date refDate = Settings::instance().evaluationDate();

	// Test numeraire time

	boost::math::ntl::RR::SetPrecision(256);

	Real rate = 0.00;

	Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),rate,Actual365Fixed())));
	std::vector<Date> volstepdates;
	std::vector<Real> vols;
	vols.push_back(0.010);

	boost::shared_ptr<SwapIndex> swpIdx(new EuriborSwapIsdaFixA(10*Years,yts));
	boost::shared_ptr<IborIndex> iborIdx(new Euribor(6*Months));
	Date expiry = refDate + 5*Years;

	boost::shared_ptr<VanillaSwap> underlying = MakeVanillaSwap(10*Years,iborIdx,rate).withType(VanillaSwap::Payer);
	boost::shared_ptr<Exercise> exercise(new EuropeanExercise(expiry));
	boost::shared_ptr<Swaption> swaption(new Swaption(underlying,exercise));

	Real t = yts->timeFromReference(expiry);
	Real T = 150.0;

	for(Size i=0;i<1;i++) {

		boost::shared_ptr<Gsr> gsr(new Gsr(yts,volstepdates,vols,-0.0110,T));

		// test paramters
		Real ext = gsr->stateProcess()->expectation(0.0,0.0,t);
		Real var = gsr->stateProcess()->variance(0.0,0.0,t);
		Real yt = gsr->stateProcess()->y(t);
		Real gtT = gsr->stateProcess()->G(t,T,ext);
		std::cout << "E(x(t)) = " << ext << std::endl;
		std::cout << "V(x(t)) = " << var << std::endl;
		std::cout << "y(t) = " << gsr->stateProcess()->y(t) << std::endl;
		std::cout << "G(t,T) = " << gsr->stateProcess()->G(t,T,ext) << std::endl;
		std::cout << "G(t,t+5) = " << gsr->stateProcess()->G(t,t+5,ext) << std::endl;
		// test zero coupon bond
		/*Real y=-5.0;
		while(y <= 10.0001) {
			Real x=y*std::sqrt(var)+ext;
			std::cout << y << ";" << gsr->numeraire(5.0,y) << ";" << x << ";" << exp(-x*gtT-0.5*yt*gtT*gtT) << std::endl;
			y+=0.1;
		}*/

		// test forward integration
		Array z = gsr->yGrid(10.0,32);
		Array payoff(z.size()), undeflatedpayoff(z.size());
		for(int j=0; j<z.size(); j++) {
			undeflatedpayoff[j] = gsr->forwardRate(expiry,iborIdx,expiry,z[j]);
			payoff[j] = undeflatedpayoff[j] / gsr->numeraire(t,z[j]);
		}
		CubicInterpolation payoff0(z.begin(),z.end(),payoff.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
		Real price=0.0;
		for(int j=0;j<z.size()-1; j++) {
			price += gsr->gaussianShiftedPolynomialIntegral( 0.0, payoff0.cCoefficients()[j], payoff0.bCoefficients()[j], payoff0.aCoefficients()[j], payoff[j], z[j], z[j], z[j+1] );
			std::cout << j << ";" << z[j] << ";" << undeflatedpayoff[j] << std::endl;
		}
		std::cout << "forward price = " << price << std::endl;

		//boost::shared_ptr<PricingEngine> jam(new GsrJamshidianSwaptionEngine(gsr));
		//boost::shared_ptr<PricingEngine> in(new GsrSwaptionEngine(gsr,512,25.0,false,false,yts,yts));

		//swaption->setPricingEngine(jam);
		//Real npvJam = swaption->NPV();
		////Real npvJam = 0.0;
		//swaption->setPricingEngine(in);
		//Real npvIn = swaption->NPV();
		//
		//std::cout << "T=" << T << " jam=" << npvJam << " in=" << npvIn <<  std::endl;

		//T+=5.0;
	}

	// TEST1

	//Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),0.03,Actual365Fixed())));
	//boost::shared_ptr<SwapIndex> swpIdx(new EuriborSwapIsdaFixA(20*Years,yts));
	//boost::shared_ptr<IborIndex> iborIdx(new Euribor(6*Months,yts));

	//std::vector<Date> expiries;
	//for(Size i=1;i<35;i++) {
	//	expiries.push_back(refDate+i*Years);
	//}
	//std::vector<Date> volstepdates(expiries);
	//volstepdates.pop_back();

	//boost::shared_ptr<VanillaSwap> underlying = swpIdx->underlyingSwap(refDate);
	//boost::shared_ptr<Exercise> exercise(new BermudanExercise(expiries));
	//boost::shared_ptr<Swaption> swaption(new Swaption(underlying,exercise));

	//boost::shared_ptr<Gsr> model(new Gsr(yts,volstepdates,std::vector<Real>(volstepdates.size()+1,0.01),0.01));
	////model->stateProcess()->enableCache();
	////boost::shared_ptr<Gsr> model(new Gsr(yts,std::vector<Date>(),std::vector<Real>(1,0.01),0.01));
	//boost::shared_ptr<PricingEngine> intEngine(new GsrSwaptionEngine(model,32,7.0));
	//swaption->setPricingEngine(intEngine);

	//BOOST_MESSAGE("NPV=" << swaption->NPV());

	//boost::shared_ptr<PricingEngine> jamEngine(new GsrJamshidianSwaptionEngine(model));
	//std::vector<boost::shared_ptr<CalibrationHelper>> basket;

	//for(Size i=1;i<35;i++) {
	//	boost::shared_ptr<CalibrationHelper> h(new SwaptionHelper(i*Years,(35-i)*Years,Handle<Quote>(new SimpleQuote(0.25)),iborIdx,1*Years,Thirty360(),Actual360(),yts));
	//	//h->setPricingEngine(jamEngine);
	//	h->setPricingEngine(intEngine);
	//	basket.push_back(h);
	//}


	//LevenbergMarquardt lm;
	//EndCriteria ec(2000,200,1E-8,1E-8,1E-8);
	//model->calibrate(basket,lm,ec);
	////model->calibrateIterative(basket,lm,ec);

	//BOOST_MESSAGE("Calibrated model params");
	//for(Size i=0;i<model->params().size();i++) {
	//	BOOST_MESSAGE(model->params()[i]);
	//}


	// TEST2

	//std::vector<Date> stepDates; // no step dates
	//std::vector<Real> vols(1,0.01);
	//std::vector<Real> reversions(1,0.01);

	//Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),0.03,Actual365Fixed())));
	//boost::shared_ptr<Gsr> model(new Gsr(yts,stepDates,vols,reversions,50.0));
	//boost::shared_ptr<HullWhite> hw(new HullWhite(yts,0.01,0.01));

	//Date expiry = TARGET().advance(refDate,5*Years);
	//Period tenor = 10*Years;
	//boost::shared_ptr<SwapIndex> swpIdx(new EuriborSwapIsdaFixA(tenor,yts));
	////boost::shared_ptr<SwapIndex> swpIdx(new SwapIndex("Artificial",tenor,42,swpIdx0->currency(),swpIdx0->fixingCalendar(),swpIdx0->fixedLegTenor(),swpIdx0->fixedLegConvention(),swpIdx0->dayCounter(),swpIdx0->iborIndex()));
	//Real forward = swpIdx->fixing(expiry);

	//boost::shared_ptr<VanillaSwap> underlying = swpIdx->underlyingSwap(expiry);
	//boost::shared_ptr<VanillaSwap> underlyingFixed = MakeVanillaSwap(10*Years,swpIdx->iborIndex(),forward)
	//	        .withEffectiveDate(swpIdx->valueDate(expiry))
 //               .withFixedLegCalendar(swpIdx->fixingCalendar())
 //               .withFixedLegDayCount(swpIdx->dayCounter())
 //               .withFixedLegTenor(swpIdx->fixedLegTenor())
 //               .withFixedLegConvention(swpIdx->fixedLegConvention())
 //               .withFixedLegTerminationDateConvention(swpIdx->fixedLegConvention())
	//			.receiveFixed(true); // receiver swap
	//boost::shared_ptr<Exercise> exercise(new EuropeanExercise(expiry));
	//boost::shared_ptr<Swaption> swaption(new Swaption(underlyingFixed,exercise));

	//swaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new GsrJamshidianSwaptionEngine(model)));
	//Real jamNpv = swaption->NPV();

	////swaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new GsrSwaptionEngine(model,256,8.0)));
	//Real intNpv = swaption->NPV();

	//Real modNpv = model->swaptionPrice(Option::Call,expiry,tenor,forward,swpIdx,Null<Date>(),0.0,256,8.0);

	//swaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new JamshidianSwaptionEngine(hw)));
	//Real hwNpv = swaption->NPV();

	////swaption->setPricingEngine(boost::shared_ptr<PricingEngine>(new TreeSwaptionEngine(hw,2048)));
	//Real hwNpv2 = swaption->NPV();

	//std::cout << "refDate = " << refDate << " epxiry date = " << expiry << " value date = " << swpIdx->valueDate(expiry) << " forward = " << forward << std::endl;
	//BOOST_MESSAGE(std::setprecision(12) << hwNpv << ";" << jamNpv << ";" << hwNpv2 << ";" << intNpv);

	Settings::instance().evaluationDate() = saveDate;

}


test_suite* GsrTest::suite() {
    test_suite* suite = BOOST_TEST_SUITE("GSR model tests");
	//suite->add(QUANTLIB_TEST_CASE(&GsrTest::testGsrProcess));
	//suite->add(QUANTLIB_TEST_CASE(&GsrTest::testGsrModel));
	//suite->add(QUANTLIB_TEST_CASE(&GsrTest::testNonstandardSwaption));
	suite->add(QUANTLIB_TEST_CASE(&GsrTest::testDummy));
	return suite;
}

