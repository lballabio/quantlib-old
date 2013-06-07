#include <cmsFundTrust.hpp>
#include <stdio.h>

namespace QuantLib {

	using namespace cmsFundTrustHelper;
	using namespace QuantExt;

	CmsFundTrust::CmsFundTrust(const boost::shared_ptr<YieldTermStructure> yts, 
		const boost::shared_ptr<SwaptionVolatilityStructure> swVol, const double meanRev, const double cmsMeanRev) :
		yts_(yts), swVol_(swVol), meanRev_(meanRev), cmsMeanRev_(cmsMeanRev)
	{
		calculate();
	}

	vector<double> CmsFundTrust::result(string tag) {
		map<string,vector<double>>::iterator iter;
		iter=results_.find(tag);
		if(iter==results_.end()) QL_FAIL("Result with tag " << tag << " does not exist.");
		return (*iter).second;
	}

	void CmsFundTrust::calculate() {

		boost::shared_ptr<IborIndex> iborIndex(new Euribor6M(Handle<YieldTermStructure>(yts_)));
		
		// CMS Swaption

		// Create the CMS coupons.    
		Date startDate(29, July, 2033);
		Date maturity(29, July, 2046);
		Date start, end, pay;
		vector<boost::shared_ptr<CmsCoupon> > cmsCoupons;
		vector<boost::shared_ptr<CappedFlooredCmsCoupon> > cmsCapFloorCoupons;
		boost::shared_ptr<SwapIndex> swapIndex(new EuriborSwapIsdaFixA(10*Years, iborIndex->forwardingTermStructure()));
		Real nominal = 4.0e8;	
		Natural cmsFixDays = 2;
		Real gearing = 1.0;
		Spread spread = 0.0005;
		Rate cap = 0.09;
		Rate tempCap = Null<Rate>();
		Rate floor = 0.0;
		Rate tempFloor = Null<Rate>();
		DayCounter cmsLegBasis = Thirty360(Thirty360::European);
		DayCounter floatBasis = Actual360();
		bool isArrears = false;
		vector<Date> volStepDates;
		vector<Real> fairRates,fairRates1;
		
		BusinessDayConvention cmsPayConv = ModifiedFollowing;
		Calendar cmsLegCal = TARGET();
		
		for(int i = 0; i < 26; ++i) {
			start = startDate + (i*6)*Months;
			end = start + 6*Months;
			pay = cmsLegCal.adjust(end, cmsPayConv);
			cmsCoupons.push_back(boost::shared_ptr<CmsCoupon>(new CmsCoupon(pay, nominal, start, end, 
				cmsFixDays, swapIndex, gearing, spread, start, end, cmsLegBasis, isArrears)));
			cmsCapFloorCoupons.push_back(boost::shared_ptr<CappedFlooredCmsCoupon>(new CappedFlooredCmsCoupon(pay, nominal, 
				start, end, cmsFixDays, swapIndex, gearing, spread, cap, floor, start, end, cmsLegBasis, isArrears)));
			
			// For consistent vol term structure dates.
			Date aFixingDate = cmsCoupons[i]->fixingDate();
			boost::shared_ptr<SwapIndex> aSwapIndex = cmsCoupons[i]->swapIndex();
			boost::shared_ptr<VanillaSwap> aVanillaSwap = aSwapIndex->underlyingSwap(aFixingDate);
			Schedule uFixedSched = aVanillaSwap->fixedSchedule();
			Date uSwapStart = uFixedSched.startDate();
			volStepDates.push_back(uSwapStart);
			fairRates.push_back(aVanillaSwap->fairRate());
			fairRates1.push_back(aVanillaSwap->fairRate());
		}
		
		vector<Real> cmsCouponRates(cmsCoupons.size(), 0.0);
		vector<Real> cfCmsCouponRates(cmsCapFloorCoupons.size(), 0.0);
		
		// Price the CMS coupons
		Handle<Quote> cmsMeanRev(boost::shared_ptr<Quote>(new SimpleQuote(cmsMeanRev_)));
		boost::shared_ptr<CmsCouponPricer> pricer1(new NumericHaganPricer(Handle<SwaptionVolatilityStructure>(swVol_), GFunctionFactory::NonParallelShifts, cmsMeanRev));
		boost::shared_ptr<CmsCouponPricer> pricer2(new AnalyticHaganPricer(Handle<SwaptionVolatilityStructure>(swVol_), GFunctionFactory::NonParallelShifts, cmsMeanRev));

		Real price = 0.0, priceCF = 0.0;
		//pricer1->setSwaptionVolatility(swVol_);
		//pricer2->setSwaptionVolatility(swVol_);
		vector<boost::shared_ptr<CmsCouponPricer> > pricers(2);
		pricers[0] = pricer1;
		pricers[1] = pricer2;
		
		// Loop over periods.
		/*cout << "------ SEPERATE COUPON PRICING ------" << endl;
		cout << "fix,start,end,pmt,dcf,fair_swap_rate,cms_rate,cms_cf_rate,"
			 << "cms_price,cms_cf_price,cms_amount,cms_cf_amount,df_pmt" << endl;
		*/

		vector<double> fixingDates,startDates,endDates,pmtDates,cmsRates, cfCmsRates, cmsPrice, cfCmsPrice, cmsAmount, cfCmsAmount, df;

		for(Size i = 0; i < cmsCoupons.size(); ++i) {
			
			cmsCoupons[i]->setPricer(pricers[0]);
			cmsCapFloorCoupons[i]->setPricer(pricers[0]);
			cmsCouponRates[i] = cmsCoupons[i]->rate();
			cfCmsCouponRates[i] = cmsCapFloorCoupons[i]->rate();
			
			price = cmsCoupons[i]->price(Handle<YieldTermStructure>(yts_));
			priceCF = cmsCapFloorCoupons[i]->price(Handle<YieldTermStructure>(yts_));
			
			fixingDates.push_back((double)cmsCoupons[i]->fixingDate().serialNumber());
			startDates.push_back((double)cmsCoupons[i]->accrualStartDate().serialNumber());
			endDates.push_back((double)cmsCoupons[i]->accrualEndDate().serialNumber());
			pmtDates.push_back((double)cmsCoupons[i]->date().serialNumber());
			cmsRates.push_back(cmsCoupons[i]->rate());
			cfCmsRates.push_back(cmsCapFloorCoupons[i]->rate());
			cmsPrice.push_back(price);
			cfCmsPrice.push_back(priceCF);
			cmsAmount.push_back(cmsCoupons[i]->amount());
			cfCmsAmount.push_back(cmsCapFloorCoupons[i]->amount());
			df.push_back(yts_->discount(cmsCoupons[i]->date()));
			/*cout << io::iso_date(cmsCoupons[i]->fixingDate()) << "," 
				 << io::iso_date(cmsCoupons[i]->accrualStartDate()) << ","
				 << io::iso_date(cmsCoupons[i]->accrualEndDate()) << ","
				 << io::iso_date(cmsCoupons[i]->date()) << ","
				 << cmsCoupons[i]->accrualPeriod() << ","
				 << fairRates[i] << ","
				 << io::rate(cmsCoupons[i]->rate()) << ","
				 << io::rate(cmsCapFloorCoupons[i]->rate()) << ","
				 << price << "," << priceCF << ","
				 << cmsCoupons[i]->amount() << ","
				 << cmsCapFloorCoupons[i]->amount() << ","
				 << vars.termStructure->discount(cmsCoupons[i]->date()) << endl;*/
		}
		//cout << endl;
		results_.insert(pair<string,vector<double>>("SwaptionFixingDates",fixingDates));
		results_.insert(pair<string,vector<double>>("SwaptionStartDates",startDates));
		results_.insert(pair<string,vector<double>>("SwaptionEndDates",endDates));
		results_.insert(pair<string,vector<double>>("SwaptionPayDates",pmtDates));
		results_.insert(pair<string,vector<double>>("SwaptionSwapRates",fairRates1));
		results_.insert(pair<string,vector<double>>("SwaptionCmsRates",cmsRates));
		results_.insert(pair<string,vector<double>>("SwaptionCfCmsRates",cfCmsRates));
		results_.insert(pair<string,vector<double>>("SwaptionCmsPrice",cmsPrice));
		results_.insert(pair<string,vector<double>>("SwaptionCfCmsPrice",cfCmsPrice));
		results_.insert(pair<string,vector<double>>("SwaptionCmsAmounts",cmsAmount));
		results_.insert(pair<string,vector<double>>("SwaptionCfCmsAmounts",cfCmsAmount));
		results_.insert(pair<string,vector<double>>("SwaptionDiscountFactors",df));

		
		// Set up a CMS swap and value it.
		Period cmslegFreq(6, Months);
		Period floatlegFreq(3, Months);
		boost::shared_ptr<IborIndex> floatIndex(new Euribor3M(Handle<YieldTermStructure>(yts_)));
		Spread floatSpread = 0.0035; // correct spread
		
		Schedule cmsSchedule(startDate, maturity, cmslegFreq, cmsLegCal, Unadjusted, Unadjusted,
							 DateGeneration::Backward, false);
		
		Schedule floatSchedule(startDate, maturity, floatlegFreq, cmsLegCal, cmsPayConv, cmsPayConv,
							   DateGeneration::Backward, false);
		
		Leg cmsLeg = CmsLeg(cmsSchedule, swapIndex)
			.withNotionals(nominal)
			.withPaymentDayCounter(cmsLegBasis)
			.withPaymentAdjustment(cmsPayConv)
			.withFixingDays(swapIndex->fixingDays())
			.withGearings(gearing)
			.withSpreads(spread)
			.withCaps(cap)
			.withFloors(floor);
		
		setCouponPricer(cmsLeg, pricers[0]);
		
		Leg floatLeg = IborLeg(floatSchedule, floatIndex)
			.withNotionals(nominal)
			.withPaymentDayCounter(floatBasis)
			.withPaymentAdjustment(cmsPayConv)
			.withFixingDays(floatIndex->fixingDays())
			.withSpreads(floatSpread);
		
		boost::shared_ptr<Swap> aCmsSwap(new Swap(cmsLeg, floatLeg));
		
		boost::shared_ptr<PricingEngine> swapEngine(new DiscountingSwapEngine(Handle<YieldTermStructure>(yts_)));
		aCmsSwap->setPricingEngine(swapEngine);
		
		/*cout << "------ PRICE AS SWAP WITH HAGAN ------" << endl;
		cout << "Pay leg (CMS):\t" << aCmsSwap->legNPV(0) << endl;
		cout << "Rec leg (3ME):\t" << aCmsSwap->legNPV(1) << endl;
		cout << "Swap NPV:\t" << aCmsSwap->legNPV(1) + aCmsSwap->legNPV(0) << endl << endl;
		*/

		vector<double> legNpvs;

		legNpvs.push_back(aCmsSwap->legNPV(0));
		legNpvs.push_back(aCmsSwap->legNPV(1));
		legNpvs.push_back(aCmsSwap->legNPV(0)+aCmsSwap->legNPV(1));

		results_.insert(pair<string,vector<double>>("SwaptionLegNpvs",legNpvs));
		
		// Calculate the cms coupon prices using hull white with an integral.
		vector<Real> sigmas(volStepDates.size(), 0.01);
		
		Brent solveVols;
		Real solveAcc = 1.0e-4, guess = 0.01;
		Real min = 1.0e-4, max = (1 + 1.0e-4);
		vector<Date> tempVolDates;
		vector<Real> tempSigmas;
		boost::shared_ptr<QuantExt::HullWhite1> hw1(new QuantExt::HullWhite1(Handle<YieldTermStructure>(yts_)));
		hw1->setSpeed(meanRev_);
		
		for(Size i = 0; i < volStepDates.size(); ++i) {
			// Set up the function for the solver.
			tempVolDates.push_back(volStepDates[i]);
			boost::function<Real (Real)> bSolveVolFunc;
			bSolveVolFunc = boost::bind(&solveVolFunc, hw1, cmsCoupons[i], tempFloor, tempFloor, Handle<YieldTermStructure>(yts_), 
				cmsCouponRates[i], tempVolDates, tempSigmas, _1);
			try {
				guess = solveVols.solve(bSolveVolFunc, solveAcc, guess, min, max);
			} catch (QuantLib::Error & e) {
				cout << "QuantLib Exception: "<< e.what() << endl;
			}
			
			sigmas[i] = guess;
			tempSigmas.push_back(guess);
		}
		
		// Check the integrals.
		Real integralValue = 0.0;
		vector<double> hwDates, hwSigma, hwCmsRateIntegral; 
		cout << "------ CHECK THE CMS RATES FROM INTEGRAL MATCH HAGAN CMS ------" << endl;
		cout << "vol_step_date,hw_volatility,cf_cms_rate_hagan,cf_cms_rate_integral,difference" << endl;
		for(Size i = 0; i < cmsCoupons.size(); ++i) { 
			integralValue = cmsIntegralValue(hw1, cmsCoupons[i], tempCap, tempFloor, Handle<YieldTermStructure>(yts_), volStepDates, sigmas);
			/* cout << io::iso_date(volStepDates[i]) << "," << sigmas[i] << "," << cmsCouponRates[i] 
				 << "," << integralValue << "," <<(cmsCouponRates[i] - integralValue) << endl; */
			hwDates.push_back((double)volStepDates[i].serialNumber());
			hwSigma.push_back(sigmas[i]);
			hwCmsRateIntegral.push_back(integralValue);
		}
		//cout << endl;

		results_.insert(pair<string,vector<double>>("SwaptionHwDates",hwDates));
		results_.insert(pair<string,vector<double>>("SwaptionHwSigmas",hwSigma));
		results_.insert(pair<string,vector<double>>("SwaptionHwCmsRates",hwCmsRateIntegral));
		
		// Value the CMS swap using a tree based on the model above.
		//	Real tempGearing = 0.000000001;
		
		vector<boost::shared_ptr<CapFloorCmsSwap> > vCfCmsSwaps;
		boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_0(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, 
			cmsSchedule, swapIndex, gearing, spread, cap, floor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
			floatIndex, 1.0, floatSpread, floatBasis, cmsPayConv));
	//	boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_1(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, 
	//		cmsSchedule, swapIndex, gearing, spread, cap, tempFloor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
	//		floatIndex, 1.0, floatSpread, floatBasis, cmsPayConv));
	//	boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_2(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, 
	//		cmsSchedule, swapIndex, gearing, spread, tempCap, floor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
	//		floatIndex, 1.0, floatSpread, floatBasis, cmsPayConv));
	//	boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_3(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, 
	//		cmsSchedule, swapIndex, gearing, spread, tempCap, tempFloor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
	//		floatIndex, 1.0, floatSpread, floatBasis, cmsPayConv));
	//	boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_4(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, cmsSchedule, 
	//		swapIndex, tempGearing, 0.0, tempCap, tempFloor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
	//		floatIndex, 1.0, floatSpread, floatBasis, cmsPayConv));
	//	boost::shared_ptr<CapFloorCmsSwap> cfCmsSwap_5(new CapFloorCmsSwap(CapFloorCmsSwap::Payer, nominal, cmsSchedule, 
	//		swapIndex, gearing, spread, tempCap, tempFloor, cmsLegBasis, cmsPayConv, false, floatSchedule, 
	//		floatIndex, tempGearing, 0.0, floatBasis, cmsPayConv));
			
		vCfCmsSwaps.push_back(cfCmsSwap_0);
	//	vCfCmsSwaps.push_back(cfCmsSwap_1);
	//	vCfCmsSwaps.push_back(cfCmsSwap_2);
	//	vCfCmsSwaps.push_back(cfCmsSwap_3);
	//	vCfCmsSwaps.push_back(cfCmsSwap_4);
	//	vCfCmsSwaps.push_back(cfCmsSwap_5);
		
	//	boost::shared_ptr<PricingEngine> swaEngine(new TreeCapFloorCmsSwapEngine(hw1,540));
	//	
	//	vector<Real> swapPrices(vCfCmsSwaps.size(), 0.0);
	//	for(Size i = 0; i < vCfCmsSwaps.size(); i++) {
	//		vCfCmsSwaps[i]->setPricingEngine(swaEngine);
	//		swapPrices[i] = vCfCmsSwaps[i]->NPV();
	//	}
		
	//	cout << "------ VALUE THE UNDERLYING SWAPS ON THE TREE ------" << endl;
	//	cout << "Swap NPV (w cap, w floor):\t" << swapPrices[0] << endl;
	//	cout << "Swap NPV (w cap, w/o floor):\t" << swapPrices[1] << endl;
	//	cout << "Swap NPV (w/o cap, w floor):\t" << swapPrices[2] << endl;
	//	cout << "Swap NPV (w/o cap, w/o floor):\t" << swapPrices[3] << endl;
	//	cout << "Float Leg NPV:\t" << swapPrices[4] << endl;
	//	cout << "CMS Leg NPV:\t" << swapPrices[5] << endl << endl;
		
		// Create a swaption and value on a tree.
		Settlement::Type settlementType = Settlement::Physical;
		Date exercise(27, June, 2033);
		boost::shared_ptr<Exercise> bspExercise(new EuropeanExercise(exercise));
		
		std::vector<boost::shared_ptr<CapFloorCmsSwaption> > vCfCmsSwaptions;
		boost::shared_ptr<CapFloorCmsSwaption> cfCmsSwaption_0(new CapFloorCmsSwaption(vCfCmsSwaps[0], 
			bspExercise, settlementType));
	//	boost::shared_ptr<CapFloorCmsSwaption> cfCmsSwaption_1(new CapFloorCmsSwaption(vCfCmsSwaps[1], 
	//		bspExercise, settlementType));
	//	boost::shared_ptr<CapFloorCmsSwaption> cfCmsSwaption_2(new CapFloorCmsSwaption(vCfCmsSwaps[2], 
	//		bspExercise, settlementType));
	//	boost::shared_ptr<CapFloorCmsSwaption> cfCmsSwaption_3(new CapFloorCmsSwaption(vCfCmsSwaps[3], 
	//		bspExercise, settlementType));
		
		vCfCmsSwaptions.push_back(cfCmsSwaption_0);
	//	vCfCmsSwaptions.push_back(cfCmsSwaption_1);
	//	vCfCmsSwaptions.push_back(cfCmsSwaption_2);
	//	vCfCmsSwaptions.push_back(cfCmsSwaption_3);
		
		boost::shared_ptr<PricingEngine> swoEngine(new TreeCapFloorCmsSwaptionEngine(hw1,540));
		
		vector<Real> swaptionPrices(vCfCmsSwaptions.size(), 0.0);
		for(Size i = 0; i < vCfCmsSwaptions.size(); i++) {
			vCfCmsSwaptions[i]->setPricingEngine(swoEngine);
			swaptionPrices[i] = vCfCmsSwaptions[i]->NPV();
		}

		vector<double> swaptionNpv;
		swaptionNpv.push_back(swaptionPrices[0]);
		results_.insert(pair<string,vector<double>>("SwaptionHwNpv",swaptionNpv));

		
	//	cout << "------ VALUE THE UNDERLYING SWAPTIONS ON THE TREE ------" << endl;
	//	cout << "Swaption NPV (w cap, w floor):\t" << swaptionPrices[0] << endl;
	//	cout << "Swaption NPV (w cap, w/o floor):\t" << swaptionPrices[1] << endl;
	//	cout << "Swaption NPV (w/o cap, w floor):\t" << swaptionPrices[2] << endl;
	//	cout << "Swaption NPV (w/o cap, w/o floor):\t" << swaptionPrices[3] << endl << endl;
		
		// Value a payer swaption with strike of 9% and .5% to get bounds.
	//	std::vector<Real> bounds(2, 0.0);
	//	Real upperStrike = 0.005, lowerStrike = 0.09;
	//	Real upperVol = vars.volCube->volatility(startDate, 13*Years, upperStrike);
	//	Real lowerVol = vars.volCube->volatility(startDate, 13*Years, lowerStrike);
	//	
	//	boost::shared_ptr<VanillaSwap> upperSwap = MakeVanillaSwap(13*Years, floatIndex, 0.005)
	//		.withEffectiveDate(startDate)
	//		.withFloatingLegSpread(0.0)
	//		.withType(VanillaSwap::Payer)
	//		.withNominal(nominal);
	//	
	//	boost::shared_ptr<Swaption> upperSwaption(new Swaption(upperSwap, bspExercise, settlementType));
	//	boost::shared_ptr<PricingEngine> bsEngine(new BlackSwaptionEngine(vars.termStructure, upperVol));
	//	upperSwaption->setPricingEngine(bsEngine);
	//	bounds[0] = upperSwaption->NPV();
	//	
	//	boost::shared_ptr<VanillaSwap> lowerSwap = MakeVanillaSwap(13*Years, floatIndex, 0.09)
	//		.withEffectiveDate(startDate)
	//		.withFloatingLegSpread(0.0)
	//		.withType(VanillaSwap::Payer)
	//		.withNominal(nominal);
	//	
	//	boost::shared_ptr<Swaption> lowerSwaption(new Swaption(lowerSwap, bspExercise, settlementType));	
	//	bsEngine.reset(new BlackSwaptionEngine(vars.termStructure, lowerVol));
	//	lowerSwaption->setPricingEngine(bsEngine);
	//	bounds[1] = lowerSwaption->NPV();
	//	
	//	cout << "Upper bound:\t" << bounds[0] << " with vol " << upperVol << endl;
	//	cout << "Lower bound:\t" << bounds[1] << " with vol " << lowerVol << endl << endl;


	// Value the swap 5595

	//	Date firstFixDate(27, January, 2011);
	//	TimeSeries<Real> pastCmsFixings;
	//	pastCmsFixings[firstFixDate] = 0.03429;
	//	IndexManager::instance().setHistory(swapIndex->name(), pastCmsFixings);
	//	TimeSeries<Real> pastFloatFixings;
	//	pastFloatFixings[firstFixDate] = 0.01057;
	//	IndexManager::instance().setHistory(floatIndex->name(), pastFloatFixings);
	//	TimeSeries<Real> pastSixMonthFixings;
	//	pastSixMonthFixings[firstFixDate] = 0.01301;
	//	IndexManager::instance().setHistory(vars.iborIndex->name(), pastSixMonthFixings);
	//	

		// Swap 5610 and 5595

		Date startSwap(29, January, 2011);
		Date endSwap(29, July, 2034);
		nominal = 1.0e8; //nominals for 5610. this is taken for 5595 also, must be multiplied by 3 !!
		floatSpread = 0.0086; double floatSpread2 = 0.0089; //spreads for 5610 and 5595 resp.
		cmsCoupons.clear();
		cmsCapFloorCoupons.clear();
		fairRates.clear();
		vector<double> fairRates2;

		vector<double> fixingDates2,startDates2,endDates2,pmtDates2,cmsRates2, cfCmsRates2, cmsPrice2, cfCmsPrice2, cmsAmount2, cfCmsAmount2, df2;

	//	
		for(int i = 0; i < 47; ++i) {
			start = startSwap + (i*6)*Months;
			end = start + 6*Months;
			pay = cmsLegCal.adjust(end, cmsPayConv);
			cmsCoupons.push_back(boost::shared_ptr<CmsCoupon>(new CmsCoupon(pay, nominal, start, end, 
				cmsFixDays, swapIndex, gearing, spread, start, end, cmsLegBasis, isArrears)));
			cmsCapFloorCoupons.push_back(boost::shared_ptr<CappedFlooredCmsCoupon>(new CappedFlooredCmsCoupon(pay, nominal, 
				start, end, cmsFixDays, swapIndex, gearing, spread, cap, floor, start, end, cmsLegBasis, isArrears)));
	//		
	//		// Fair swap rates.
			Date aFixingDate = cmsCoupons[i]->fixingDate();
			boost::shared_ptr<SwapIndex> aSwapIndex = cmsCoupons[i]->swapIndex();
			boost::shared_ptr<VanillaSwap> aVanillaSwap = aSwapIndex->underlyingSwap(aFixingDate);
			fairRates.push_back(aVanillaSwap->fairRate());
			fairRates2.push_back(aVanillaSwap->fairRate());
		}
	//	
		cmsCouponRates.clear();
		cmsCouponRates.resize(cmsCoupons.size());
		cfCmsCouponRates.clear();
		cfCmsCouponRates.resize(cmsCapFloorCoupons.size());
	//	
	//	// Price the CMS coupons
		price = 0.0; priceCF = 0.0;
	//	
	//	// Loop over periods.
	//	cout << "------ SEPERATE COUPON PRICING 5595 ------" << endl;
	//	cout << "fix,start,end,pmt,dcf,fair_swap_rate,cms_rate,cms_cf_rate,"
	//		 << "cms_price,cms_cf_price,cms_amount,cms_cf_amount,df_pmt" << endl;
		/*for(Size i = 0; i < cmsCoupons.size(); ++i) {
	//		
			cmsCoupons[i]->setPricer(pricers[0]);
			cmsCapFloorCoupons[i]->setPricer(pricers[0]);
			cmsCouponRates[i] = cmsCoupons[i]->rate();
			cfCmsCouponRates[i] = cmsCapFloorCoupons[i]->rate();
	//		
			price = cmsCoupons[i]->price(Handle<YieldTermStructure>(yts_));
			priceCF = cmsCapFloorCoupons[i]->price(Handle<YieldTermStructure>(yts_));

			fixingDates2.push_back(cmsCoupons[i]->fixingDate().serialNumber());
			startDates2.push_back(cmsCoupons[i]->accrualStartDate().serialNumber());
			endDates2.push_back(cmsCoupons[i]->accrualEndDate().serialNumber());
			pmtDates2.push_back(cmsCoupons[i]->date().serialNumber());
			cmsRates2.push_back(cmsCoupons[i]->rate());
			cfCmsRates2.push_back(cmsCapFloorCoupons[i]->rate());
			cmsPrice2.push_back(price);
			cfCmsPrice2.push_back(priceCF);
			cmsAmount2.push_back(cmsCoupons[i]->amount());
			cfCmsAmount2.push_back(cmsCapFloorCoupons[i]->amount());
			df2.push_back(yts_->discount(cmsCoupons[i]->date()));
	//		cout << io::iso_date(cmsCoupons[i]->fixingDate()) << "," 
	//			 << io::iso_date(cmsCoupons[i]->accrualStartDate()) << ","
	//			 << io::iso_date(cmsCoupons[i]->accrualEndDate()) << ","
	//			 << io::iso_date(cmsCoupons[i]->date()) << ","
	//			 << cmsCoupons[i]->accrualPeriod() << ","
	//			 << fairRates[i] << ","
	//			 << io::rate(cmsCoupons[i]->rate()) << ","
	//			 << io::rate(cmsCapFloorCoupons[i]->rate()) << ","
	//			 << price << "," << priceCF << ","
	//			 << cmsCoupons[i]->amount() << ","
	//			 << cmsCapFloorCoupons[i]->amount() << ","
	//			 << vars.termStructure->discount(cmsCoupons[i]->date()) << endl;
		}
	//	cout << endl;

		results_.insert(pair<string,vector<double>>("SwapFixingDates",fixingDates2));
		results_.insert(pair<string,vector<double>>("SwapStartDates",startDates2));
		results_.insert(pair<string,vector<double>>("SwapEndDates",endDates2));
		results_.insert(pair<string,vector<double>>("SwapPayDates",pmtDates2));
		results_.insert(pair<string,vector<double>>("SwapSwapRates",fairRates2));
		results_.insert(pair<string,vector<double>>("SwapCmsRates",cmsRates2));
		results_.insert(pair<string,vector<double>>("SwapCfCmsRates",cfCmsRates2));
		results_.insert(pair<string,vector<double>>("SwapCmsPrice",cmsPrice2));
		results_.insert(pair<string,vector<double>>("SwapCfCmsPrice",cfCmsPrice2));
		results_.insert(pair<string,vector<double>>("SwapCmsAmounts",cmsAmount2));
		results_.insert(pair<string,vector<double>>("SwapCfCmsAmounts",cfCmsAmount2));
		results_.insert(pair<string,vector<double>>("SwapDiscountFactors",df2));
		*/

	//	
	//	// Set up the swap.
		Schedule cmsSwapSched(startSwap, endSwap, cmslegFreq, cmsLegCal, Unadjusted, Unadjusted,
			DateGeneration::Backward, false);
	//	
		Schedule floatSwapSched(startSwap, endSwap, floatlegFreq, cmsLegCal, cmsPayConv, cmsPayConv,
			DateGeneration::Backward, false);
	//	
		Leg cmsSwapLeg = CmsLeg(cmsSwapSched, swapIndex)
			.withNotionals(nominal)
			.withPaymentDayCounter(cmsLegBasis)
			.withPaymentAdjustment(cmsPayConv)
			.withFixingDays(swapIndex->fixingDays())
			.withGearings(gearing)
			.withSpreads(spread)
			.withCaps(cap)
			.withFloors(floor);
	//	
		setCouponPricer(cmsSwapLeg, pricers[0]);
	//	
		// float leg 5610
		Leg floatSwapLeg = IborLeg(floatSwapSched, floatIndex)
			.withNotionals(nominal)
			.withPaymentDayCounter(floatBasis)
			.withPaymentAdjustment(cmsPayConv)
			.withFixingDays(floatIndex->fixingDays())
			.withSpreads(floatSpread);
		
		// float leg 5595
		Leg floatSwapLeg2 = IborLeg(floatSwapSched, floatIndex)
			.withNotionals(nominal)
			.withPaymentDayCounter(floatBasis)
			.withPaymentAdjustment(cmsPayConv)
			.withFixingDays(floatIndex->fixingDays())
			.withSpreads(floatSpread2);

	//	value 5610

		aCmsSwap.reset(new Swap(floatSwapLeg, cmsSwapLeg));
		aCmsSwap->setPricingEngine(swapEngine);
	//	
	//	cout << "------ PRICE AS SWAP WITH HAGAN ------" << endl;
	//	cout << "Pay leg (3ME):\t" << aCmsSwap->legNPV(0) << endl;
	//	cout << "Rec leg (CMS):\t" << aCmsSwap->legNPV(1) << endl;
	//	cout << "Swap NPV:\t" << aCmsSwap->legNPV(1) + aCmsSwap->legNPV(0) << endl << endl;
		
	//	cout << endl << "CMS demo case done" << endl;

		vector<double> swapNpvs;
		swapNpvs.push_back(aCmsSwap->legNPV(0));
		swapNpvs.push_back(aCmsSwap->legNPV(1));
		swapNpvs.push_back(aCmsSwap->legNPV(0)+aCmsSwap->legNPV(1));

		results_.insert(pair<string,vector<double>>("SwapLegNpvs",swapNpvs));

	//	value 5595

		aCmsSwap.reset(new Swap(floatSwapLeg2, cmsSwapLeg));
		aCmsSwap->setPricingEngine(swapEngine);
	//	
	//	cout << "------ PRICE AS SWAP WITH HAGAN ------" << endl;
	//	cout << "Pay leg (3ME):\t" << aCmsSwap->legNPV(0) << endl;
	//	cout << "Rec leg (CMS):\t" << aCmsSwap->legNPV(1) << endl;
	//	cout << "Swap NPV:\t" << aCmsSwap->legNPV(1) + aCmsSwap->legNPV(0) << endl << endl;
		
	//	cout << endl << "CMS demo case done" << endl;

		vector<double> swapNpvs2;
		swapNpvs2.push_back(aCmsSwap->legNPV(0));
		swapNpvs2.push_back(aCmsSwap->legNPV(1));
		swapNpvs2.push_back(aCmsSwap->legNPV(0)+aCmsSwap->legNPV(1));

		results_.insert(pair<string,vector<double>>("Swap2LegNpvs",swapNpvs2));


	}

	namespace cmsFundTrustHelper {

		Real cmsSwapletIntegrand(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon, 
			Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, Real r){
			
			Real result = 0.0;
			Time dcf = 0.0;
			Time timeToDate = 0.0;
			Date today = Settings::instance().evaluationDate();
			DayCounter basisForTime = Actual365Fixed(); // should be replaced
			Date cmsPmtDate = aCmsCoupon->date();
			Time timeToPmt = basisForTime.yearFraction(today, cmsPmtDate);
			
			// Get the underlying swap fixed dates and dcb.
			Date aFixingDate = aCmsCoupon->fixingDate();
			boost::shared_ptr<SwapIndex> aSwapIndex = aCmsCoupon->swapIndex();
			boost::shared_ptr<VanillaSwap> aVanillaSwap = aSwapIndex->underlyingSwap(aFixingDate);
			Schedule uFixedSched = aVanillaSwap->fixedSchedule();
			Date uSwapStart = uFixedSched.startDate();
			Time timeSwapStart = basisForTime.yearFraction(today, uSwapStart);
			Date uSwapEnd = uFixedSched.endDate();
			Time timeSwapEnd = basisForTime.yearFraction(today, uSwapEnd);
			DayCounter fixedDCB = aVanillaSwap->fixedDayCount();
			
			// Form the integrand without the normal density.
			for(Size i = 0; i < uFixedSched.size() - 1; ++i){
				dcf = fixedDCB.yearFraction(uFixedSched[i], uFixedSched[i+1]);
				timeToDate = basisForTime.yearFraction(today, uFixedSched[i+1]);
				result += dcf * aHw1->discountBond(timeSwapStart, timeToDate, r);
			}
			result = (1 - aHw1->discountBond(timeSwapStart, timeSwapEnd, r)) / result;
			
			// Account for gearing, spread, cap and floor.
			result *= aCmsCoupon->gearing();
			result += aCmsCoupon->spread();
			
			if(cmsCap != Null<Rate>()) {
				result = min(result, cmsCap);
			}
			
			if(cmsFloor != Null<Rate>()) {
				result = max(result, cmsFloor);
			}
			
			result *= aHw1->discountBond(timeSwapStart, timeToPmt, r);
			
			// Form the normal density and multiply.
			Real mean = aTermStructure->forwardRate(timeSwapStart, timeSwapStart, Continuous, NoFrequency);
			Real stdDev = sqrt(aHw1->spotRateVariance(timeSwapStart));
			boost::math::normal shortRateDist(mean, stdDev);
			result *= pdf(shortRateDist, r);
			
			return result;
		}

		
		Real cmsIntegralValue(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon,
				Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, 
				vector<Date> & volDates, vector<Real> & sigmas){
				
				Date today = Settings::instance().evaluationDate();
				DayCounter basisForTime = Actual365Fixed(); // should be replaced
				aHw1->setVolatilityStructure(volDates, sigmas);
				
				// Calculate the bounds for the integration.
				Real a, b, result;
				Real funcAcc = 1.0e-8;
				Real intAcc = 1.0e-4;
				Size maxEval = 1000;
				
				boost::function<Real (Real)> cmsIntegrand;
				cmsIntegrand = boost::bind(&cmsSwapletIntegrand, aHw1, aCmsCoupon, cmsCap, cmsFloor, aTermStructure, _1);
				
				Date aFixingDate = aCmsCoupon->fixingDate();

				boost::shared_ptr<SwapIndex> aSwapIndex = aCmsCoupon->swapIndex();
				boost::shared_ptr<VanillaSwap> aVanillaSwap = aSwapIndex->underlyingSwap(aFixingDate);
				Schedule uFixedSched = aVanillaSwap->fixedSchedule();
				Date uSwapStart = uFixedSched.startDate();
				Time timeSwapStart = basisForTime.yearFraction(today, uSwapStart);
				Real instFwd = aTermStructure->forwardRate(timeSwapStart, timeSwapStart, Continuous, NoFrequency);
				a = b = instFwd;
				
				// Upper bound.
				while (cmsIntegrand(b) > funcAcc) b *= 2.0;
				// Lower bound.
				while(cmsIntegrand(a) > funcAcc){
					if(a >= 0)
						a -= (b - instFwd);
					else
						a *= 2.0;
				}
				
				// Set up and do the integral.
				SimpsonIntegral aSimpsonInt(intAcc, maxEval);
				result = aSimpsonInt(cmsIntegrand, a, b);
				return result;
			}

		Real solveVolFunc(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon,
				Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, Real aCmsRate, vector<Date> & volDates, 
				vector<Real> sigmas, Real aVol){
				
				sigmas.push_back(aVol);
				Real result = cmsIntegralValue(aHw1, aCmsCoupon, cmsCap, cmsFloor, aTermStructure, volDates, sigmas) - aCmsRate;
				return result;
		}
		
	}
	
}
