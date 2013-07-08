/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006 Klaus Spanderen

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

#include "libormarketmodel.hpp"
//#include "utilities.hpp"
#include <iostream>

#include <ql/indexes/ibor/euribor.hpp>
#include <ql/instruments/capfloor.hpp>
#include <ql/termstructures/yield/zerocurve.hpp>
#include <ql/termstructures/volatility/optionlet/capletvariancecurve.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>

#include <ql/math/statistics/generalstatistics.hpp>
#include <ql/math/randomnumbers/rngtraits.hpp>
#include <ql/methods/montecarlo/multipathgenerator.hpp>

#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/pricingengines/capfloor/blackcapfloorengine.hpp>
#include <ql/pricingengines/capfloor/analyticcapfloorengine.hpp>

#include <ql/models/shortrate/calibrationhelpers/caphelper.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>

#include <ql/legacy/libormarketmodels/lfmcovarproxy.hpp>
#include <ql/legacy/libormarketmodels/lmexpcorrmodel.hpp>
#include <ql/legacy/libormarketmodels/lmlinexpcorrmodel.hpp>
#include <ql/legacy/libormarketmodels/lmfixedvolmodel.hpp>
#include <ql/legacy/libormarketmodels/lmextlinexpvolmodel.hpp>
#include <ql/legacy/libormarketmodels/liborforwardmodel.hpp>
#include <ql/legacy/libormarketmodels/lfmswaptionengine.hpp>
#include <ql/legacy/libormarketmodels/lfmhullwhiteparam.hpp>

#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/schedule.hpp>
#include <ql/quotes/simplequote.hpp>

#include "customutilities.hpp"

using namespace QuantLib;
//using namespace boost::unit_test_framework;

namespace {

	/* Input Parameters
		boost::shared_ptr<IborIndex> makeIndex(std::vector<Date> dates,	std::vector<Rate> rates)
		boost::shared_ptr<IborIndex> makeIndex()
		boost::shared_ptr<OptionletVolatilityStructure> caplet volatility curve
	*/
	
	boost::shared_ptr<IborIndex> makeIndex(std::vector<Date> dates,
		std::vector<Rate> rates) {
			DayCounter dayCounter = Actual360();

			RelinkableHandle<YieldTermStructure> termStructure;

			boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

			Date todaysDate =
				index->fixingCalendar().adjust(Date(4,September,2005));
			Settings::instance().evaluationDate() = todaysDate;

			dates[0] = index->fixingCalendar().advance(todaysDate,
				index->fixingDays(), Days);

			termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
				new ZeroCurve(dates, rates, dayCounter)));

			return index;
	}


	boost::shared_ptr<IborIndex> makeIndex() {
		std::vector<Date> dates;
		std::vector<Rate> rates;
		dates.push_back(Date(4,September,2005));
		dates.push_back(Date(4,September,2018));
		rates.push_back(0.039);
		rates.push_back(0.041);

		return makeIndex(dates, rates);
	}

	
	boost::shared_ptr<OptionletVolatilityStructure>
		makeCapVolCurve(const Date& todaysDate) {
			Volatility vols[] = {14.40, 17.15, 16.81, 16.64, 16.17,
				15.78, 15.40, 15.21, 14.86}; // caplet volatility curve

			std::vector<Date> dates;
			std::vector<Volatility> capletVols;
			boost::shared_ptr<LiborForwardModelProcess> process(
				new LiborForwardModelProcess(10, makeIndex()));

			for (Size i=0; i < 9; ++i) {
				capletVols.push_back(vols[i]/100);
				dates.push_back(process->fixingDates()[i+1]);
			}

			return boost::shared_ptr<CapletVarianceCurve>(
				new CapletVarianceCurve(todaysDate, dates,
				capletVols, Actual360()));
	}
}


void LiborMarketModelTest::testSimpleCovarianceModels() {
	BOOST_MESSAGE("Testing simple covariance models...");

	SavedSettings backup;

	const Size size = 10;
	const Real tolerance = 1e-14;
	Size i;

	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmExponentialCorrelationModel(size, 0.1));

	Matrix recon = corrModel->correlation(0.0)
		- corrModel->pseudoSqrt(0.0)*transpose(corrModel->pseudoSqrt(0.0));

	for (i=0; i<size; ++i) {
		for (Size j=0; j<size; ++j) {			
			if (std::fabs(recon[i][j]) > tolerance)
				BOOST_ERROR("Failed to reproduce correlation matrix"
				<< "\n    calculated: " << recon[i][j]
				<< "\n    expected:   " << 0);
		}		
	}

	std::vector<Time> fixingTimes(size);
	for (i=0; i<size; ++i) {
		fixingTimes[i] = 0.5*i;
	}

	const Real a=0.2;
	const Real b=0.1;
	const Real c=2.1;
	const Real d=0.3;

	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmLinearExponentialVolatilityModel(fixingTimes, a, b, c, d));

	boost::shared_ptr<LfmCovarianceProxy> covarProxy(
		new LfmCovarianceProxy(volaModel, corrModel));

	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, makeIndex()));

	boost::shared_ptr<LiborForwardModel> liborModel(
		new LiborForwardModel(process, volaModel, corrModel));

	std::cout.precision(3);
	for (Real t=0; t<4.6; t+=0.31) { //14*0.31=4.34<4.60
		recon = covarProxy->covariance(t)
			- covarProxy->diffusion(t)*transpose(covarProxy->diffusion(t));
		std::cout << "Covariance Matrix" << std::endl;
		std::cout << covarProxy->covariance(t);
		for (Size i=0; i<size; ++i) {
			for (Size j=0; j<size; ++j) {				
				if (std::fabs(recon[i][j]) > tolerance)
					BOOST_ERROR("Failed to reproduce correlation matrix"
					<< "\n    calculated: " << recon[i][j]
				<< "\n    expected:   " << 0);
			}
		}

		Array volatility = volaModel->volatility(t);
		std::cout << "Volatility" << std::endl;
		std::cout << volatility << std::endl;
		for (Size k=0; k<size; ++k) {
			Real expected = 0;
			if (k>2*t) {
				const Real T = fixingTimes[k];
				expected=(a*(T-t)+d)*std::exp(-b*(T-t)) + c;
			}
			
			if (std::fabs(expected - volatility[k]) > tolerance)
				BOOST_ERROR("Failed to reproduce volatilities"
				<< "\n    calculated: " << volatility[k]
			<< "\n    expected:   " << expected);
		}
	}
}


void LiborMarketModelTest::testCapletPricing() {
	BOOST_MESSAGE("Testing caplet pricing...");

	SavedSettings backup;

	const Size size = 10;
#if defined(QL_USE_INDEXED_COUPON)
	const Real tolerance = 1e-5;
#else
	const Real tolerance = 1e-12;
#endif

	boost::shared_ptr<IborIndex> index = makeIndex();
	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, index));

	// set-up pricing engine
	const boost::shared_ptr<OptionletVolatilityStructure> capVolCurve =
		makeCapVolCurve(Settings::instance().evaluationDate()); //input caplet volatility curve

	Array variances = LfmHullWhiteParameterization(process, capVolCurve)
		.covariance(0.0).diagonal();
	std::cout << Sqrt(variances) << std::endl;
	// caplet volatility = Sqrt(variances) = [ 0.000000; 0.144000; 0.196030; 0.159045; 0.162679; 0.139841; 0.137742; 0.128101; 0.138934; 0.116066 ]

	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmFixedVolatilityModel(Sqrt(variances),
		process->fixingTimes()));

	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmExponentialCorrelationModel(size, 0.3)); //rho=0.3

	boost::shared_ptr<AffineModel> model(
		new LiborForwardModel(process, volaModel, corrModel));

	const Handle<YieldTermStructure> termStructure =
		process->index()->forwardingTermStructure();

	boost::shared_ptr<AnalyticCapFloorEngine> engine1(
		new AnalyticCapFloorEngine(model, termStructure));

	boost::shared_ptr<Cap> cap1(
		new Cap(process->cashFlows(),
		std::vector<Rate>(size, 0.04))); //exercise rate=0.04
	cap1->setPricingEngine(engine1);

	const Real expected = 0.015853935178;
	const Real calculated = cap1->NPV();

	std::cout << "Cap NPV: " << cap1->NPV() << std::endl;

	if (std::fabs(expected - calculated) > tolerance)
		BOOST_ERROR("Failed to reproduce npv"
		<< "\n    calculated: " << calculated
		<< "\n    expected:   " << expected);
}

void LiborMarketModelTest::testCalibration() {
	BOOST_MESSAGE("Testing calibration of a Libor forward model...");

	SavedSettings backup;

	const Size size = 14;
	const Real tolerance = 8e-3;

	/* In reality, one has to strip caplet volatilities back from cap quotes
		cap market prices (cap quotes) -> implied caplet volatilities
	*/
	Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
		0.169007,0.167956,0.166261,0.164239,
		0.162082,0.159923,0.157781,0.155745,
		0.153776,0.151950,0.150189,0.148582,
		0.147034,0.145598,0.144248}; //19

	Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
		0.136930, 0.126833, 0.118135, 0.175963,
		0.166359, 0.155203, 0.143712, 0.132769,
		0.122947, 0.114310, 0.174455, 0.162265,
		0.150539, 0.138734, 0.128215, 0.118470,
		0.110540, 0.169780, 0.156860, 0.144821,
		0.133537, 0.123167, 0.114363, 0.106500,
		0.164521, 0.151223, 0.139670, 0.128632,
		0.119123, 0.110330, 0.103114, 0.158956,
		0.146036, 0.134555, 0.124393, 0.115038,
		0.106996, 0.100064}; //42

	boost::shared_ptr<IborIndex> index = makeIndex();
	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, index));
	Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

	// set-up the model
	// sigma_n(t)=sigma(T_n-t)=(a(T_n-t)+b)*exp(-c(T_n-t))+d)
	Real aguess = 0.5;
	Real bguess = 0.6;
	Real cguess = 0.1;
	Real dguess = 0.1;
	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmExtLinearExponentialVolModel(process->fixingTimes(), aguess, bguess, cguess, dguess));

	// rho(i,j;rho,beta)=rho+(1-rho)*exp(-beta*|i-j|)
	Real rhoguess = 0.5;
	Real betaguess = 0.8;
	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmLinearExponentialCorrelationModel(size, rhoguess, betaguess));

	boost::shared_ptr<LiborForwardModel> model(
		new LiborForwardModel(process, volaModel, corrModel));

	Size swapVolIndex = 0;
	DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

	// set-up calibration helper
	std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

	Size i;
	for (i=2; i < size; ++i) { // 2,3,...,13
		const Period maturity = i*index->tenor();
		Handle<Quote> capVol(
			boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-2])));

		boost::shared_ptr<CalibrationHelper> caphelper(
			new CapHelper(maturity, capVol, index, Annual,
			index->dayCounter(), true, termStructure,
			CalibrationHelper::ImpliedVolError));

		caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
			new AnalyticCapFloorEngine(model, termStructure)));

		calibrationHelper.push_back(caphelper);
		
		if (i<= size/2) { // 2,3,4,5,6,7
			// add a few swaptions to test swaption calibration as well
			for (Size j=1; j <= size/2; ++j) {
				const Period len = j*index->tenor();
				Handle<Quote> swaptionVol(
					boost::shared_ptr<Quote>(
					new SimpleQuote(swaptionVols[swapVolIndex++])));
				
				boost::shared_ptr<CalibrationHelper> swaptionHelper(
					new SwaptionHelper(maturity, len, swaptionVol, index,
					index->tenor(), dayCounter,
					index->dayCounter(),
					termStructure,
					CalibrationHelper::ImpliedVolError));

				swaptionHelper->setPricingEngine(
					boost::shared_ptr<PricingEngine>(
					new LfmSwaptionEngine(model,termStructure)));

				calibrationHelper.push_back(swaptionHelper);
			}
		}
	}
	std::cout << "calibration helper size: " << calibrationHelper.size() << std::endl; // 12+7*(12/2)=54

	/*
	boost::shared_ptr<LiborForwardModel>(
		boost::shared_ptr<LiborForwardModelProcess>, //boost::shared_ptr<LiborForwardModelProcess> process(new LiborForwardModelProcess(size, index));
		boost::shared_ptr<LmVolatilityModel>, //LmExtLinearExponentialVolModel(process->fixingTimes(),0.5,0.6,0.1,0.1));
		boost::shared_ptr<LmCorrelationModel> //LmLinearExponentialCorrelationModel(size, 0.5, 0.8));
		))->calibrate(
			std::vector<boost::shared_ptr<CalibrationHelper> >, 
			LevenbergMarquardt(1e-6, 1e-6, 1e-6), 
			EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
	*/

	/*
		Calibrated parameters
		1. correlation of forward LIBOR rates L_n(t)=L(t,Tn,Tn+1): mu(j,k)=Corr(dL_k(t),dL_j(t))=q(T_k-t,T_j-t)
		2. volatilities of forward LIBOR rates L_n(t)=L(t,Tn,Tn+1): sigma_n(t), t=0,...,T_M-1, n=t,...,M
		   sigma_n(t)=lambda_n(t)*phi(L_n(t))
		             =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*k_n
					 =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*(L_n(t))^p
					 =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*(b*L_n(t)+a)
		   sigma_n(t)=v(n)*sigma_n(t)
					 =v(n)*sigma(T_n-t)
					 =v(n)*(a(T_n-t)+b)*exp(-c(T_n-t))+d)
	*/
	
	LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
	model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));

	std::cout.precision(10);

	// measure the calibration error
	Real calculated = 0.0;
	for (i=0; i<calibrationHelper.size(); ++i) {
		Real diff = calibrationHelper[i]->calibrationError();
		calculated += diff*diff;
		std::cout << i << ": " << calibrationHelper[i]->marketValue() << std::endl;
	}
	if (std::sqrt(calculated) > tolerance)
		BOOST_ERROR("Failed to calibrate libor forward model"
		<< "\n    calculated diff: " << std::sqrt(calculated)
		<< "\n    expected : smaller than  " << tolerance);
}

void LiborMarketModelTest::testSwaptionPricing() {
	BOOST_MESSAGE("Testing forward swap and swaption pricing...");

	SavedSettings backup;

	const Size size  = 10;
	const Size steps = 8*size;
#if defined(QL_USE_INDEXED_COUPON)
	const Real tolerance = 1e-6;
#else
	const Real tolerance = 1e-12;
#endif

	std::vector<Date> dates;
	std::vector<Rate> rates;
	dates.push_back(Date(4,September,2005));
	dates.push_back(Date(4,September,2011));
	rates.push_back(0.04);
	rates.push_back(0.08);

	boost::shared_ptr<IborIndex> index = makeIndex(dates, rates);

	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, index));

	Real rhoguess = 0.5;
	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmExponentialCorrelationModel(size, rhoguess));

	Real aguess = 0.291;
	Real bguess = 1.483;
	Real cguess = 0.116;
	Real dguess = 0.00001;
	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmLinearExponentialVolatilityModel(process->fixingTimes(),
		aguess, bguess, cguess, dguess));

	// set-up pricing engine
	process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
		new LfmCovarianceProxy(volaModel, corrModel)));

	// set-up a small Monte-Carlo simulation to price swaptions
	typedef PseudoRandom::rsg_type rsg_type;
	typedef MultiPathGenerator<rsg_type>::sample_type sample_type;

	std::vector<Time> tmp = process->fixingTimes();
	TimeGrid grid(tmp.begin(), tmp.end(), steps);

	Size i;
	std::vector<Size> location;
	for (i=0; i < tmp.size(); ++i) {
		location.push_back(
			std::find(grid.begin(),grid.end(),tmp[i])-grid.begin());
	}

	rsg_type rsg = PseudoRandom::make_sequence_generator(
		process->factors()*(grid.size()-1),
		BigNatural(42));

	const Size nrTrails = 5000;
	MultiPathGenerator<rsg_type> generator(process, grid, rsg, false);

	boost::shared_ptr<LiborForwardModel>
		liborModel(new LiborForwardModel(process, volaModel, corrModel));

	Calendar calendar = index->fixingCalendar();
	DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
	BusinessDayConvention convention = index->businessDayConvention();

	Date settlement  = index->forwardingTermStructure()->referenceDate();

	boost::shared_ptr<SwaptionVolatilityMatrix> m =
		liborModel->getSwaptionVolatilityMatrix();


	std::cout << "Swaption NPV" << std::endl;
	for (i=1; i < size; ++i) { //i=1,2,,,9
		for (Size j=1; j <= size-i; ++j) { //j=1-9,1-8,...1-2,1 
			Date fwdStart    = settlement + Period(6*i, Months);
			Date fwdMaturity = fwdStart + Period(6*j, Months);

			Schedule schedule(fwdStart, fwdMaturity, index->tenor(), calendar,
				convention, convention, DateGeneration::Forward, false);

			Rate swapRate  = 0.0404;
			boost::shared_ptr<VanillaSwap> forwardSwap( //Swap
				new VanillaSwap(VanillaSwap::Receiver, 1.0,
				schedule, swapRate, dayCounter,
				schedule, index, 0.0, index->dayCounter()));
			forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
				new DiscountingSwapEngine(index->forwardingTermStructure())));

			// check forward pricing first
			const Real expected = forwardSwap->fairRate();
			const Real calculated = liborModel->S_0(i-1,i+j-1);

			if (std::fabs(expected - calculated) > tolerance)
				BOOST_ERROR("Failed to reproduce fair forward swap rate"
				<< "\n    calculated: " << calculated
				<< "\n    expected:   " << expected);

			swapRate = forwardSwap->fairRate();
			forwardSwap = boost::shared_ptr<VanillaSwap>(
				new VanillaSwap(VanillaSwap::Receiver, 1.0,
				schedule, swapRate, dayCounter,
				schedule, index, 0.0, index->dayCounter()));
			forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
				new DiscountingSwapEngine(index->forwardingTermStructure())));

			if (i == j && i<=size/2) { //i=j=1,2,3,4,5
				boost::shared_ptr<PricingEngine> engine(
					new LfmSwaptionEngine(liborModel,
					index->forwardingTermStructure()));
				boost::shared_ptr<Exercise> exercise(
					new EuropeanExercise(process->fixingDates()[i]));

				boost::shared_ptr<Swaption> swaption( //Swaption
					new Swaption(forwardSwap, exercise));
				swaption->setPricingEngine(engine);

				GeneralStatistics stat;

				for (Size n=0; n<nrTrails; ++n) {
					sample_type path = (n%2) ? generator.antithetic()
						: generator.next();

					std::vector<Rate> rates(size);
					for (Size k=0; k<process->size(); ++k) {
						rates[k] = path.value[k][location[i]];
					}
					std::vector<DiscountFactor> dis =
						process->discountBond(rates);

					Real npv=0.0;
					for (Size m=i; m < i+j; ++m) {
						npv += (swapRate - rates[m])
							* (  process->accrualEndTimes()[m]
						- process->accrualStartTimes()[m])*dis[m];
					}
					stat.add(std::max(npv, 0.0));
				}

				if (std::fabs(swaption->NPV() - stat.mean())
					> stat.errorEstimate()*2.35)
					BOOST_ERROR("Failed to reproduce swaption npv"
					<< "\n    calculated: " << stat.mean()
					<< "\n    expected:   " << swaption->NPV());

				std::cout << swaption->NPV() << ", " << stat.mean() << std::endl;
			}
		}
	}
}

void LiborMarketModelTest::mylmmtest(){
	BOOST_MESSAGE("Å°Å°Å° Testing calibration of a Libor forward model Å°Å°Å°");

	SavedSettings backup;

	const Size size = 14;
	const Real tolerance = 8e-3;

	/*
		1. Time dependent volatilities of forward LIBOR rates: lambdak(t)
		2. Correlation of forward LIBOR rates
	*/

	Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
		0.169007,0.167956,0.166261,0.164239,
		0.162082,0.159923,0.157781,0.155745,
		0.153776,0.151950,0.150189,0.148582,
		0.147034,0.145598,0.144248}; //19

	Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
		0.136930, 0.126833, 0.118135, 0.175963,
		0.166359, 0.155203, 0.143712, 0.132769,
		0.122947, 0.114310, 0.174455, 0.162265,
		0.150539, 0.138734, 0.128215, 0.118470,
		0.110540, 0.169780, 0.156860, 0.144821,
		0.133537, 0.123167, 0.114363, 0.106500,
		0.164521, 0.151223, 0.139670, 0.128632,
		0.119123, 0.110330, 0.103114, 0.158956,
		0.146036, 0.134555, 0.124393, 0.115038,
		0.106996, 0.100064}; //42

	std::cout << "\ncapVols" << std::endl;
	for (Size i=0; i<LENGTH(capVols); ++i) {
		std::cout << capVols[i] << std::endl;
	}
	std::cout << "\nswaptionVols" << std::endl;
	for (Size i=0; i<LENGTH(swaptionVols); ++i) {
		std::cout << swaptionVols[i] << std::endl;
	}


	boost::shared_ptr<IborIndex> index = makeIndex();
	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, index));
	Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

	// set-up the model
	// Rebonato ||lambdak(t)||=g(t,x)=g(x)=(a+bx)exp(-cx)+d
	// sigma_i(t)=k_i*((a*(T_{i}-t)+d)*e^{-b(T_{i}-t)}+c)
	const Real a=0.5; //0.2
	const Real b=0.6; //0.1
	const Real c=0.1; //2.1
	const Real d=0.1; //0.3	
	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmExtLinearExponentialVolModel(process->fixingTimes(), a, b, c, d));
	std::cout << "\nCaplet Volatility" << std::endl;
	for (Size t=0; t<size/2; ++t) {
		std::cout << t << ": " << volaModel->volatility(t) << std::endl;
	}	

	// rho_{i,j}=rho + (1-rho)*e^{(-\beta \|i-j\|)}
	const Real rho=0.5;
	const Real beta=0.5;
	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmLinearExponentialCorrelationModel(size, rho, beta));
	// lmexpcorrmodel.cpp
	std::cout << "\nForward Libor Correlation" << std::endl;
	std::cout << corrModel->correlation(0) << std::endl;
	/*
	std::cout << "sqrt(correlation)" << std::endl;
	std::cout << corrModel->pseudoSqrt(0) << std::endl;
	std::cout << "sqrt(correlation)^T*sqrt(correlation)" << std::endl;
	std::cout << (corrModel->pseudoSqrt(0))*transpose(corrModel->pseudoSqrt(0)) << std::endl;
	*/
	
	boost::shared_ptr<LiborForwardModel> model(
		new LiborForwardModel(process, volaModel, corrModel));
	const Array initparams = model->params();

	Size swapVolIndex = 0;
	DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

	// set-up calibration helper
	std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

	Size i;
	for (i=2; i < size; ++i) { // 2,3,...,13
		const Period maturity = i*index->tenor();
		Handle<Quote> capVol(
			boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-2])));

		boost::shared_ptr<CalibrationHelper> caphelper(
			new CapHelper(maturity, capVol, index, Annual,
			index->dayCounter(), true, termStructure,
			CalibrationHelper::ImpliedVolError));

		caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
			new AnalyticCapFloorEngine(model, termStructure)));

		calibrationHelper.push_back(caphelper);

		if (i<= size/2) { // 2,3,4,5,6,7
			// add a few swaptions to test swaption calibration as well
			for (Size j=1; j <= size/2; ++j) {
				const Period len = j*index->tenor();
				Handle<Quote> swaptionVol(
					boost::shared_ptr<Quote>(
					new SimpleQuote(swaptionVols[swapVolIndex++])));

				boost::shared_ptr<CalibrationHelper> swaptionHelper(
					new SwaptionHelper(maturity, len, swaptionVol, index,
					index->tenor(), dayCounter,
					index->dayCounter(),
					termStructure,
					CalibrationHelper::ImpliedVolError));

				swaptionHelper->setPricingEngine(
					boost::shared_ptr<PricingEngine>(
					new LfmSwaptionEngine(model,termStructure)));

				calibrationHelper.push_back(swaptionHelper);
			}
		}
	}	

	/*
	boost::shared_ptr<LiborForwardModel>(
		boost::shared_ptr<LiborForwardModelProcess>, //boost::shared_ptr<LiborForwardModelProcess> process(new LiborForwardModelProcess(size, index));
		boost::shared_ptr<LmVolatilityModel>, //LmExtLinearExponentialVolModel(process->fixingTimes(),0.5,0.6,0.1,0.1));
		boost::shared_ptr<LmCorrelationModel> //LmLinearExponentialCorrelationModel(size, 0.5, 0.8));
		))->calibrate(
			std::vector<boost::shared_ptr<CalibrationHelper> >, 
			LevenbergMarquardt(1e-6, 1e-6, 1e-6), 
			EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
	*/
	
	LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
	model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
	const Array calibparams = model->params();

	std::cout.precision(10);

	// measure the calibration error
	std::cout << "\nCalibration Helper Parameters (After Calibration)" << std::endl;
	std::cout << "Market Value\tCalibration Error" << std::endl;
	Real calculated = 0.0;
	for (i=0; i<calibrationHelper.size(); ++i) {
		Real diff = calibrationHelper[i]->calibrationError();
		calculated += diff*diff;

		//std::cout << calibrationHelper[i]->marketValue() << std::endl; //i << ": " << 
		std::cout << i << ": " << calibrationHelper[i]->marketValue() << "\ts" << calibrationHelper[i]->calibrationError() << std::endl;
	}
	if (std::sqrt(calculated) > tolerance)
		BOOST_ERROR("Failed to calibrate libor forward model"
		<< "\n    calculated diff: " << std::sqrt(calculated)
		<< "\n    expected : smaller than  " << tolerance);

	const char paramnames[20][64] = {"a","b","c","d",
		                 "sigma1","sigma2","sigma3","sigma4","sigma5",
						 "sigma6","sigma7","sigma8","sigma9","sigma10",
						 "sigma11","sigma12","sigma13","sigma14", //forward libor volatilities: sigma
						 "rho","beta"};  //4+14+2=20

	std::cout << "\nParameters" << std::endl;
	std::cout << std::setw(3) << "i" << std::setw(10) << "Parameter" << std::setw(15) << "Initial" << std::setw(15) << "Calibrated" << std::endl;
	for (Size i=0; i<calibparams.size(); ++i) {
		std::cout << std::setw(3) << i << std::setw(10) << paramnames[i] 
		<< std::setw(15) << initparams[i] << std::setw(15) << calibparams[i] << std::endl;
	}
	std::cout << "\nEstimated Forward LIBOR Volatilities" << std::endl;
	for (Size i=4; i<18; ++i) { //calibparams.size()=20=4+14+2
		std::cout << std::setw(3) << ((i-4.0)/2) << std::setw(10) << paramnames[i] << std::setw(15) << calibparams[i] << std::endl;
	}


	//******************************************************************************
	// Repricing caps and swaptions using the calibrated parameters
	//******************************************************************************
	std::cout << "\n\nRepricing caps and swaptions using the calibrated parameters" << std::endl;
	boost::shared_ptr<LmVolatilityModel> volaModelOpt(
			new LmExtLinearExponentialVolModel(process->fixingTimes(), calibparams[0], calibparams[1], calibparams[2], calibparams[3]));
	boost::shared_ptr<LmCorrelationModel> corrModelOpt(
		new LmLinearExponentialCorrelationModel(size, calibparams[19], calibparams[20]));
	/*boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, makeIndex()));*/
	process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
		new LfmCovarianceProxy(volaModelOpt, corrModelOpt)));
	boost::shared_ptr<LiborForwardModel> liborModelOpt(
		new LiborForwardModel(process, volaModelOpt, corrModelOpt));

	Calendar calendar = index->fixingCalendar();
	//DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
	BusinessDayConvention convention = index->businessDayConvention();
	Date settlement  = index->forwardingTermStructure()->referenceDate();

	//boost::shared_ptr<SwaptionVolatilityMatrix> volaMatOpt =
	//liborModelOpt->getSwaptionVolatilityMatrix();
	//std::cout << volaModelOpt->volatility(0.0) << std::endl;
	//std::cout << corrModelOpt->correlation(0.0) << std::endl;
	std::cout.precision(4);
	std::cout << "\nForward LIBOR volatility matrix recovered from cap/swaption volatilities" << std::endl;
	boost::shared_ptr<LfmCovarianceProxy> covarProxyOpt(
		new LfmCovarianceProxy(volaModelOpt, corrModelOpt));
	std::cout << "Forward LIBOR proxy volatility at time 0" << std::endl;
	std::cout << volaModelOpt->volatility(0) << std::endl;
	std::cout << "Forward LIBOR proxy covariance matrix at time 0" << std::endl;
	std::cout << covarProxyOpt->covariance(0) << std::endl;	
	for (Real t=0; t<4.6; t+=0.31) { //14*0.31=4.34<4.60
		std::cout << "Forward LIBOR proxy volatility at time " << t << std::endl;
		std::cout << covarProxyOpt->covariance(t) << std::endl;
		std::cout << "Forward LIBOR proxy volatility matrix at time " << t << std::endl;
		std::cout << volaModelOpt->volatility(t) << std::endl;
	}

	std::cout << "\nSwaption NPV using calibrated forward LIBOR vols/corrs parameters" << std::endl;
	for (i=1; i < size; ++i) { //i=1,2,,,14
		for (Size j=1; j <= size-i; ++j) { //j=1~14,1~13,1-9,1-8,...1-2,1 
			Date fwdStart    = settlement + Period(6*i, Months);
			Date fwdMaturity = fwdStart + Period(6*j, Months);

			Schedule schedule(fwdStart, fwdMaturity, index->tenor(), calendar,
				convention, convention, DateGeneration::Forward, false);

			Rate swapRate  = 0.0404;
			boost::shared_ptr<VanillaSwap> forwardSwap(
				new VanillaSwap(VanillaSwap::Receiver, 1.0,
				schedule, swapRate, dayCounter,
				schedule, index, 0.0, index->dayCounter()));
			forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
				new DiscountingSwapEngine(index->forwardingTermStructure())));

			swapRate = forwardSwap->fairRate();
			forwardSwap = boost::shared_ptr<VanillaSwap>(
				new VanillaSwap(VanillaSwap::Receiver, 1.0,
				schedule, swapRate, dayCounter,
				schedule, index, 0.0, index->dayCounter()));
			forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
				new DiscountingSwapEngine(index->forwardingTermStructure())));

			if (i == j && i<=size) { //i=j=1,2,3,4,5,6,7,...,14
				boost::shared_ptr<PricingEngine> engine(
					new LfmSwaptionEngine(liborModelOpt,
					index->forwardingTermStructure()));
				boost::shared_ptr<Exercise> exercise(
					new EuropeanExercise(process->fixingDates()[i]));

				boost::shared_ptr<Swaption> swaption( //Swaption
					new Swaption(forwardSwap, exercise));
				swaption->setPricingEngine(engine);
				
				Real swaptionNPV = swaption->NPV();
				//std::cout << swaption->NPV() << std::endl;
				std::cout << swaption->impliedVolatility(swaptionNPV,index->forwardingTermStructure(),0.15) << std::endl;
			}
		}
	}
}

//////////////////////////////////////////////////////////////////////////

namespace {

	boost::shared_ptr<IborIndex> makeIndex_(std::vector<Date> dates,
		std::vector<Rate> rates) {
			DayCounter dayCounter = Actual360();

			RelinkableHandle<YieldTermStructure> termStructure;

			boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

			Date todaysDate = index->fixingCalendar().adjust(Date(31,October,2001));// Input
			Settings::instance().evaluationDate() = todaysDate;

			dates[0] = index->fixingCalendar().advance(todaysDate,
				index->fixingDays(), Days);

			termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
				new ZeroCurve(dates, rates, dayCounter)));

			return index;
	}

	// Interpolate forward LIBOR rates
	boost::shared_ptr<IborIndex> makeIndex_() {
		std::vector<Date> dates;
		std::vector<Rate> rates;
		dates.push_back(Date(31,October,2001));// Input
		dates.push_back(Date(31,October,2011));
		rates.push_back(0.0009);
		rates.push_back(0.0275);

		return makeIndex_(dates, rates);
	}
}


void LiborMarketModelTest::testIshiyamaLMM(){

	BOOST_MESSAGE("êŒéRçKëæòY Forward LIBOR Market Model Calibration Test");

	//******************************************************************************
	// Calibrate LMM vol/corr parameters using cap/swaption volatilities
	//******************************************************************************
	SavedSettings backup;

	const Size size = 20;
	const Real tolerance = 8e-3;

	// caplet volatility curve
	Volatility capVols[] = {153.50, 140.00, 126.50, 113.00, 100.50, 88.00, 83.75, 79.50, 73.00, 66.50,
		61.75, 57.00, 52.75, 48.50, 48.00, 47.50, 45.75, 44.00, 42.00, 40.00};//20
	/*Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
		0.169007,0.167956,0.166261,0.164239,
		0.162082,0.159923,0.157781,0.155745,
		0.153776,0.151950,0.150189,0.148582,
		0.147034,0.145598,0.144248}; //19*/

	Volatility swaptionVols[] = {82.40, 56.60, 53.10, 49.00, 44.90, 42.25, 39.60, 36.33, 33.07, 29.80,
		                         52.20, 47.70, 45.20, 41.10, 37.60, 33.85, 30.10, 28.63, 27.17, 25.70,
								 44.80,	41.50, 36.90, 33.55, 31.50, 28.75, 26.00, 24.85, 23.70, 22.55,
								 42.10, 36.20, 32.00, 33.30, 30.70, 26.88, 22.65, 21.75, 20.85, 19.95,
								 35.20, 29.00, 26.30, 23.60, 21.40, 20.50, 19.60, 19.20, 18.80, 18.40,
								 29.80, 25.13, 23.50, 21.58, 19.85, 19.25, 18.65, 18.34, 18.03, 17.73,
								 24.40, 21.25, 20.70, 19.55, 18.30, 18.00, 17.70, 17.48, 17.27, 17.05,
								 23.13, 20.23, 20.00, 18.88, 17.53, 17.23, 16.93, 16.75, 16.57, 16.38,
								 21.87, 19.22, 19.30, 18.22, 16.77, 16.47, 16.17, 16.02, 15.87, 15.72,
								 20.60, 18.20, 18.60, 17.55, 16.00, 15.70, 15.40, 15.28, 15.17, 15.05};//10x10=100
	/*Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
		0.136930, 0.126833, 0.118135, 0.175963,
		0.166359, 0.155203, 0.143712, 0.132769,
		0.122947, 0.114310, 0.174455, 0.162265,
		0.150539, 0.138734, 0.128215, 0.118470,
		0.110540, 0.169780, 0.156860, 0.144821,
		0.133537, 0.123167, 0.114363, 0.106500,
		0.164521, 0.151223, 0.139670, 0.128632,
		0.119123, 0.110330, 0.103114, 0.158956,
		0.146036, 0.134555, 0.124393, 0.115038,
		0.106996, 0.100064}; //42*/

	boost::shared_ptr<IborIndex> index = makeIndex_();
	index->forwardingTermStructure()->enableExtrapolation(true);

	boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, index));

	Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

	// set-up the model
	Real aguess = 0.5;
	Real bguess = 0.6;
	Real cguess = 0.1;
	Real dguess = 0.1;
	boost::shared_ptr<LmVolatilityModel> volaModel(
		new LmExtLinearExponentialVolModel(process->fixingTimes(), aguess, bguess, cguess, dguess));

	Real rhoguess = 0.5;
	Real betaguess = 0.8;
	boost::shared_ptr<LmCorrelationModel> corrModel(
		new LmLinearExponentialCorrelationModel(size, rhoguess, betaguess));
	
	boost::shared_ptr<LiborForwardModel> model(
		new LiborForwardModel(process, volaModel, corrModel));
	const Array initparams = model->params();
	
	Size swapVolIndex = 0;
	DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

	// set-up calibration helper
	std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

	int caphelpersize =0;
	int swaptionhelpersize =0;

	Size i;
	for (i=1; i <= size; ++i) { // 1,2,...,20
		const Period maturity = i*index->tenor(); //0.5y,1y,1.5y,...,10y
		Handle<Quote> capVol(
			boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-1]/100.0))); //i-1=0,1,...,19

		boost::shared_ptr<CalibrationHelper> caphelper(
			new CapHelper(maturity, capVol, index, Annual,
			index->dayCounter(), true, termStructure,
			CalibrationHelper::ImpliedVolError));

		caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
			new AnalyticCapFloorEngine(model, termStructure)));

		calibrationHelper.push_back(caphelper);
		caphelpersize++; //caphelpersize=1,2,...,20
		
		// add a few swaptions to test swaption calibration as well
		if (i < size/2) { //i=1,2,...,9
			for (Size j=1; j <= (size/2-i); ++j) { //j=1~9, 1~8,...,1~2, 1
				const Period len = 2*j*index->tenor(); //1y~9y, 1y~8y,..., 1y
				swapVolIndex = (j-1) + (i-1)*10;
				Handle<Quote> swaptionVol(
					boost::shared_ptr<Quote>(
					new SimpleQuote(swaptionVols[swapVolIndex]/100.0)));
				//std::cout << swaptionVols[swapVolIndex] << ", ";
				
				boost::shared_ptr<CalibrationHelper> swaptionHelper(
					new SwaptionHelper(maturity, len, swaptionVol, index,
					index->tenor(), dayCounter,
					index->dayCounter(),
					termStructure,
					CalibrationHelper::ImpliedVolError));

				swaptionHelper->setPricingEngine(
					boost::shared_ptr<PricingEngine>(
					new LfmSwaptionEngine(model,termStructure)));

				calibrationHelper.push_back(swaptionHelper);
				swaptionhelpersize++;
				std::cout << "(" << i << ", " << j << "; " << swapVolIndex << "), ";
			}
			std::cout << std::endl;
		}
		
	}
	
	std::cout << "cap helper size: " << caphelpersize << std::endl; //20
	std::cout << "swaption helper size: " << swaptionhelpersize << std::endl; // 9(9+1)/2=45
	std::cout << "calibration helper size: " << calibrationHelper.size() << std::endl; // 65
	
	LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
	model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
	const Array calibparams = model->params();

	std::cout.precision(10);

	// measure the calibration error
	std::cout << "\nCalibration Helper Parameters (After Calibration)" << std::endl;
	std::cout << "Market Value\tCalibration Error" << std::endl;
	Real calculated = 0.0;
	for (i=0; i<calibrationHelper.size(); ++i) {
		Real diff = calibrationHelper[i]->calibrationError();
		calculated += diff*diff;
		std::cout << i << ": " << calibrationHelper[i]->marketValue() << "\ts" << calibrationHelper[i]->calibrationError() << std::endl;
	}
	if (std::sqrt(calculated) > tolerance)
		BOOST_ERROR("Failed to calibrate libor forward model"
		<< "\n    calculated diff: " << std::sqrt(calculated)
		<< "\n    expected : smaller than  " << tolerance);


	// print parameters
	const char paramnames[26][64] = {"a","b","c","d",
		"sigma1","sigma2","sigma3","sigma4","sigma5",
		"sigma6","sigma7","sigma8","sigma9","sigma10",
		"sigma11","sigma12","sigma13","sigma14",
		"sigma15","sigma16","sigma17","sigma18","sigma19","sigma20", //forward libor volatilities: sigma
		"rho","beta"};  //4+20+2=26

	std::cout << "\nParameters" << std::endl;
	std::cout << std::setw(3) << "i" << std::setw(10) << "Parameter" << std::setw(15) << "Initial" << std::setw(15) << "Calibrated" << std::endl;
	for (Size i=0; i<calibparams.size(); ++i) {
		std::cout << std::setw(3) << i << std::setw(10) << paramnames[i] 
		<< std::setw(15) << initparams[i] << std::setw(15) << calibparams[i] << std::endl;
	}
	std::cout << "\nEstimated Forward LIBOR Local Volatilities" << std::endl;
	for (Size i=4; i<24; ++i) { //calibparams.size()=26=4+20+2
		std::cout << std::setw(3) << ((i-4.0)/2) << std::setw(10) << paramnames[i] << std::setw(15) << calibparams[i] << std::endl;
	}


	//******************************************************************************
	// Repricing caps and swaptions using the calibrated parameters
	//******************************************************************************
	std::cout << "\n\nRepricing caps and swaptions using the calibrated parameters" << std::endl;
	boost::shared_ptr<LmVolatilityModel> volaModelOpt(
			new LmExtLinearExponentialVolModel(process->fixingTimes(), calibparams[0], calibparams[1], calibparams[2], calibparams[3]));
	boost::shared_ptr<LmCorrelationModel> corrModelOpt(
		new LmLinearExponentialCorrelationModel(size, calibparams[24], calibparams[25]));
	/*boost::shared_ptr<LiborForwardModelProcess> process(
		new LiborForwardModelProcess(size, makeIndex()));*/
	process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
		new LfmCovarianceProxy(volaModelOpt, corrModelOpt)));
	boost::shared_ptr<LiborForwardModel> liborModelOpt(
		new LiborForwardModel(process, volaModelOpt, corrModelOpt));

	Calendar calendar = index->fixingCalendar();
	//DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
	BusinessDayConvention convention = index->businessDayConvention();
	Date settlement  = index->forwardingTermStructure()->referenceDate();

	std::cout.precision(3);
	std::cout << "\nForward LIBOR volatility matrix recovered from cap/swaption volatilities" << std::endl;
	boost::shared_ptr<LfmCovarianceProxy> covarProxyOpt(
		new LfmCovarianceProxy(volaModelOpt, corrModelOpt));
	std::cout << "Forward LIBOR proxy volatility at time 0" << std::endl;
	std::cout << volaModelOpt->volatility(0)*100.0 << std::endl;
	std::cout << "Forward LIBOR proxy covariance matrix at time 0" << std::endl;
	std::cout << covarProxyOpt->covariance(0)*100.0 << std::endl;
	std::cout << "Forward LIBOR proxy correlation matrix at time 0" << std::endl;
	std::cout << corrModelOpt->correlation(0) << std::endl;
	for (Real t=0; t<10.0; t+=0.5) {
		std::cout << "Forward LIBOR proxy volatility at time " << t << std::endl;
		std::cout << volaModelOpt->volatility(t)*100.0 << std::endl;
		std::cout << "Forward LIBOR proxy covariance at time " << t << std::endl;
		std::cout << covarProxyOpt->covariance(t)*100.0 << std::endl;
		/*std::cout << "Forward LIBOR proxy correlation matrix at time " << t << std::endl;
		std::cout << corrModelOpt->correlation(t) << std::endl;*/
	}
}