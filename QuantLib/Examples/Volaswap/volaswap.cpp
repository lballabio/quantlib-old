#include <math.h>
#include <ql/quantlib.hpp>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace QuantLib;

#define FACTORS 10
#define LTCORR 0.5
#define CORRDECAY 0.2
#define PRICINGPATHS 100000
#define CALIBRATIONPATHS 10000
#define SEED 24

int main(int argc, char* argv[]) {

    bool calibrate = false;
    bool price = true;

	// set evalulation date

	Date evalDate = Date(18,September,2008);
	Date settlDate = TARGET().advance(evalDate,2*Days);
	Settings::instance().evaluationDate() = evalDate;

	// set up the interest rate curve

	// End of Day Data NxB IKB

	Real irquotevalues[] = { 0.0455, 0.0472, 0.0492, 0.0519, 0.0524,                // deposit quotes
		                  0.0516, 0.0485, 0.0481, 0.0480, 0.0480, 0.0480, 0.0481, 0.0482, 0.0483, 0.0486,
	                      0.0491, 0.0496, 0.0496, 0.0493, 0.0489};                 // swap quotes

	Period tenors[] = { 1*Months, 2*Months, 3*Months, 6*Months, 9*Months,           // deposits
						1*Years, 2*Years, 3*Years, 4*Years, 5*Years, 6*Years, 7*Years, 8*Years, 9*Years, 10*Years,
	                    12*Years, 15*Years, 20*Years, 25*Years, 30*Years };   // swaps

	Size noDep = 5, noSwap = 15; // put number of deposits and swaps here 

	std::vector<boost::shared_ptr<Quote> > irquotes;
	for(int i=0;i<noDep+noSwap;i++) {
		irquotes.push_back(boost::shared_ptr<Quote>(new SimpleQuote(irquotevalues[i])));
	}

	boost::shared_ptr<IborIndex> euribor6mBootstrap(new Euribor(6*Months)); // index without yieldcurve assigned for swaprate helpers

	std::vector<boost::shared_ptr<RateHelper> > irratehelpers;
	for(int i=0;i<noDep;i++) irratehelpers.push_back(boost::shared_ptr<RateHelper>(new DepositRateHelper(Handle<Quote>(irquotes[i]),tenors[i],
		2,TARGET(),ModifiedFollowing,false,Actual360()))); // check: fixing days = 2 for all tenors ?
	for(int i=noDep;i<noDep+noSwap;i++) irratehelpers.push_back(boost::shared_ptr<RateHelper>(new SwapRateHelper(Handle<Quote>(irquotes[i]),tenors[i],
		TARGET(), Annual, ModifiedFollowing, Thirty360(),euribor6mBootstrap)));

	boost::shared_ptr<YieldTermStructure> yts(new PiecewiseYieldCurve<ZeroYield,Linear>(0,TARGET(),irratehelpers,Actual365Fixed())); // discount is 1.0 on TODAY (evalDate)
	yts->enableExtrapolation();

	// set up the swaption vol cube

	// atm

	Period swapTenorsVal[] = { 20*Years, 25*Years, 30*Years };
	Period optionTenorsVal[] = { 3*Months, 6*Months, 1*Years, 2*Years, 3*Years, 4*Years, 5*Years, 7*Years };
	Size noSwAtm = 3, noOpAtm = 8; // put number of atm swap tenors and option tenors here

	// End of Day Data NxB
	//Real swaptionatmvalues[] = { 0.181, 0.18, 0.18, // option tenor 1 3m into 20y, 25y, 30y
	//	                         0.167, 0.166, 0.166, // option tenor 2 6m
	//							 0.148, 0.148, 0.149, //                1y
	//							 0.137, 0.137, 0.137, //                2y
	//							 0.13, 0.131, 0.132,  //                3y
	//							 0.126, 0.127, 0.128, //                4y
	//							 0.123, 0.124, 0.125, //                5y
	//							 0.118, 0.119, 0.119 }; //              7y
	
	// End of Day Data BBG VCUB
	Real swaptionatmvalues[] = { 0.1795,0.1780,0.1755, // option tenor 1 3m into 20y, 25y, 30y
		                         0.1635,0.1630,0.1615, // option tenor 2 6m
								 0.1470,0.1460,0.1460, //                1y
								 0.1350,0.1350,0.1350, //                2y
								 0.1295,0.1300,0.1300, //                3y
								 0.1270, 0.1270, 0.1270, //              4y
								 0.1235, 0.1240, 0.1240, //              5y
								 0.1165, 0.1160, 0.1145 }; //            7y 


	std::vector<Period> optionTenors, swapTenors;
	std::vector<std::vector<Handle<Quote> > > swatmquotes;
	for(int i=0;i<noOpAtm;i++) {
		optionTenors.push_back(optionTenorsVal[i]);
		std::vector<Handle<Quote> > swatmtemp;
		for(int j=0;j<noSwAtm;j++) {
			if(i==0) swapTenors.push_back(swapTenorsVal[j]);
			swatmtemp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(swaptionatmvalues[i*noSwAtm+j]))));
		}
		swatmquotes.push_back(swatmtemp);
	}

	boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolAtm(new SwaptionVolatilityMatrix(TARGET(),ModifiedFollowing,
		optionTenors,swapTenors,swatmquotes,Actual365Fixed()));

	// smile
	Period swapTenorsSmileVal[] = { 20*Years, 25*Years, 30*Years };
	//Period optionTenorsSmileVal[] = { 5*Years, 7*Years }; // for RB smile
	Period optionTenorsSmileVal[] = { 3*Months, 6*Months, 1*Years, 2*Years, 3*Years, 4*Years, 5*Years, 7*Years  }; // for BBG smile
	Real smileSpreadVal[] = { -0.02, -0.01, 0.00, 0.01, 0.02 };

	// RB Data (+100bp Quote)
	//Real smileVal[] = { 0.00, 0.00, 0.00, -0.0077, -0.0077, // opt1swp1 5y20y
	//	                0.00, 0.00, 0.00, 0.00, 0.00, // opt1swp2 5y25y
	//	                0.00, 0.00, 0.00, 0.00, 0.00, // ... 5y30y
	//					0.00, 0.00, 0.00, -0.0077, -0.0077, // opt2swp1 7y20y 
	//	                0.00, 0.00, 0.00, 0.00, 0.00, // opt2swp2 7y25y
	//					0.00, 0.00, 0.00, 0.00, 0.00 }; // ... 7y30y

	Real smileValAbs[] = { 0.2725, 0.2161, 0.00, 0.1706, 0.1741, // opt1swp1 3m20y // this is NOT the spread, but absolute vols, except ATM which is zero !
						0.2711, 0.2147, 0.00, 0.1691, 0.1727, // opt1swp2 3m25y	
						0.2684, 0.2121, 0.00, 0.1667, 0.1704, // opt1swp3 3m30y
						0.2484, 0.1970, 0.00, 0.1554, 0.1586, // opt2     6m
						0.2485, 0.1967, 0.00, 0.1549, 0.1582, // 		
						0.2472, 0.1953, 0.00, 0.1533, 0.1568, // 
						0.2329, 0.1840, 0.00, 0.1366, 0.1335, // opt3     1y
						0.2322, 0.1831, 0.00, 0.1356, 0.1326, // 		
						0.2333, 0.1834, 0.00, 0.1355, 0.1327, // 
						0.1684, 0.1472, 0.00, 0.1295, 0.1284, // opt4     2y
						0.1689, 0.1474, 0.00, 0.1295, 0.1284, // 		
						0.1695, 0.1475, 0.00, 0.1294, 0.1284, // 
						0.1688, 0.1442, 0.00, 0.1256, 0.1252, // opt5     3y
						0.1702, 0.1450, 0.00, 0.1261, 0.1257, // 		
						0.1710, 0.1452, 0.00, 0.1261, 0.1257, // 
						0.1681, 0.1409, 0.00, 0.1247, 0.1250, // opt6     4y
						0.1689, 0.1411, 0.00, 0.1246, 0.1250, // 		
						0.1697, 0.1414, 0.00, 0.1246, 0.1250, // 
						0.1640, 0.1373, 0.00, 0.1210, 0.1212, // opt7     5y
						0.1656, 0.1381, 0.00, 0.1214, 0.1217, // 		
						0.1663, 0.1384, 0.00, 0.1214, 0.1217, // 
						0.1532, 0.1311, 0.00, 0.1124, 0.1124, // opt8     7y
						0.1534, 0.1308, 0.00, 0.1119, 0.1120, // 		
						0.1519, 0.1293, 0.00, 0.1104, 0.1105 }; 

	//Size noSwSmile=3, noOpSmile=2, noSmileSpreads=5; // put number of swap tenors, option tenors and strike spreads for smile here (RB)
	Size noSwSmile=3, noOpSmile=8, noSmileSpreads=5; // put number of swap tenors, option tenors and strike spreads for smile here (BBG)

	std::vector<Period> swapTenorsSmile, optionTenorsSmile;
	std::vector<Real> smileSpreads;
	std::vector<std::vector<Handle<Quote> > > swaptionsmilequotes;

	for(int i=0;i<noSwSmile;i++) swapTenorsSmile.push_back(swapTenorsSmileVal[i]);
	for(int i=0;i<noOpSmile;i++) optionTenorsSmile.push_back(optionTenorsSmileVal[i]);
	for(int i=0;i<noSmileSpreads;i++) smileSpreads.push_back(smileSpreadVal[i]);
	for(int i=0;i<noSwSmile*noOpSmile;i++) {
		std::vector<Handle<Quote> > qSwSmileTmp;
		for(int j=0;j<noSmileSpreads;j++) { 
			//qSwSmileTmp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(smileVal[i*noSmileSpreads+j]))));		// RB Smile Data
			qSwSmileTmp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(j==static_cast<int>(noSmileSpreads/2) ? 0.0 : smileValAbs[i*noSmileSpreads+j]-swaptionatmvalues[i]))));		// BBG Smile Data (set atm = 0, calculate vol spreads to atm)
		}
		swaptionsmilequotes.push_back(qSwSmileTmp);
	}

	boost::shared_ptr<SwapIndex> swapIndex(new EuriborSwapIsdaFixA(30*Years,Handle<YieldTermStructure>(yts)));
	boost::shared_ptr<SwapIndex> shortSwapIndex(new EuriborSwapIsdaFixA(1*Years,Handle<YieldTermStructure>(yts)));

	boost::shared_ptr<SwaptionVolatilityStructure> swaptionCube(new SwaptionVolCube2(Handle<SwaptionVolatilityStructure>(swaptionVolAtm),optionTenorsSmile, swapTenorsSmile,
		smileSpreads,swaptionsmilequotes,swapIndex,shortSwapIndex,false)); // linear interpolated cube

	// times, accruals, forwards setup

	Date termDate=TARGET().advance(settlDate,30*Years);

	Schedule forwardSched( Date(15,January,2009), Date(15,January,2035), 6*Months, TARGET(), Following, Following, DateGeneration::Forward, false );

	std::vector<Real> rateTimes(forwardSched.size());
	DayCounter forwardsDc = Actual365Fixed(); // daycount w.r.t. which forwards are expressed !
    for (Size i=0; i<forwardSched.size(); ++i) {
        rateTimes[i] = forwardsDc.yearFraction(evalDate, forwardSched[i]);
	}

	std::vector<Real> todaysForwards(rateTimes.size()-1);
	std::vector<Real> displacements(rateTimes.size()-1);
	std::vector<Real> volatilities(rateTimes.size()-1);	
	for(int i=0;i<todaysForwards.size();i++) {
		todaysForwards[i] = yts->forwardRate(rateTimes[i],rateTimes[i+1],Simple,Annual);
		displacements[i] = 0.10; // put displacements here (just dummy values, will be overwritten later)
		volatilities[i] = 1.0;   // put vol adjustments here (just dummy values, will be overwritten later)
	}

	// calibration error evaluation, model must be set here

	struct CalibrationError {

		CalibrationError(const boost::shared_ptr<YieldTermStructure>& yts, const boost::shared_ptr<SwaptionVolatilityStructure>& vcube,
						 const std::vector<Real>& rateTimes, const std::vector<Real>& todaysForwards) : 
							yts_(yts), vcube_(vcube), rateTimes_(rateTimes), todaysForwards_(todaysForwards) {

			const Size step=2; // swap fixed leg is covering step periods of forwards rates

			const Size swpFixIdx[] = { 0,0,0,0,0, 2,2,2,2,2, 4,4,4,4,4, 6,6,6,6,6, 8,8,8,8,8, 10,10,10,10,10, 12,12,12,12,12 };
			const Size swpEndIdx[] = { 52,52,52,52,52, 52,52,52,52,52, 52,52,52,52,52, 52,52,52,52,52, 52,52,52,52,52, 52,52,52,52,52, 52,52,52,52,52 };
			const Real atmOffset[] = { -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02 };

			Size noCalInstr = 35;

			LMMCurveState curve0(rateTimes);
			curve0.setOnForwardRates(todaysForwards);
			Real d0 = yts->discount(rateTimes[0]);

			for(Size i=0; i<noCalInstr ; i++) {
				strikes_.push_back(curve0.swapRate(swpFixIdx[i],swpEndIdx[i],step)+atmOffset[i]);
				forwards_.push_back(curve0.swapRate(swpFixIdx[i],swpEndIdx[i],step));
				annuities_.push_back(d0*curve0.swapAnnuity(0,swpFixIdx[i], swpEndIdx[i], step));
				fixingTimes_.push_back(rateTimes[swpFixIdx[i]]);
				boost::shared_ptr<StrikedTypePayoff> payoff(new PlainVanillaPayoff(Option::Call,strikes_[i]));
				MultiStepSwaption calInstr(rateTimes,swpFixIdx[i],swpEndIdx[i],payoff,step);
				marketVols_.push_back(vcube_->volatility(fixingTimes_[i],swpEndIdx[i]/2.0,strikes_[i]));
				marketPrices_.push_back(blackFormula(Option::Call,strikes_[i],forwards_[i],sqrt(fixingTimes_[i])*marketVols_[i],annuities_[i]));
				weights_.push_back(1.0);
				calibrationInstruments_.add(calInstr);
			}

			calibrationInstruments_.finalize();

			// evolution
			evolution_ = calibrationInstruments_.evolution();
			// numeraires
			numeraires_ = terminalMeasure(evolution_);
			// correlation (is fixed!)
			Matrix corrMatrix = exponentialCorrelations(rateTimes,LTCORR,CORRDECAY);
			corr_ = boost::shared_ptr<PiecewiseConstantCorrelation>(new TimeHomogeneousForwardCorrelation(corrMatrix,evolution_.rateTimes()));
			// rescale weights so that sum of sqaure is 1.0
			Real sumw=0.0;
			for(int i=0;i<weights_.size();i++) sumw += weights_[i]*weights_[i];
			for(int i=0;i<weights_.size();i++) weights_[i] /= sqrt(sumw);
		}

		Disposable<Array> operator()(const std::vector<Real>& displacements, const std::vector<Real>& volatilities, const Real a, const Real b, const Real c, const Real d,
						const Real volvol, const Real meanRev, const Size paths=CALIBRATIONPATHS) const { // put default paths here (used in calibration)

			boost::shared_ptr<MarketModel> model(new AbcdVol(a,b,c,d,volatilities,corr_,evolution_,FACTORS,todaysForwards_,displacements)); // factors = 10
			long seed = SEED;
			//MTBrownianGeneratorFactory generatorFactory(seed);
			SobolBrownianGeneratorFactory generatorFactory(SobolBrownianGenerator::Diagonal, seed);

			// SBGM
			//boost::shared_ptr<MarketModelEvolver> evolver(new LogNormalFwdRatePc(model, generatorFactory,numeraires_,0)); // what does initial step mean?
			//boost::shared_ptr<MarketModelEvolver> evolver(new LogNormalFwdRateEuler(model, generatorFactory,numeraires_,0)); // what does initial step mean?
			// FLSV
			boost::shared_ptr<MarketModelVolProcess> volProcess(new SquareRootAndersen(1.0,meanRev,volvol,1.0,evolution_.evolutionTimes(),8,0.5,0.5,1.5)); 
			boost::shared_ptr<MarketModelEvolver> evolver(new SVDDFwdRatePc(model, generatorFactory, volProcess, 2, 2, numeraires_)); 

			Size initialNumeraire = evolver->numeraires().front();
			Real initialNumeraireValue = yts_->discount(rateTimes_[initialNumeraire]);

			AccountingEngine engine(evolver, calibrationInstruments_, initialNumeraireValue);
			boost::shared_ptr<SequenceStatisticsInc> stats(new SequenceStatisticsInc(calibrationInstruments_.numberOfProducts()));
			engine.multiplePathValues(*stats, paths);

			Array result(calibrationInstruments_.size());
			std::cout << "# fixingTime strike price(market) price(model) vol(market) vol(model) weight" << std::endl;
			for(Size i=0; i<calibrationInstruments_.size(); i++) {
				Real modelp = stats->mean()[i]; // model price
				Real modelv = 0.0; // implied vol
				try { modelv = blackFormulaImpliedStdDev(Option::Call,strikes_[i],forwards_[i],modelp,annuities_[i]) / sqrt(fixingTimes_[i]);
				} catch(...) {}
				Real marketp = marketPrices_[i]; // market price
				Real marketv = marketVols_[i]; // market vol
				result[i] = weights_[i]*(modelp-marketp); // measure error in terms of price difference
				std::cout << i << " " << fixingTimes_[i] << " " << strikes_[i] << " " << marketp << " " << modelp << " " << marketv << " " << modelv << " " << weights_[i] << std::endl;
			}

			return result;
		}

		const boost::shared_ptr<YieldTermStructure>& yts_;
		const boost::shared_ptr<SwaptionVolatilityStructure>& vcube_;
		const std::vector<Real>& rateTimes_;
		const std::vector<Real>& todaysForwards_;
		MultiProductComposite calibrationInstruments_;
		EvolutionDescription evolution_;
		std::vector<Real> marketPrices_, marketVols_, strikes_, forwards_, annuities_, fixingTimes_, weights_;
		std::vector<Size> numeraires_;
		boost::shared_ptr<PiecewiseConstantCorrelation> corr_;
	};

	CalibrationError calErr(yts,swaptionCube,rateTimes,todaysForwards);

	// this is taken from ABCD calibration
	
	class AbcdParametersTransformation : public ParametersTransformation {
                 mutable Array y_;
                 const Real eps1_;
         public:

            AbcdParametersTransformation() : y_(Array(4)),
                eps1_(.000000001){ }

            Array direct(const Array& x) const {
                y_[0] = x[0]*x[0] - x[3]*x[3] + eps1_;  // a + d > 0
                y_[1] = x[1];
                y_[2] = x[2]*x[2]+ eps1_;               // c > 0
                y_[3] = x[3]*x[3]+ eps1_;               // d > 0
                return y_;
            }

            Array inverse(const Array& x) const {
                y_[0] = std::sqrt(x[0] + x[3]- eps1_);
                y_[1] = x[1];
                y_[2] = std::sqrt(x[2]- eps1_);
                y_[3] = std::sqrt(x[3]- eps1_);
                return y_;
            }
    };

	// cost function to calibrate abcd and constant displacement for all rates
	// vector x = (a',b',c',d', d0', dinf', dl', v0', vinf', vl', volvol', meanRev')
	// where a',b',c',d' are transformed via AbcdParametersTransformation and
	// d0', dinf', dl', v0', vinf', vl', volvol', meanRev' are transformed via y=x^2
	// and displacements = dinf + (d0-dinf) * exp(-dl * #libor)
	// and volatilities = vinf + (v0-vinf) * exp(-vl * #libor)
	
	class CalAbcdDis : public CostFunction {

	public: 
		CalAbcdDis(CalibrationError* calError, Size numberOfForwards) : calError_(calError), numberOfForwards_(numberOfForwards) {}

		Real value(const Array& x) const {
			Array z = values(x);
			Real res = 0.0;
			for(int i=0;i<z.size();i++) {
				res+=z[i]*z[i];
			}
			//res/=z.size(); // done via weights
			return sqrt(res);
		}

        Disposable<Array> values(const Array& x) const {
			std::vector<Real> displacements(numberOfForwards_); 
			std::vector<Real> volatilities(numberOfForwards_);  
			AbcdParametersTransformation trans;
			Array y = trans.direct(x);
			Real d0,dinf,dl,v0,vinf,vl,volvol,meanrev;
			d0=x[4]*x[4];
			dinf=x[5]*x[5];
			dl=x[6]*x[6];
			v0=x[7]*x[7];
			vinf=x[8]*x[8];
			vl=x[9]*x[9];
			d0=std::min(d0,1.0);
			dinf=std::min(dinf,1.0);
			v0=std::min(v0,2.0);
			vinf=std::min(vinf,2.0);
			volvol=1.25; //x[10]*x[10];   /// FIX THE VOLVOL !!!
			meanrev=x[11]*x[11];
			for(int i=0;i<numberOfForwards_;i++) {
				Real idx = (double)i / (double)numberOfForwards_;
				displacements[i] = dinf+exp(-idx*dl)*(d0-dinf);
				volatilities[i] = vinf+exp(-idx*vl)*(v0-vinf);
			}
			std::cout << "Trying (a,b,c,d)=(" << y[0] << "," << y[1] << "," << y[2] << "," << y[3] << ")" << std::endl;
			std::cout << "(d0,dinf,dl)=(" << d0 << "," << dinf << "," << dl << "), (v0,vinf,vl)=(" << v0 << "," << vinf << "," << vl << ")" << std::endl;
			std::cout << "(volvol,meanrev)=(" << volvol << "," << meanrev << ")" << std::endl;
			std::cout << "Displacements: "; for(int i=0;i<numberOfForwards_;i++) std::cout << displacements[i] << ","; std::cout << std::endl;
			std::cout << "Phis: "; for(int i=0;i<numberOfForwards_;i++) std::cout << volatilities[i] << ","; std::cout << std::endl;
			Array error = calError_->operator()(displacements,volatilities,y[0],y[1],y[2],y[3],volvol,meanrev);
			Real sumErr=0.0;
			for(Size i=0;i<error.size();i++) {
				sumErr+=error[i]*error[i];
			}
			//sumErr/=error.size(); // done via weights
			std::cout << "Error: " << sqrt(sumErr) << std::endl;
			std::cout << "--------------------------------------------------------------------------------------------------------" << std::endl;
			return error;
		}

	private:
		CalibrationError* calError_;
		Size numberOfForwards_;

	};

	// calibrate the model !
	if(calibrate) {
		std::cout << "CALIBRATION STARTED" << std::endl;
		CalAbcdDis calCost(&calErr,todaysForwards.size());
		NoConstraint noConstraint;
		AbcdParametersTransformation trans;
		// vol abcd
		Array startAbcd(4);
		startAbcd[0] = 0.0526421; startAbcd[1] = -0.11285; startAbcd[2] = 0.655273; startAbcd[3] = 0.0371576; // START VALUES CALIBRATION abcd
		Array startAbcdInv = trans.inverse(startAbcd);
		Array start(4+3+3+2); // 4 abcd, 3 displacement, 3 voladjusterrs, 2 volvol,meanrev
		start[0] = startAbcdInv[0]; start[1] = startAbcdInv[1]; start[2] = startAbcdInv[2]; start[3] = startAbcdInv[3];
		start[4] = sqrt(0.641966); start[5] = sqrt(1.49192E-5); start[6] = sqrt(3.49002); // START VALUES transformed dinf, d0, dl, vinf, v0, vl 
		start[7] = sqrt(1.46214); start[8] = sqrt(2.02632); start[9] = sqrt(4.288867);
		start[10] = sqrt(1.25); start[11] = sqrt(0.15); // START VALUES volvol, meanrev 
		Problem calProblem(calCost,noConstraint,start);
		EndCriteria endC(2500,40,1E-14,1E-6,1E-6);
		LevenbergMarquardt opt;
		//Simplex opt(0.01);
		opt.minimize(calProblem,endC);
		// set calibrated values
		Array result = calProblem.currentValue();
		Array dres = trans.direct(result);
		Real a=dres[0]; Real b=dres[1]; Real c=dres[2]; Real d=dres[3];
		Real d0=result[4]*result[4]; Real dinf=result[5]*result[5]; Real dl=result[6]*result[6];
		Real v0=result[7]*result[7]; Real vinf=result[8]*result[8]; Real vl=result[9]*result[9];
		Real volvol=result[10]*result[10]; Real meanrev=result[11]*result[11];
		std::cout << "Calibration result:" << std::endl;
		std::cout << "a,b,c,d = " << a << "," << b << "," << c << "," << d << std::endl;
		std::cout << "d0,dinf,dl = " << d0 << "," << dinf << "," << dl << std::endl;
		std::cout << "v0,vinf,vl = " << v0 << "," << vinf << "," << vl << std::endl;
		std::cout << "volvol, meanrev = " << volvol << "," << meanrev << std::endl;
	}

	// price the otc swap !
	if(price) {
		std::cout << "PRICING STARTED" << std::endl;

		//result 1 (FLSV, 5000 paths)
		//a,b,c,d = 0.146641,-0.0838546,0.878426,0.029039
		//d0,dinf,dl = 0.192274,0.075661,1.07315
		//v0,vinf,vl = 1.48443,0.950788,1.03851
		//volvol, meanrev = 0.531083,0.31118
		/*Real a=0.146641; Real b=-0.0838546; Real c=0.878426; Real d=0.029039;
		Real d0=0.192274; Real dinf=0.075661; Real dl=1.07315;
		Real v0=1.48443; Real vinf=0.950788; Real vl=1.03851;
		Real volvol=0.531083; Real meanrev=0.31118;*/
		
		//result 2 (FLSV, refinement of result 1 with 100000 paths)
		/*a,b,c,d = 0.139554,-0.147794,1.18073,0.0289232
		d0,dinf,dl = 0.236085,0.070035,1.07391
		v0,vinf,vl = 1.61279,0.762128,1.44591
		volvol, meanrev = 0.115108,0.0156075*/
		Real a=0.139554; Real b=-0.147794; Real c=1.18073; Real d=0.0289232; //Real d=0.0389232; // shifted d for vega calculation
		Real d0=0.236085; Real dinf=0.070035; Real dl=1.07391;
		Real v0=1.61279; Real vinf=0.762128; Real vl=1.44591;
		Real volvol=1.25; Real meanrev=0.0156075;

		//result 3 (FLSV, calibration with different start values		
	        // Real a=0.0549993; Real b=-0.10386; Real c=0.71529; Real d=0.0376729;
		// Real d0=0.689256; Real dinf=0.0525882; Real dl=3.6251;
	        // Real v0=1.06682; Real vinf=2.02632; Real vl=8.45854e-5;
		// Real volvol=0.176627; Real meanrev=0.0350293;

		//result 4 (FLSV, calibration with fixed volvol)
		// a,b,c,d = 0.0526539,-0.112852,0.654989,0.0371449
		// d0,dinf,dl = 0.640446,1.57354e-06,3.49814
		// v0,vinf,vl = 1.4618,2.02632,4.2133
		// volvol, meanrev = 1.25,0.157441
		// Real a = 0.0526539; Real b = -0.112852; Real c = 0.654989; Real d = 0.0371449;
		// Real d0 = 0.640446; Real dinf = 1.57354e-06; Real dl = 3.49814;
		// Real v0 = 1.4618; Real vinf = 2.02632; Real vl = 4.2133;
		// Real volvol = 1.25; Real  meanrev = 0.157441;

		//test
		/*Real a=0.0; Real b=0.0; Real c=0.0; Real d=0.10;
		Real d0=0.0; Real dinf=0.0; Real dl=0.0;
		Real v0=1.0; Real vinf=1.0; Real vl=0.0;
		Real volvol=0.115108; Real meanrev=0.0156075;*/

		for(int i=0;i<todaysForwards.size();i++) {
			Real idx = (double)i / (double)todaysForwards.size();
			displacements[i] = dinf+exp(-idx*dl)*(d0-dinf);
			volatilities[i] = vinf+exp(-idx*vl)*(v0-vinf);
		}

		// check calibration with #paths as in pricing
		calErr(displacements,volatilities,a,b,c,d,volvol,meanrev, PRICINGPATHS);

		// setup deal 5686
		Schedule obsSched(Date(15,October,2008), Date(15,January,2015), 3*Months, TARGET(), ModifiedFollowing, ModifiedFollowing, DateGeneration::Forward, false);

		std::vector<Real> obsTimes,paymentTimes,floatingAccruals,structuredAccruals;
		std::vector<Size> structuredFixingIndices, floatingFixingIndices, structuredPaymentIndices, floatingPaymentIndices;
		Size payIdx=0;
		//Real sumFlPv=0.0; // for pv analytical
		//Size todfwdidx=0; // for comparision with todays fwds
		for(Size i=0;i<obsSched.size();i++) {
			Real t = forwardsDc.yearFraction(evalDate,obsSched[i]);
			obsTimes.push_back(t);
			if( (i-1)%4 == 0 ) {
				structuredFixingIndices.push_back(i);
				if( i >= 1 ) {
					structuredAccruals.push_back( Actual360().yearFraction( (i == 1 ? Date(15,January,2008) : obsSched[i-4]) ,obsSched[i]) ); // here act360 yf is correct!
					structuredPaymentIndices.push_back(payIdx);
				}
			}
			if( (i-1)%2 == 0 ) {
				if( i < obsSched.size()-1) floatingFixingIndices.push_back(i);
				paymentTimes.push_back(t);
				if( i > 1 ) {
					floatingAccruals.push_back( forwardsDc.yearFraction(obsSched[i-2],obsSched[i]) );
					floatingPaymentIndices.push_back(payIdx);
					//manually compute npv of leg
					//Real flrate=yts->forwardRate(obsSched[i-2],6*Months,forwardsDc/*Actual360()*/,Compounding::Simple);
					//Real flamount=(200000000.0*flrate*forwardsDc/*Actual360()*/.yearFraction(obsSched[i-2],obsSched[i]));
					//std::cout << obsSched[i-2] << " to " << obsSched[i] << " fixing " << flrate << " todaysforward " << todaysForwards[todfwdidx++] << " amount " << flamount << " pv " << flamount*yts->discount(obsSched[i]) << std::endl;  
					//sumFlPv+=flamount*yts->discount(obsSched[i]);
				}
				payIdx++;
			}
		}

		//std::cout << "Analytical pv = " << sumFlPv << std::endl;

		// test outputs
		/*std::cout << "-------------------------------------------------------------------------------" << std::endl;
		std::cout << "ratetimes:" << std::endl;
		for(int i=0;i<rateTimes.size();i++) std::cout << i << " " << rateTimes[i] << std::endl;
		std::cout << "obstimes:" << std::endl;
		for(int i=0;i<obsTimes.size();i++) std::cout << i << " " << obsTimes[i] << std::endl;
		std::cout << "paymenttimes:" << std::endl;
		for(int i=0;i<paymentTimes.size();i++) std::cout << i << " " << paymentTimes[i] << std::endl;
		std::cout << "structuredAccruals:" << std::endl;
		for(int i=0;i<structuredAccruals.size();i++) std::cout << i << " " << structuredAccruals[i] << std::endl;
		std::cout << "floatingAccruals:" << std::endl;
		for(int i=0;i<floatingAccruals.size();i++) std::cout << i << " " << floatingAccruals[i] << std::endl;
		std::cout << "structuredFixingIndices:" << std::endl;
		for(int i=0;i<structuredFixingIndices.size();i++) std::cout << i << " " << structuredFixingIndices[i] << std::endl;
		std::cout << "floatingFixingIndices:" << std::endl;
		for(int i=0;i<floatingFixingIndices.size();i++) std::cout << i << " " << floatingFixingIndices[i] << std::endl;
		std::cout << "structuredPaymentIndices:" << std::endl;
		for(int i=0;i<structuredPaymentIndices.size();i++) std::cout << i << " " << structuredPaymentIndices[i] << std::endl;
		std::cout << "floatingPaymentIndices:" << std::endl;
		for(int i=0;i<floatingPaymentIndices.size();i++) std::cout << i << " " << floatingPaymentIndices[i] << std::endl;
		std::cout << "-------------------------------------------------------------------------------" << std::endl;*/

		std::vector<Real> refRateFixings;
		refRateFixings.push_back(0.049533);
		refRateFixings.push_back(0.050277);
		refRateFixings.push_back(0.050838);
		refRateFixings.push_back(0.050046);
		refRateFixings.push_back(0.050918);
		refRateFixings.push_back(0.047088);
		refRateFixings.push_back(0.044745);
		refRateFixings.push_back(0.043695);

		MultiStepVolSwap swap(rateTimes,obsTimes,paymentTimes,structuredAccruals,floatingAccruals,structuredFixingIndices,floatingFixingIndices,structuredPaymentIndices,floatingPaymentIndices,
			0.06465,-5.0,0.02,std::pair<Size,Size>(12,52),2,refRateFixings,true);

		// TEST swaption
		//structuredFixingIndices.clear();
		//structuredFixingIndices.push_back(24);
		//structuredPaymentIndices.clear();
		//structuredPaymentIndices.push_back(12);
		//std::vector<Real> fixingTimes;
		//for(Size i=0;i<rateTimes.size()-1;i++) {
		//	fixingTimes.push_back(rateTimes[i]-0.25);
		//	fixingTimes.push_back(rateTimes[i]);
		//}
		//MultiStepVolSwap swap(rateTimes,fixingTimes,paymentTimes,structuredAccruals,floatingAccruals,structuredFixingIndices,floatingFixingIndices,structuredPaymentIndices,floatingPaymentIndices,
		//	0.0498266,1.0,0.01,std::pair<Size,Size>(12,52),2,refRateFixings,true);
		// TEST swaption END

		// evolution
		EvolutionDescription evolution = swap.evolution();
		// numeraires
		std::vector<Size> numeraires = terminalMeasure(evolution);
		//std::vector<Size> numeraires = moneyMarketMeasure(evolution);
		// correlation (is fixed!)
		Matrix corrMatrix = exponentialCorrelations(rateTimes,LTCORR,CORRDECAY);
		boost::shared_ptr<PiecewiseConstantCorrelation> corr(new TimeHomogeneousForwardCorrelation(corrMatrix,evolution.rateTimes()));
		boost::shared_ptr<MarketModel> model(new AbcdVol(a,b,c,d,volatilities,corr,evolution,FACTORS,todaysForwards,displacements)); // factors = 10
		long seed = SEED;
		Size paths = PRICINGPATHS;
		//MTBrownianGeneratorFactory generatorFactory(seed);
		SobolBrownianGeneratorFactory generatorFactory(SobolBrownianGenerator::Diagonal, seed);
		// SBGM
		//boost::shared_ptr<MarketModelEvolver> evolver(new LogNormalFwdRatePc(model, generatorFactory,numeraires,0)); 
		//boost::shared_ptr<MarketModelEvolver> evolver(new LogNormalFwdRateEuler(model, generatorFactory,numeraires,0)); 
		// FLSV
		boost::shared_ptr<MarketModelVolProcess> volProcess(new SquareRootAndersen(1.0,meanrev,volvol,1.0,evolution.evolutionTimes(),8,0.5,0.5,1.5)); // last 4 parameters here ?
		boost::shared_ptr<MarketModelEvolver> evolver(new SVDDFwdRatePc(model, generatorFactory, volProcess, 2, 2, numeraires)); // what does 2, 2 mean?

		Size initialNumeraire = evolver->numeraires().front();
		Real initialNumeraireValue = yts->discount(rateTimes[initialNumeraire]);

		// single cashflow npvs structured leg (float amount in multistepvolswap must be set to 0)
		//Real totalNpv=0.0, totalStd=0.0;
		//for(Size p=0;p<7;p++) {
		//	swap.filterStructuredIndex(p);
		//	AccountingEngine engine(evolver, swap, initialNumeraireValue);
		//	boost::shared_ptr<SequenceStatisticsInc> stats(new SequenceStatisticsInc(swap.numberOfProducts()));
		//	engine.multiplePathValues(*stats, paths);
		//	Real npv0 = stats->mean()[0];
		//	Real std0 = stats->standardDeviation()[0] / sqrt((Real)paths);
		//	Real npvt = 200000000.0 * npv0; //+ fixedFloatPymt;
		//	Real stdt = 200000000.0 * std0;
		//	std::cout << p << " npv0=" << npv0 << " npv = " << npvt << " std = " << stdt << std::endl;
		//	totalNpv +=npvt; totalStd +=stdt;
		//}
		//std::cout << "total=" << totalNpv << " std=" << totalStd << std::endl;

		// float payment 15.01.2009: 5259333.33
		Real fixedFloatPymt = 5259333.33 * yts->discount(Date(15,January,2009));

		AccountingEngine engine(evolver, swap, initialNumeraireValue);
		boost::shared_ptr<SequenceStatisticsInc> stats(new SequenceStatisticsInc(swap.numberOfProducts()));
		engine.multiplePathValues(*stats, paths);
		Real npv0 = stats->mean()[0];
		Real std0 = stats->standardDeviation()[0] / sqrt((Real)paths);
		Real npvt = 200000000.0 * npv0 + fixedFloatPymt;
		Real stdt = 200000000.0 * std0;
		std::cout << std::setprecision(24) << "npv = " << npvt << " std = " << stdt << std::endl;

	}

	return 0;

}
