/*! \file cmsPricer.hpp
    \brief pricer for cms, cms spread coupons
	NOTE: discount curve is used as estimation curve here! Different Curves are not possible at the moment!
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <correlationTermStructure.hpp>
//#include <sabrRbsSmile.hpp>

#ifndef quantlib_cmsPricer_hpp
#define quantlib_cmsPricer_hpp

#define CMSPLOGGING

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! cms pricer to value cms and cms spread coupons */
	
	class CmsPricer {
		public:	
			/*! cms pricer is set up by
			- discounting yield curve: this yc is used as estimation and discount curve in each case in this class 
			  (even if a given swap index has another estimation / discount curve)
			- (correlation term structure)
			- settlement date (npvs are calculated w.r.t. this date)
			*/
			/*CmsPricer(boost::shared_ptr<YieldTermStructure> discountYts, boost::shared_ptr<SabrRbsSmile> volCube,
				boost::shared_ptr<CorrelationTermStructure> corr, Date settlementDate);*/

			/*! second constructor for standard swaption vol cube
			*/
			CmsPricer(boost::shared_ptr<YieldTermStructure> discountYts, boost::shared_ptr<SwaptionVolatilityStructure> volCube2,
				boost::shared_ptr<CorrelationTermStructure> corr, Date settlementDate);

			/*! set calculation modes
			- convAdjMode = 0 classic, 1 hagan integral, 2 cms replication, 3 no adjustment 
			- cmssoMode = 0 atm vols, 1 partial smile
			- hermitePoints number of integration points gauss hermite
			*/
			bool setCalculationModes(int convAdjMode, int cmssoMode, long hermitePoints);

			/*! set hagan integral method parameter
			- haganLowerBound, haganUpperBound min and max bounds for hagan integration
			- haganMinPrice min price of swaption before integration is stopped
			- integralAcc accuracy of hagan integral
			- maxSteps max steps in integration
			*/
			bool setHaganIntegralParameters(double haganLowerBound, double haganUpperBound, double haganMinPrice, double integralAcc, long maxSteps);
			
			/*! set cms replication method parameter
			- meanReversion mean reversion parameter for hull white rate scenario
			- lowerStrike, upperStrike are used to determine the hull white scenarios in order to get near these strikes
			- hn is (for swaplets half the ) the number of swaptions in the replication portfolio
			- vegaRatio if not 0, the hull white scenarios are determined such that strikes are covered such that corresponding swaption vegas reach this value times the vega of the atm swaption vega
			- matchAcc accuracy for matching hull white scenario value to strike
			- maxStrike maximum strike used (if vegaRatio should suggest higher strikes the maximum strike is capped here)
			- maxStdDevVol if > 0.0 volatilities from the cube are only retrieved up to the strikes Forward x Exp( +/- 3 vol sqrt(tfix) ), flat extrapolation is used above / under these strikes
			*/
			bool setCmsReplicationParameters(double meanReversion, double lowerStrike, double upperStrike, 
				double vegaRatio, int hn, double matchAcc, double maxStrike, double maxStdDevVol);

			/*! get convexity adjustment for swap rate (strike = 0)
			    if strike > 0, effective rate for cap / floor (flavour 1, -1) is computed (only for replication method!!) */ 
			Real swapRateAdjustment(const Date& fixingDate, 
				const Date& paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex,
				const double strike=0.0,
				const int flavour=1
				);

			/*! get price for cms spread optionlet
			* flavour = 1 is call, flavour = -1 is put */
			Real cmssoPrice(Date calculationStartDate,
				Date calculationEndDate,
				Date fixingDate, 
				Date paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex1,
				boost::shared_ptr<SwapIndex> swapIndex2,
				DayCounter couponDayCounter,
				double strike, 
				int flavour,
				bool usePrevRates=false, double pr1=0.0, double pr2=0.0, double par1=0.0, double par2=0.0);

			/*! get price for cms spread single look option
			* flavour = 1 is call, flavour = -1 is put 
			  if usePrevRates true then previous atm rates and adjustments are used as given
			  by parameters pr (prev atm rate), par (prev adjusted rate) */
			Real cmssoPrice(Date fixing, Date payment,
				boost::shared_ptr<SwapIndex> swapIndex1,
				boost::shared_ptr<SwapIndex> swapIndex2,
				double strike, 
				int flavour,
				bool usePrevRates=false, double pr1=0.0, double pr2=0.0, double par1=0.0, double par2=0.0);

			/*! get price for cms coupon */
			Real cmsPrice(Date calculationStartDate,
				Date calculationEndDate,
				Date fixingDate, 
				Date paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				double strike=0.0,
				int flavour=1,
				double margin=0.0);
			
			/*! returns last (adjusted) rate(s) after pricing of cmsso or cms 
			    in case of cmsso first = true returns first swap rate, false second */
			double lastRate(bool first,bool adjusted) {
				if(adjusted) {
					if(first) return lastAdjustedRate1_; else return lastAdjustedRate2_;
				}
				else {
					if(first) return lastRate1_; else return lastRate2_;
				}
			}

			/*! returns last adjusted spread after pricing of cmsso */
			double lastSpread() {
				return lastAdjustedSpread_;
			}

			/*! returns last upper bound of hedge portfolio (first does not work here...) */
			double lastUpperBound(bool first) {
				if(first) return lastUpperBound_; else return lastUpperBound_;
			}

			boost::shared_ptr<CorrelationTermStructure> correlationTermStructure() {
				return corr_;
			}

			/*! return underlying discount yts */
			boost::shared_ptr<YieldTermStructure> yieldTermStructure() {
				return yts_;
			}

			/*! compute the diffOrder'th derivation of G at x according to Hagan, 2.13a (diffOrder=0 is default)
				t=tau=1/q, d=Delta, n */
			static double haganG(const double x,const double t, const double d, const Size n, const int diffOrder=0);
			
			double discountHullWhite(const Date& d1, const Date& d2, double h); // returns forward discount factor under hull white scenario given by h
			double swapRateHullWhite(const Date& dfix, const vector<Date>& fixedDates,const DayCounter& fdc, double h,double annuity=0.0); // returns forward swap rate under hull white scenario given by h
			double annuityHullWhite(const Date& dfix, const vector<Date>& fixedDates, const DayCounter& fdc,double h); //returns forward annuity of swap under hull white scenario given by h
		
		private:
			void initDefaultValues();
			void initHermiteIntegrator();
			double discount(const Date& d);		

			Date refDate;
			DayCounter vdc;
			boost::shared_ptr<YieldTermStructure> yts_;
			DayCounter dc_;
			Date settlementDate_;
			//boost::shared_ptr<SabrRbsSmile> volCube_;
			boost::shared_ptr<SwaptionVolatilityStructure> volCube2_;
			bool useStdCube_;
			GaussHermiteIntegration* ghInt_;
			int convAdjMode_,cmssoMode_;
			boost::shared_ptr<CorrelationTermStructure> corr_;
			double lastRate1_,lastRate2_,lastAdjustedRate1_,lastAdjustedRate2_,lastAdjustedSpread_;
			double lastUpperBound_;
			// parameters for hagan integral method
			double haganLowerBound_,haganUpperBound_,haganMinPrice_;
			long HAGANINTEGRALMAXSTEPS;
			double HAGANINTEGRALACC;
			// parameters for direct replication method
			double meanReversion_, lowerStrike_, upperStrike_,vegaRatio_;
			double hmin_, hmax_; // these are in fact calculated from lower Strike, upper Strike
			int hn_;
			double HMATCHACC;
			double MAXSTRIKE;
			double maxStdDevVol_;
			// parameters for spread option integration
			long CMSSOHERMITEPOINTS;
			
	};


	/*! Helper class for Cmsso calculation (Brigo, 13.16.2)
	    WARNING: swap rate numbering is swapped here ;-) (payoff here is max(w*mult2*swaprate2-w*mult1*swaprate1-w*strike,0)) */

	class CmssoFunction {
		public:
			CmssoFunction(const double swapRate1,const double swapRate2,const double adjustedSwapRate1,const double adjustedSwapRate2,
								 const double vol1,const double vol2,const double rho,const double strike,const double t,
								 const double mult1,const double mult2, const int flavor);
			double operator()(const double& x) const;
		
		private:
			double swapRate1_,swapRate2_;
			double adjustedSwapRate1_,adjustedSwapRate2_;
			double vol1_,vol2_;
			double rho_;
			double strike_;
			double t_;
			double mult1_,mult2_;
			int flavor_;
	};


	/*! Helper class for hagan convexity adjustment calculation (Brigo, 13.8.5)
	    if x > strikeVolCap then volatility of level strikevolCap is used */

	/*class HaganConvAdjFunction {
		public:
			HaganConvAdjFunction(const double& tau,const double& Delta,const Size& m,
				boost::shared_ptr<SabrRbsSmile> volCube,const Date& dSwapStart,const Time& tSwapStart,
				const Period& tSwpLng, const Rate& swapRate, const double strikeVolFloor, const double strikeVolCap);
			double operator()(double x) const;

		private:
			double tau_,Delta_;
			Size m_;
			boost::shared_ptr<SabrRbsSmile> volCube_;
			Date dSwapStart_;
			Time tSwapStart_;
			Period tSwpLng_;
			Rate swapRate_;
			double strikeVolCap_,strikeVolFloor_;
	};*/

	/*! Helper class to find scenario value h corresponding to a given swap rate */
	class HullWhiteScenarioHelper {
		public:
			HullWhiteScenarioHelper(CmsPricer *pricer, const Date& dFix,const vector<Date>& dates, const DayCounter& fdc,double targetSwapRate);
			double operator()(const double& x) const;
		
		private:
			CmsPricer *pricer_;
			const Date& dFix_;
			double targetSwapRate_;
			const vector<Date>& dates_;
			const DayCounter& fdc_;

	};

	/*! Helper class to find strikes that correspond to a given vega ratio to atm 
		if direction = 1 the upper strike is searched, direction = -1 the lower strike */
	class VegaRatioHelper {
		public:
			//VegaRatioHelper(SabrRbsSmile* volCube, const Date& dFix, const Period& tenor, const double forward, const int direction, const double targetRatio, const double maxStrike);
			VegaRatioHelper(SwaptionVolatilityStructure* volCube2, SmileSection* smileSection, const Date& dFix, const Period& tenor, const double forward, const int direction, const double targetRatio, const double maxStrike);
			double operator()(const double& x) const;
		private:
			double vega(const double& strike) const;
			//SabrRbsSmile *volCube_;
			SwaptionVolatilityStructure *volCube2_;
			SmileSection *smileSec_;
			bool useStdCube_;
			const Date& dFix_;
			const Period& tenor_;
			const int direction_;
			const double targetRatio_;
			double forward_;
			double tFix_;
			double MAXSTRIKE;
	};


}


#endif

