/*! \file calibrationHelperCms.hpp
    \brief Cms 
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <CmsPricer.hpp>
#include <SabrRbsSmile.hpp>
#include <SwaptionVolCube4.hpp>
#include <CmsSwap.hpp>
#include <simulatedAnnealing.hpp>

#ifndef quantlib_calibrationHelperCms_hpp
#define quantlib_calibrationHelperCms_hpp

//#define CMSHLOGGING

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! calibration helper cms correlations */
	
	class CalibrationHelperCms {
		public:	
			/*! constructor 
			swapLetNumbers: Number of swaplets in each group, e.g. 20/20/20/20 means 5y, 10y, 15y, 20y maturity if 4 coupons / year are paid
			margins: Margins of the cms swaps specified by swapletNumbers
			noOptPillars: number of option pillars (as defined in vol cube for beta, mu, nu) used for calibration (if 0 all option pillars are taken)
			undPillarIndex: underlying pillar index (as defined in vol cube for beta, mu, nu)
			*/
			CalibrationHelperCms(boost::shared_ptr<CmsSwap> cmsSwap,
				boost::shared_ptr<CmsPricer> pricer,
				vector<long> swapLetNumbers,
				vector<double> margins,
				boost::shared_ptr<SabrRbsSmile> volCube,
				int undPillarIndex,
				long noOptPillars=0
				);

			/*! alternative constructur for standard cube */
			CalibrationHelperCms(boost::shared_ptr<CmsSwap> cmsSwap,
				boost::shared_ptr<CmsPricer> pricer,
				vector<long> swapLetNumbers,
				vector<double> weights,
				vector<double> margins,
				boost::shared_ptr<SwaptionVolCube4> volCube,
				Period underlying,
				long noOptPillars=0,
				long skipOptPillars=0
				);

			/*! set parameters for calibration 
				optimizer=0 simplex, 1 lm 
				optAcc optimizer Accuracy
				maxIt max Iterations
				maxSt max stationary
				maxMu, maxNu maximum Value for parametric smile mu and nu
				minBeta, maxBeta min and max value for beta 
				mode beta: 0: linear 1: expon decay 2: single pillars 3: abcd
				     nu:   4: linear 5: expon decay 6: single pillars 7: abcd */
			bool setParameters(double optAcc,long maxIt,long maxSt,double maxMu,double maxNu,double minBeta, double maxBeta, int mode, int optimizer);
			/*! calibrates smiles and returns rmse */
			double calibrate(); 
			/*! return margin for group (including margins of lower groups computed before, must be called for each group in asc order) */
			double margin(int group);
			/*! return market price for optionlet group */
			vector<double> marketMargins();
			/*! return number of groups */
			int numberOfGroups();

		private:
			void initDefaultValues();
			boost::shared_ptr<CmsSwap> cmsSwap_;
			long noOptPillars_,skipOptPillars_;
			vector<double> optPillarTimes_;
			int undPillarIndex_;
			boost::shared_ptr<SabrRbsSmile> volCube_;
			boost::shared_ptr<SwaptionVolCube4> volCube2_;
			bool useStdCube_;
			boost::shared_ptr<CmsPricer> pricer_;
			vector<long> swpGroupEnds_;
			vector<double> marketMargins_;
			vector<double> modelMargins_;
			vector<double> weights_;
			double OPTACCURACY_,MAXCORR_,MINPRICE_;
			long MAXIT_,MAXST_;
			double maxNu_, maxMu_;
			int optimizer_;
			double minBeta_,maxBeta_;
			int mode_;
			Period underlying_;
	};

	/*! Cost Function for calibration method */
	class CmsCostFunction : public CostFunction {
		public:
			CmsCostFunction(CalibrationHelperCms *helper, SabrRbsSmile *volCube, int noOptPillars, vector<double> optPillarTimes, int undPillarIndex, double maxNu, double maxMu,
				double minBeta, double maxBeta, int mode);
			CmsCostFunction(CalibrationHelperCms *helper, SwaptionVolCube4 *volCube2, int noOptPillars, vector<double> optPillarTimes, int skipOptPillars, Period underlying, double maxNu, double maxMu,
				double minBeta, double maxBeta, int mode, vector<double> weights);
			Real value(const QuantLib::Array& x) const;
			Disposable<Array> values(const QuantLib::Array& x) const;

		private:
			CalibrationHelperCms* helper_;
			SabrRbsSmile* volCube_;
			SwaptionVolCube4* volCube2_;
			vector<double> optPillarTimes_;
			int noOptPillars_,skipOptPillars_,undPillarIndex_;
			vector<double> weights_;
			int m_;
			double maxNu_, maxMu_;
			double minBeta_,maxBeta_;
			int mode_;
			Period underlying_;
			bool useStdCube_;
	};

}

#endif
