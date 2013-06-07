/*! \file calibrationHelperCmsCorrelations.hpp
    \brief Cms Spread Option
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <CmsPricer.hpp>
#include <CorrelationTermStructure.hpp>
#include <CmsSpreadOption.hpp>
#include <SingleLookSpreadOption.hpp>
#include <simulatedAnnealing.hpp>

#ifndef quantlib_calibrationHelperCmsCorrelations_hpp
#define quantlib_calibrationHelperCmsCorrelations_hpp



using namespace boost;
using namespace std;

namespace QuantLib {

	/*! calibration helper cms correlations */
	
	class CalibrationHelperCmsCorrelations {
		public:	
			/*! constructor for spread option series */
			CalibrationHelperCmsCorrelations(boost::shared_ptr<CmsSpreadOption> spreadOption,
				boost::shared_ptr<CmsPricer> pricer,
				long firstOptionlet,
				vector<long> optionLetNumbers,
				vector<double> prices,
				boost::shared_ptr<CorrelationTermStructure> corrTS,
				double lambda=0.0,
				bool useDiffStrike=false,
				double diffStrike=0.0, int diffFlavour=0
				);
			/*! constructor for single look options */
			CalibrationHelperCmsCorrelations(vector<boost::shared_ptr<SingleLookSpreadOption>> singleLookSpreadOptions,
				boost::shared_ptr<CmsPricer> pricer,
				vector<double> prices,
				boost::shared_ptr<CorrelationTermStructure> corrTS,
				double lambda=0.0,
				bool useDiffStrike=false,
				double diffStrike=0.0, int diffFlavour=0
				);

			/*! set parameters for calibration
			    optimizer=0 simplex, 1 lm, 2 sa */
			bool setParameters(double optAcc,long maxIt,long maxSt,double maxCorr,double minPrice,bool onlyPositiveCorr,int optimizer);
			/*! calibrates correlation term structure and returns rmse */
			double calibrate(); 
			/*! return price for optionlet group given */
			double price(int group);
			/*! return market price for optionlet group */
			double marketPrice(int group);
			/*! return number of groups */
			int numberOfGroups();

		private:
			void initDefaultValues();
			bool useSingleLook_;
			boost::shared_ptr<CmsSpreadOption> spreadOption_;
			bool useDiffStrike_;
			double diffStrike_;
			int diffFlavour_;
			boost::shared_ptr<CmsPricer> pricer_;
			long firstOptionlet_;
			vector<long> optGroupEnds_;
			vector<double> marketPrices_;
			vector<boost::shared_ptr<SingleLookSpreadOption>> singleLookSpreadOptions_;
			boost::shared_ptr<CorrelationTermStructure> corrTS_;
			double lambda_; // this parameter times sum of squares of differences between correlations is minimized as last value
			double OPTACCURACY_,MAXCORR_,MINPRICE_;
			long MAXIT_,MAXST_;
			bool ONLYPOSITIVECORR_;
			int optimizer_;
	};

	/*! Cost Function for calibration method */
	class CmsCorrelationCostFunction : public CostFunction {
		public:
			CmsCorrelationCostFunction(CalibrationHelperCmsCorrelations *helper, CorrelationTermStructure *corrTS,double lambda,double minPrice,bool onlyPositiveCorr,double MAXCORR);
			Real value(const QuantLib::Array& x) const;
			Disposable<Array> values(const QuantLib::Array& x) const;

		private:
			CalibrationHelperCmsCorrelations* helper_;
			CorrelationTermStructure* corrTS_;
			double lambda_;
			int n_,m_;
			double MINPRICE,MAXCORR_;
			bool ONLYPOSITIVECORR;
	};

	/*! Trivial constraint class for calibration of cms correlations */
	class CmsCorrelationConstraint : public Constraint {
            private:
            class Impl : public QuantLib::Constraint::Impl {
                public:
                bool test(const QuantLib::Array& params) const {
					return true; // the constraints are checked in the cost function
                }
            };
            public:
            CmsCorrelationConstraint()
				: Constraint(boost::shared_ptr<Constraint::Impl>(new Impl)) {}
	};

}

#endif
