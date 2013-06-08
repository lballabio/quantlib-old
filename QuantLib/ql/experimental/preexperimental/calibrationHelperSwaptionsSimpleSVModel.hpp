/*! \file calibrationHelperSwaptionsSimpleSVModel.hpp
    \brief calibrates simple stoch vol models to given swaption smiles
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <simulatedAnnealing.hpp>
#include <simpleSVModel.hpp>
#include <exception>

#ifndef quantlib_calibrationHelperSwaptionsSimpleSVModel_hpp
#define quantlib_calibrationHelperSwaptionsSimpleSVModel_hpp

using namespace boost;
using namespace std;

namespace QuantLib {


	/*! This class is a helper class to do several calibrations of simple stoch vol models
	to given swaption smiles */
	
	
	class CalibrationHelperSwaptionsSimpleSVModel {
		public:	
			/*! calibration helper for swaptions in the simple sv model is set up by 
			- a yield term structure
			- a swaption vol cube
			- option and swap tenors describing the swaptions to calibrate the simple stoch vol model to (cross product is taken from this)
			- strikeSpreads (e.g. -0.02, -0.015, ... ) the spreads relative to atm defining the points on the smile which are used
			for calibration. These are the same for all swaptions. If atm + spread < 0, the swaption is ignored.
			The forward swap rates are then calculated using the parRate method from YieldTermStructure */
			CalibrationHelperSwaptionsSimpleSVModel(const boost::shared_ptr<YieldTermStructure> yts, 
				const boost::shared_ptr<SwaptionVolatilityCube> swaptionVolCube,
				const vector<Period>& optionTenors,
				const vector<Period>& swapTenors,
				const vector<double>& strikeSpreads);
			
			/*! performs the calibration (if not done already) using the start values
			- lambda volatility parameter of simple stoch vol model
			- b skew parameter of simple stoch vol model
			- theta mean reversion of variance process
			- eta volatility of variance process
			- lambdaFixed if true lambda is not calibrated
			- bFixed if true b is not calibrated
			- etaFixed if true eta is not calibrated
			- useImpliedVols if true implied vols are used instead of prices (divided by annuity) for calibration
			and returns a matrix containing the columns containing the results of the calibration
			- option tenor (time from reference point)
			- swap tenor (time from reference point)
			- atm forward
			- lambda
			- b
			- theta
			- eta
			- RMSE
			- (vector of market implied vols)
			- (vector of model implied vols)
			*/
			const Matrix& stochVolParameters(double lambda, double b, double theta, double eta, 
											    bool lambdaFixed, bool bFixed, bool thetaFixed, bool etaFixed,bool useImpliedVols);


		private:
			bool calibrationDone_;
			boost::shared_ptr<YieldTermStructure> yts_;
			boost::shared_ptr<SwaptionVolatilityCube> swaptionVolCube_;
			vector<Period> optionTenors_;
			vector<Period> swapTenors_;
			vector<double> strikeSpreads_;
			Matrix result_;
	};

}

#endif
