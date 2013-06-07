/*! \file simulatedAnnealing.hpp
\brief Simulated Annealing optimization method
Simulated Annelaing Optimization Algorithm as described in "Numerical Recipes in C", Chapter 10.9
Implements the quantlib OptimizationMethod class 
Peter Caspers
*/


#ifndef quantlib_optimization_simulated_annealing_hpp
#define quantlib_optimization_simulated_annealing_hpp

#define MAX_DOUBLE 1E15 // big value for objective function that is used in case of NAN-values

#include <ql/quantlib.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

namespace QuantLib {

	/*! Simulated Annealing Optimizer Class */
	class SimulatedAnnealing : public OptimizationMethod {
	public:
		/*! Constructor taking as input the 
		- lambda characteristic length lambda (used to set up initial simplex by adding this value sequentially to the components of the start vector)
		- rngSeed seed for the random number generator used to produce the noise
		- iterationsBeforeTemperatureDecrease number of iterations before the temperature is decreased
		- iterationsBeforeTemperatureZero number of iterations before the temperature is set to zero
		- startTemperature start temperature
		- temperatureDecayFactor factor by which the temperature is decreased
		*/
		SimulatedAnnealing(Real lambda = 0.01, int rngSeed=0,Size iterationsBeforeTemperatureDecrease = 5,
			Size iteratopmsBeforeTemperatureZero = 25, Real startTemperature = 2.5,
			Real temperatureDecayFactor = 0.2) : 
		lambda_(lambda), seed(rngSeed), iterBeforeTempDec(iterationsBeforeTemperatureDecrease),
			iterBeforeTempZero(iteratopmsBeforeTemperatureZero),startTemp(startTemperature),
			tempDecayFactor(temperatureDecayFactor)  {
				//logFile=fopen("simulated_annealing.log","w");
		}
		virtual EndCriteria::Type minimize(Problem& P,
			const EndCriteria& endCriteria);
	private:
		double amotsa(Problem&,double); 
		Real lambda_;
		int seed;
		Size iterBeforeTempDec;
		Size iterBeforeTempZero;
		Real startTemp;
		Real tempDecayFactor;
		Real currentTemp;
		mutable std::vector<Array> vertices_;
		mutable Array values_, sum_;
		int i,ihi,ilo,j,m,n;
		double rtol,sum,swap,yhi,ylo,ynhi,ysave,yt,ytry;
		double yb;
		Array pb;
		double tt;
		MersenneTwisterUniformRng mt;
		//FILE* logFile;
	};

}

#endif
