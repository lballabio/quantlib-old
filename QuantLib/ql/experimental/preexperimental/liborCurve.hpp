/*! \file liborCurve.hpp
    \brief liborCurve for use in libor market model (monte carlo, compute swap rates, discounts, ...)
	Peter Caspers */

#include <ql/quantlib.hpp>
#include <sigmaParametrization.hpp>
#include <betaParametrization.hpp>

#ifndef quantlib_liborCurve_hpp
#define quantlib_liborCurve_hpp

#define REFINEMENTSTEPS 2 // number of steps per rateTime-Interval
#define MINRATE 0.0010 // minimum possible interest rate

using namespace boost;
using namespace std;

namespace QuantLib {
	
	/*! LiborCurve class */
	class LiborCurve {
		public:
			/*! The LiborCurve is constructed by
			- rateTimes	grid of libor rate times
			- initialRates initial forward rates
			- sigma	volatility parameterization of libors
			- beta skew parameterization of libors
			- meanLevel	the long run mean of the variance process, usually 1.0
			- volVar	the volatility of the variance process
			- v0	the start value of the variance process, usually 1.0
			- simulation steps	the number of simulation steps (index of libor, until which the simulation is run), defaulted by number of libors, i.e. simulation runs through all libors
			*/
			LiborCurve(std::vector<Time>& rateTimes,std::vector<double>& initialRates,
				boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<PiecewiseConstantBeta> beta,
				Real meanLevel, // omega
				Real reversionSpeed, // theta
				Real volVar, // eta
				Real v0, // start value for variance process
				Size simulationSteps=0 // number of simulation steps, if 0 rateTimes.size()-1 is taken
				) : rateTimes_(rateTimes), initialRates_(initialRates), sigma_(sigma), beta_(beta),
				omega_(meanLevel), theta_(reversionSpeed), eta_(volVar), v0_(v0), N_(rateTimes.size()-1) {

					QL_REQUIRE(N_==initialRates_.size(), initialRates_.size() << " libors are given for " << rateTimes_.size() << " rate times.");

					rates_=vector<double>(N_,0.0);
					K_=sigma_->numberOfFactors_;
					factors_=vector<double>(N_,0.0);

					n_=vector<double>(K_+1,0.0);

					if(simulationSteps==0) simulationSteps=rateTimes.size()-1;
					sobolBg_=boost::shared_ptr<SobolBrownianGenerator>(new SobolBrownianGenerator(K_+1,simulationSteps*REFINEMENTSTEPS,SobolBrownianGenerator::Factors));
					mtBg_=boost::shared_ptr<MTBrownianGenerator>(new MTBrownianGenerator(K_+1,simulationSteps*REFINEMENTSTEPS));
					resetPath();
			}

			/*! set t=0 and rates to initial rates */
			void resetPath();
			/*! evolve rate curve by steps indices, return the weight for this path */
			double evolve(Size steps=1);
			/*! return current index (-1 if at t=0, i.e. "before" rateTimes[0]) */
			int currentIndex();
			/*! return current value of spot numeraire (only valid at rate times grid points) */
			Real spotNumeraire();
			/*! return current libor forward with given fixing index */
			Real forwardRate(Size fixing);
			/*! return current swap rate (only valid at rate times grid points) */
			Real swapRate(Size fixing, Size length, Size payFreq);
			/*! return current swap annuity w.r.t. reference (only valid at rate times grid points) */
			Real swapAnnuity(Size reference, Size fixing, Size length, Size payFreq);
			/*! return swap annuity w.r.t. initial rates */
			Real swapAnnuityT0(Size fixing, Size length, Size payFreq);
			/*! return current R-rate (Antonov paper), only valid at rate times grid points, w.r.t. reference index */
			Real rRate(Size reference, Size fixing, Size delay, Size length, Size payFreq);
			/*! return measure change process w.r.t. reference point (only valid at grid points) from swap rate as given by swapFixing, length, payFreq to terminal measure (swapFixing+payDelay) */
			Real swap2TerminalMeasure(Size reference, Size fixing, Size delay, Size length, Size payFreq);
			/*! return current discount factor between start index and end index */
			Real discountFactor(Size start, Size end);
			/*! return discount factor w.r.t. initial rates between start and index */
			Real discountFactorT0(Size start, Size end);
			/*! shift current state and initital Rates of rate curve by given amount */
			void shift(double offset);
			/*! shift forward libor belonging to fixing index by given amount, either for current state only (default) or both for current state and initial state */
			void shift(Size fixing, double offset, bool currentStateOnly=true);

			// DEBUG
			//double rRate_; 

		private:
			//Size nextResetIndex(Time t) const;

			boost::shared_ptr<AbcdVolRatioCorrSigma> sigma_;
			boost::shared_ptr<PiecewiseConstantBeta> beta_;
			std::vector<Time>& rateTimes_;
			std::vector<double>& initialRates_;
			Real omega_, theta_, eta_, v0_;
			Size N_; // number of libor rates
			Size K_; // number of factors
			int currentIndex_; // current index
			double t_; // current time
			std::vector<double> rates_; // current state
			Real v_; // current variance
			std::vector<double> n_; // normal variates
			std::vector<double> factors_; // factors by which rates are multiplied during evolvement
			InverseCumulativeNormal inverseCumulative_;
			MersenneTwisterUniformRng mt_;
			boost::shared_ptr<SobolBrownianGenerator> sobolBg_;
			boost::shared_ptr<MTBrownianGenerator> mtBg_;
			
	};


	
}

#endif

