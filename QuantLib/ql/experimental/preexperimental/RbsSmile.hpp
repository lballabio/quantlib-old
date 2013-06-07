/*! \file RbsSmile.hpp
    \brief parametric smile (rbs paper)
	Peter Caspers 
*/

#include <ql/quantlib.hpp>

#ifndef quantlib_RbsSmile_hpp
#define quantlib_RbsSmile_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! parametric smile for extrapolation */
	
	class RbsSmile {
		public:	
			/*! rbs smile is set up by absolute left and right strike, assumed to contain atm,
			two prices (put for left, call for right boundary) and first and second order
			derivatives of price functions to match at boundary points. The parameter mu is for
			the left wing, the parameter nu for the right wing of the smile.
			Volatility is extrapolated flat left from minStrike. */
			RbsSmile(double leftBound, double rightBound,
				double leftPrice, double rightPrice,
				double left1d, double left2d, double right1d, double right2d,
				double mu, double nu,double minStrike=0.0001,double blackAccuracy=1.0E-6);

			/*! set up smile by model parameters
			    dummy variable is only to distinguish constructors ! */
			RbsSmile(double leftBound, double rightBound,
				double mu, double nu, double la, double lb, double lc, double ra, double rb, double rc,bool dummy,
				double minStrike=0.0001,double blackAccuracy=1.0E-6);

			/*! get extrapolated price, strike must be outside strike area,
			    for strike > atm call, for strike < atm put price is returned */
			double price(double strike);

			/*! get implied volatility */
			double impliedVola(double forward, double strike, double maturity, double annuity=1.0);

			/*! recalibrate model setting different parameters to new values */
			void recalibrate(double mu, double nu);
			void recalibrate(double leftPrice, double rightPrice, 
				double left1d, double left2d, double right1d, double right2d);
			void recalibrate(double mu, double nu, double leftPrice, double rightPrice,
				double left1d, double left2d, double right1d, double right2d);

			/*! get vector of parameters (mu, nu, la, lb, lc, ra, rb, rc) */
			vector<double> modelParameters();
		
		private:

			void update();

			double leftBound_, rightBound_, leftPrice_, rightPrice_;
			double left1d_, left2d_, right1d_, right2d_;
			double logLeft1d_, logLeft2d_, logRight1d_, logRight2d_;
			double mu_, nu_;
			double la_, lb_, lc_, ra_, rb_, rc_;

			double minStrike_,blackAccuracy_;

	};

}


#endif

