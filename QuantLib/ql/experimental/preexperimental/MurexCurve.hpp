/*! \file MurexCurve.hpp
    \brief Murex Zero Curve
	Peter Caspers 
*/

#include <ql/quantlib.hpp>

#ifndef quantlib_murexCurve_hpp
#define quantlib_murexCurve_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! murex zero curve */
	
	class MurexCurve {
		public:	
			/*! murex zero curve 
			  interpolationMode = 0: linear in rate, =1: linear in rate*time */
			MurexCurve(Date referenceDate,
				vector<Date> maturities,
				vector<double> discounts,
				long interpolationMode);

			bool setDayCounter(DayCounter dc);
			DayCounter dayCounter();
			
			double rate(Date maturity);
			double rate(double t);
			double discount(Date maturity);
			double discount(double t);

		private:

			void calculate();

			DayCounter dc_;
			Date refDate_;
			vector<Date> maturities_;
			vector<double> discounts_, rates_, times_;
			int n_;
			Interpolation interpol_;
			long interpolationMode_;

	};

}

#endif


