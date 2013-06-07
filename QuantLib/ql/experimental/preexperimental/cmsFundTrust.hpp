/*! \file cmsFundTrust.hpp
    \brief cms funding trust pricer - wrapper for excel export (Francis Duffy)
	Peter Caspers
*/

#include <ql/quantlib.hpp>

#include <qle/models/hullwhite1.hpp>
#include <boost/math/distributions/normal.hpp>
#include <math.h>
#include <qle/instruments/capfloorcmsswap.hpp>
#include <qle/pricingengines/treecapfloorcmsswapengine.hpp>
#include <qle/instruments/capfloorcmsswaption.hpp>
#include <qle/pricingengines/treecapfloorcmsswaptionengine.hpp>

#ifndef quantlib_cmsFundTrust_hpp
#define quantlib_cmsFundTrust_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! pricing for cms funding trust positions as developed by Francis Duffy */
	
	class CmsFundTrust {

		public:
			
			/*! constructor */
			CmsFundTrust(const boost::shared_ptr<YieldTermStructure> yts, const boost::shared_ptr<SwaptionVolatilityStructure> swVol, const double meanRev, const double cmsMeanRev);

			/*! get results */
			vector<double> result(string tag);
		
		private:
			
			void calculate();

			boost::shared_ptr<YieldTermStructure> yts_;
			boost::shared_ptr<SwaptionVolatilityStructure> swVol_;
			double meanRev_, cmsMeanRev_;

			map<string,vector<double>> results_;

	};

	namespace cmsFundTrustHelper {

		Real cmsSwapletIntegrand(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon, 
				Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, Real r);
	
		Real cmsIntegralValue(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon,
				Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, vector<Date> & volDates, vector<Real> & sigmas);

		Real solveVolFunc(boost::shared_ptr<QuantExt::HullWhite1> aHw1, boost::shared_ptr<CmsCoupon> aCmsCoupon,
				Real cmsCap, Real cmsFloor, Handle<YieldTermStructure> aTermStructure, Real aCmsRate, vector<Date> & volDates, 
				vector<Real> sigmas, Real aVol);

	}

}


#endif

