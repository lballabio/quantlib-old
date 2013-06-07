/*! \file singleLookSpreadOption.hpp
    \brief Single Look Cms Spread Option
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <CmsPricer.hpp>
#include <CorrelationTermStructure.hpp>

#ifndef quantlib_singleLookSpreadOption_hpp
#define quantlib_singleLookSpreadOption_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! single look cms spread option */
	
	class SingleLookSpreadOption {
		public:	
			/*! cms spread option */
			SingleLookSpreadOption(Date fixing,Date payment,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				double strike,
				int flavour);
			
			double rate(bool first,bool adjusted);
			double spread();
			
			/*! get price for spread option */
			Real npv(boost::shared_ptr<CmsPricer> pricer, bool useDiffStrike, double diffStrike,
				int diffFlavour,bool usePrecomputedRates=false);

		private:
			Date fixing_,payment_;
			double rate1_,rate2_,adjustedRate1_,adjustedRate2_,spread_;
			boost::shared_ptr<SwapIndex> index1_,index2_;
			double strike_;
			int flavour_;
			bool areRatesComputed_;
			bool useDiffStrike_;
			double diffStrike_;
			int diffFlavour_;
	};
}

#endif


