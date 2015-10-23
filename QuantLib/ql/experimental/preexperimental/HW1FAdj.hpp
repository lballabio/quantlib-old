#include <ql/quantlib.hpp>
#include <math.h>

namespace QuantLib {

	class HW1FAdj {

	public:

		HW1FAdj(const double a, 
				const std::vector<double>& times,
				const std::vector<double>& sigma,
				boost::shared_ptr<YieldTermStructure> yieldTS);
	
		double expectationX(const Size index, const Size payIndex);
		double expectationY(const Size index, const Size payIndex);
		double varianceX(const Size index);
		double varianceY(const Size index);
		double covarianceXY(const Size index);
		double correlationXY(const Size index);
		double zeroCouponPrice(const Size startIndex, const Size endIndex, const double x, const double y, const double adjuster=1.0,const double startTimeMin=0.0, const double endTimeMax=500.0);
		double swapRate(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y,const double adjuster=1.0);
		double swapAnnuity(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y,const double adjuster=1.0);
		double varianceI(const int index0, const int index1,const double adjuster=1.0,const double startTimeMin=0.0, const double endTimeMax=500.0);

	private:

		double a_;
		std::vector<double> times_, sigma_;
		boost::shared_ptr<YieldTermStructure> yieldTS_;
		
	};

	

}