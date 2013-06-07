/*! \file cmsSwap.hpp
    \brief Cms Swap
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <CmsPricer.hpp>

#ifndef quantlib_cmsSwap_hpp
#define quantlib_cmsSwap_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! cms swap */
	
	class CmsSwap {
		public:	
			/*! construct cms swap 
			for the structured leg the curve in the cms pricer is used as estimation and discount curve
			for the float leg the estimation curve is the curve of the floatIndex
			the discount curve for the float leg must be specified separately */
			CmsSwap(boost::shared_ptr<Schedule> fixingSchedule,
				boost::shared_ptr<Schedule> paymentSchedule,
				boost::shared_ptr<Schedule> calculationSchedule,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				boost::shared_ptr<Schedule> floatLegFixingSchedule,
				boost::shared_ptr<Schedule> floatLegPaymentSchedule,
				boost::shared_ptr<Schedule> floatLegCalculationSchedule,
				boost::shared_ptr<IborIndex> floatIndex,
				DayCounter floatLegCouponDayCounter,
				boost::shared_ptr<YieldTermStructure> floatDiscountCurve
				);

			CmsSwap(boost::shared_ptr<Schedule> calculationSchedule,
				int fixingDays,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				boost::shared_ptr<Schedule> floatLegCalculationSchedule,
				int floatFixingDays,
				boost::shared_ptr<IborIndex> iborIndex,
				DayCounter floatLegCouponDayCounter,
				boost::shared_ptr<YieldTermStructure> floatDiscountCurve,
				double strike=0.0000,
				int flavour=1);

			/*! return schedules */
			std::vector<Date> fixingSchedule();
			std::vector<Date> paymentSchedule();
			std::vector<Date> calculationSchedule();

			/*! return cms rates
				adjusted = false => forward swap rates
				adjusted = true  => convexity adjusted forward swap rates */
			std::vector<double> rates(bool adjusted);

			/*! return upper bound of hedge portfolio */
			std::vector<double> upperBounds();
			
			/*! set cap floor payoff 
			    flavour = 1 cap, -1 floor
				strike of cap or floor */
			bool setCapPayoff(double strike, int flavour) {
				strike_=strike;
				flavour_=flavour;
				return true;
			}
			
			/*! set margin this is added _after_ flooring / capping (!) to structured leg */
			bool setMargin(double margin) {
				margin_=margin;
				return true;
			}

			/*! npv of structured leg 
				precomputedSwaplets is the number of already computed swaplet prices
				precomputedPrice is the price of the precomputed swaplets
				swapletUppterBound is the index of the first swaplet that will not be computed (if 0 all swaplets will be computed) */
			Real npv(boost::shared_ptr<CmsPricer> pricer,int precomputedSwaplets=0,double precomputedPrice=0.0,int swapletUpperBound=0);
			
			/*! total npv of swap (pay structured leg, receive float leg with flat margin) */
			Real CmsSwap::totalNpv(boost::shared_ptr<CmsPricer> pricer);
			
			/*! get implied margin for cms swap 
			    if number of swaplets is given, only implied margin of this part is returned (but including precomputed swaplets)
				precomputed floatlets and floatlets upper bound refer to the float leg of the swap
				they must be given, because the frequency of this leg can be different
				if the latter two numbers are zero they are set to the corresponding values of the cms leg */
			Real margin(boost::shared_ptr<CmsPricer> pricer, int precomputedSwaplets=0, double precomputedMargin=0.0, int swapletUpperBound=0, int precomputedFloatlets=0, int floatletUpperBound=0);

		private:
			vector<Date> fixings_,payments_,calc_;
			vector<Date> floatFixings_,floatPayments_,floatCalc_;
			vector<double> rates_,adjustedRates_,upperBound_;
			boost::shared_ptr<SwapIndex> swapIndex_;
			boost::shared_ptr<IborIndex> floatIndex_;
			DayCounter couponDayCounter_,floatLegCouponDayCounter_;
			double strike_,margin_;
			int flavour_;
			boost::shared_ptr<YieldTermStructure> floatDiscountCurve_;
	};

}

#endif


