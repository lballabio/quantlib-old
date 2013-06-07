/*! \file cmsSpreadOption.hpp
    \brief Cms Spread Option
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <CmsPricer.hpp>
#include <CorrelationTermStructure.hpp>

#ifndef quantlib_cmsSpreadOption_hpp
#define quantlib_cmsSpreadOption_hpp

#define CORRACC 0.00001 // accuracy for implied correlation computation
#define CORRSTEP 0.05   // step for brent solver


using namespace boost;
using namespace std;

namespace QuantLib {

	/*! cms spread option */
	
	class CmsSpreadOption {
		public:	
			/*! cms spread option */
			CmsSpreadOption(boost::shared_ptr<Schedule> fixingSchedule,
				boost::shared_ptr<Schedule> paymentSchedule,
				boost::shared_ptr<Schedule> calculationSchedule,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				DayCounter couponDayCounter,
				double strike,
				int flavour);

			CmsSpreadOption(boost::shared_ptr<Schedule> calculationSchedule,
				int fixingDays,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				DayCounter couponDayCounter,
				double strike,
				int flavour);

			Real strike();
			std::vector<Date> fixingSchedule();
			std::vector<Date> paymentSchedule();
			std::vector<Date> calculationSchedule();
			std::vector<double> rates(bool first,bool adjusted);
			std::vector<double> spreads();
			int flavour();
			
			/*! get price for spread option */
			Real npv(boost::shared_ptr<CmsPricer> pricer,Size preCalculatedFixings=0,double preCalculatedPrice=0.0,
				Size fixingUpperBound=0,bool useOtherStrike=false,double otherStrike=0.0, int otherFlavour=0,bool usePreviousRates=true);

			/*! calibrate correlation pillar to given cmsso price */
			Real impliedCorrelation(const boost::shared_ptr<CmsPricer> pricer,const Period& pillar, const double price,Size preCalculatedFixings=0,double preCalculatedPrice=0.0);

		private:
			vector<Date> fixings_,payments_,calc_;
			vector<double> rates1_,rates2_,adjustedRates1_,adjustedRates2_,spreads_;
			boost::shared_ptr<SwapIndex> index1_,index2_;
			DayCounter couponDayCounter_;
			double strike_;
			int flavour_;
			vector<bool> areRatesComputed_;
	};

	class CmsSpreadOptionImplCorrHelper {
		public:
			CmsSpreadOptionImplCorrHelper(CmsSpreadOption* cmsso, boost::shared_ptr<CmsPricer> pricer, const Period& pillar, const double price,
				const Size preCalculatedFixings,const double preCalculatedPrice) :
			  cmsso_(cmsso), pricer_(pricer), pillar_(pillar), price_(price), preCalcFixings_(preCalculatedFixings), preCalcPrice_(preCalculatedPrice) {
			  
				pillar_ = Period(pillar.length(),pillar.units());
			  
			  }
		     
			  double operator()(double r0) const {
				  //if(r<-MAXCORR) return 1.0;//r=-MAXCORR;
				  //if(r>MAXCORR) return 1.0;//r=MAXCORR;
				  double r=atan(r0)*2.0/M_PI;
				  pricer_->correlationTermStructure()->setPillarCorrelation(pillar_,r);
				  double p=cmsso_->npv(pricer_,preCalcFixings_,preCalcPrice_);
				  //FILE *out=fopen("brent.log","a");
				  //fprintf(out,"%f;%f;%f\n",r,p,p-price_);
				  //fclose(out);
				  return p-price_;
			  }

		private:
			CmsSpreadOption* cmsso_;
			boost::shared_ptr<CmsPricer> pricer_;
			Period  pillar_;
			double price_;
			Size preCalcFixings_;
			double preCalcPrice_;
	};

}

#endif


