#include <singleLookSpreadOption.hpp>
#include <stdio.h>

namespace QuantLib {

	SingleLookSpreadOption::SingleLookSpreadOption(Date fixing, Date payment,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				double strike,
				int flavour) : 	fixing_(fixing),payment_(payment),index1_(index1), index2_(index2),
								strike_(strike),flavour_(flavour)
	{
		areRatesComputed_=false;
	}

	Real SingleLookSpreadOption::npv(boost::shared_ptr<CmsPricer> pricer,
		bool useDiffStrike, double diffStrike, int diffFlavour, bool usePreviousRates) {

		if(!useDiffStrike_) {
			diffStrike=strike_;
			diffFlavour=flavour_;
		}
		double price=pricer->cmssoPrice(fixing_,payment_,index1_,index2_,
				diffStrike,diffFlavour,usePreviousRates & areRatesComputed_,
				rate1_,rate2_,adjustedRate1_,adjustedRate2_);
		
		if(!(usePreviousRates & areRatesComputed_)) {
				rate1_=pricer->lastRate(true,false);
				rate2_=pricer->lastRate(false,false);			
				adjustedRate1_=pricer->lastRate(true,true);	
				adjustedRate2_=pricer->lastRate(false,true);	
				areRatesComputed_=true;
				spread_=pricer->lastSpread();
		}
		//FILE* out=fopen("CMSSO.log","a");
		//fprintf(out,"---\n");
		//fclose(out);
		return price;
	}

	double SingleLookSpreadOption::rate(bool first,bool adjusted) {
		if(adjusted) {
			if(first) return adjustedRate1_; else return adjustedRate2_;
		}
		else {
			if(first) return rate1_; else return rate2_;
		}
	}

	double SingleLookSpreadOption::spread() {
		return spread_;
	}

}