#include <correlationTermStructure.hpp>


namespace QuantLib {

	CorrelationTermStructure::CorrelationTermStructure(const Date& referenceDate,
							   const std::vector<Period>& maturities,
							   const std::vector<double>& correlations,
							   const Calendar& calendar,
							   const BusinessDayConvention& bdc,
							   bool endOfMonth,
							   bool leftFlat,
							   bool rightFlat,
                               const DayCounter& dayCounter) :
	
	referenceDate_(referenceDate), calendar_(calendar), dayCounter_(dayCounter), maturities_(maturities), correlations_(correlations),bdc_(bdc), endOfMonth_(endOfMonth),
		leftFlat_(leftFlat), rightFlat_(rightFlat)
	
	{	
		QL_REQUIRE(maturities.size()==correlations.size(),"Number of maturities (" << maturities.size() << ") is not eqal to correlations (" << correlations.size() << ")");
		
		for(int i=0;i<maturities.size();i++) {
			Date d = calendar.advance(referenceDate,maturities[i].length(),maturities[i].units(),bdc_,endOfMonth_);
			dates_.push_back(d);
			times_.push_back(this->timeFromReference(d));
		}
		minTime_=times_[0];
		maxTime_=times_[maturities_.size()-1];

		interpolation_ = LinearInterpolation(times_.begin(),times_.end(),correlations_.begin());
		interpolation_.update();
	}

	Time CorrelationTermStructure::timeFromReference(const Date& date) const {
        return dayCounter_.yearFraction(referenceDate_, date);
	}

	double CorrelationTermStructure::correlation(Date d, bool allowExtrapolation) {
		double t=this->timeFromReference(d);
		if(leftFlat_ && t<minTime_) t=minTime_;
		if(rightFlat_ && t>maxTime_) t=maxTime_;
		return interpolation_(t,allowExtrapolation);
	}

	double CorrelationTermStructure::correlation(Period p, bool allowExtrapolation) {
		Date d=calendar_.advance(referenceDate_,p.length(),p.units(),bdc_,endOfMonth_);
		return correlation(d,allowExtrapolation);
	}


	bool CorrelationTermStructure::setPillarCorrelation(const Period& p, double correlation) {

		int ind=0;
		bool found=false;
		do {
			if(maturities_[ind]==p) {
				found=true;
			}
			else {
				ind=ind+1;
			}
		} while(!found && ind < maturities_.size());

		if(!found) QL_FAIL("Maturity " << p << " not found in correlation pillar set.");

		correlations_[ind] = correlation;
		interpolation_.update();

		return true;

	}

	bool CorrelationTermStructure::setPillarCorrelation(int i,double correlation) {
		QL_REQUIRE(i>=0 && i<maturities_.size(),"Can not set pillar # " << i << " correlation with " << maturities_.size() << " pillars.");
		correlations_[i]=correlation;
		interpolation_.update();
	}
	
	std::vector<double> CorrelationTermStructure::pillarCorrelations() {
		return correlations_;
	}

	double CorrelationTermStructure::pillarCorrelation(int i) {
		return correlations_[i];
	}

	int CorrelationTermStructure::numberOfPillars() {
		return maturities_.size();
	}

}