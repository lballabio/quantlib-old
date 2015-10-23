#include <MurexCurve.hpp>
#include <stdio.h>

namespace QuantLib {

	MurexCurve::MurexCurve(Date referenceDate,
				vector<Date> maturities,
				vector<double> discounts,
				long interpolationMode
				) :	refDate_(referenceDate),maturities_(maturities),discounts_(discounts),interpolationMode_(interpolationMode)
	{
		QL_REQUIRE(maturities.size()==discounts.size(),"Maturities size (" << maturities.size() << ") must match discounts size (" << discounts.size() << ")");
		QL_REQUIRE(interpolationMode_==0 || interpolationMode_==1,"Interpolation Mode (" << interpolationMode_ << ") must be 0 or 1.");
		dc_=Actual365Fixed();
		n_=maturities.size()+1;
		times_=vector<double>(n_);
		rates_=vector<double>(n_);
		calculate();
	}

	bool MurexCurve::setDayCounter(DayCounter dc) {
		dc_=dc;	
		calculate();
		return true;
	}

	DayCounter MurexCurve::dayCounter() {
		return dc_;
	}

	double MurexCurve::rate(Date maturity) {
		return rate(dc_.yearFraction(refDate_,maturity));
	}

	double MurexCurve::discount(Date maturity) {
		return discount(dc_.yearFraction(refDate_,maturity));
	}
	
	double MurexCurve::rate(double t) {
		return interpol_(t,true) / (interpolationMode_==2 ? t : 1.0);
	}


	double MurexCurve::discount(double t) {
		return exp(-rate(t)*t);
	}


	void MurexCurve::calculate() {
		
		times_[0]=0.0;
		for(int i=0;i<n_-1;i++) {
			times_[i+1]=dc_.yearFraction(refDate_,maturities_[i]);
			rates_[i+1]=-log(discounts_[i])/(interpolationMode_==2 ? 1.0 : times_[i+1]);
		}
		rates_[0]=rates_[1];

		interpol_=LinearInterpolation(times_.begin(),times_.end(),rates_.begin());
		interpol_.update();

	}

}