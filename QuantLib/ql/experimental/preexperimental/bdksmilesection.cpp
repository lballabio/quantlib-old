#include <bdksmilesection.hpp>

#include <iostream>

namespace QuantLib {

    BdkSmileSection::BdkSmileSection(const boost::shared_ptr<SmileSection> source, const Real mu, const Real nu, const Real minLeftCutoffStrike, const Real maxRightCutoffStrike, const std::vector<Real>& moneynessGrid, const Real atm)
		: source_(source), mu_(mu), nu_(nu), SmileSection(*source) {

		QL_REQUIRE(minLeftCutoffStrike >= 0.0, "MinLeftCutoffStrike (" << minLeftCutoffStrike << ") must be non negative.");
		QL_REQUIRE(maxRightCutoffStrike >= minLeftCutoffStrike, "MaxRightCutoffStrike (" << maxRightCutoffStrike << ") must be greater of equal to MinLeftCutoffStrike (" << minLeftCutoffStrike << ")");
	
		if(atm==Null<Real>()) {
			f_ = source_->atmLevel();
		}
		else {
			f_ = atm;
		}

		const std::vector<Real> moneynessGrid1 = makeMoneynessGrid(moneynessGrid);

		leftCutoff_ = 0.0;
		rightCutoff_ = QL_MAX_REAL;

		QL_REQUIRE(minLeftCutoffStrike < moneynessGrid1.back() * f_,"MinLeftCutoffStrike (" << minLeftCutoffStrike << ") must be less than rightmost strike in moneyness grid (" << moneynessGrid1.back() * f_ << ")");
		QL_REQUIRE(maxRightCutoffStrike > moneynessGrid1.front() * f_,"MaxRightCutoffStrike (" << maxRightCutoffStrike << ") must be greater than leftmost strike in moneyness grid (" << moneynessGrid1.front() * f_ << ")");

		bool af;

		//Size leftIndex=0;
		//while(moneynessGrid1[leftIndex] * f_ < minLeftCutoffStrike) leftIndex++;
		//do {
		//	af=true;
		//	leftCutoff_ = moneynessGrid1[leftIndex] * f_;
		//	Real left1d = source->digitalOptionPrice(leftCutoff_,Option::Put);
		//	Real left2d = source->density(leftCutoff_,Option::Put);
		//	Real p0 = source->optionPrice(leftCutoff_,Option::Put);
		//	Real l1 = left1d / p0;
		//	Real l2 = - left1d / (p0*p0) * left1d + 1.0 / p0 * left2d;
		//	// compute paramters for price extrapolation at left bound
		//	lc_=0.5*(l2+mu_/(leftCutoff_*leftCutoff_));
		//	lb_=l1-mu_/leftCutoff_-2.0*lc_*leftCutoff_;
		//	la_=log(p0)-mu_*log(leftCutoff_)-lb_*leftCutoff_-lc_*leftCutoff_*leftCutoff_;
		//	// test if admissable
		//	std::pair<Real,Real> afr = arbitragefreeRegion(moneynessGrid1);
		//	if( afr.first > QL_EPSILON || afr.second < leftCutoff_) {
		//		af = false;
		//		leftIndex++;
		//		while(moneynessGrid1[leftIndex] * f_ < afr.first) leftIndex++;
		//	}
		//} while(!af && leftIndex < moneynessGrid1.size());

		// let's try a quartic polynomial
		Size leftIndex=0;
		Real laf = arbitragefreeRegion(moneynessGrid1).first;
		while(moneynessGrid1[leftIndex] * f_ < std::max(minLeftCutoffStrike,laf)) leftIndex++;
		do {
			af=true;
			leftCutoff_ = moneynessGrid1[leftIndex] * f_;
			Real left1d = source->digitalOptionPrice(leftCutoff_,Option::Put);
			Real left2d = source->density(leftCutoff_,Option::Put);
			Real p0 = source->optionPrice(leftCutoff_,Option::Put);
			if(left1d*leftCutoff_ < p0) {
				af=false;
				leftIndex++;
			}
			else {
				la_ = 6.0/(leftCutoff_*leftCutoff_)*p0-3.0/leftCutoff_*left1d+0.5*left2d;
				lb_ = -8.0/(leftCutoff_*leftCutoff_*leftCutoff_)*p0+5.0/(leftCutoff_*leftCutoff_)*left1d-1.0/leftCutoff_*left2d;
				lc_ = 3.0/(leftCutoff_*leftCutoff_*leftCutoff_*leftCutoff_)*p0-2.0/(leftCutoff_*leftCutoff_*leftCutoff_)*left1d+1.0/(2.0*leftCutoff_*leftCutoff_)*left2d;
				std::pair<Real,Real> afr = arbitragefreeRegion(moneynessGrid1);
				if( afr.first > QL_EPSILON || afr.second < leftCutoff_) {
					af = false;
					leftIndex++;
					while(moneynessGrid1[leftIndex] * f_ < afr.first) leftIndex++;
				}
			}
		} while(!af && leftIndex < moneynessGrid1.size());

		QL_REQUIRE(af,"No arbitrage free extrapolation to left is possible.");

		std::cout << "BDK Smile: Left cutoff is " << moneynessGrid1[leftIndex] * f_ << " af=[" << arbitragefreeRegion(moneynessGrid1).first << "," << arbitragefreeRegion(moneynessGrid1).second << "]" << std::endl;

		Size rightIndex = moneynessGrid1.size()-1;
		while(moneynessGrid1[rightIndex] * f_ > maxRightCutoffStrike && rightIndex >= leftIndex) rightIndex--;
		do {
			af=true;
			rightCutoff_ = moneynessGrid1[rightIndex] * f_;
			Real right1d_ = -source->digitalOptionPrice(rightCutoff_);
			Real right2d_ = source->density(rightCutoff_);
			Real p1 = source->optionPrice(rightCutoff_);
			Real r1 = right1d_ / p1;
			Real r2 = - right1d_ / (p1*p1) * right1d_ + 1.0 / p1 * right2d_;
			// compute parameters for price extrapolation at right bound
			rc_=r1*rightCutoff_*rightCutoff_*rightCutoff_+0.5*rightCutoff_*rightCutoff_*(rightCutoff_*rightCutoff_*r2+nu_);
			rb_=-rightCutoff_*rightCutoff_*r1-nu_*rightCutoff_-2.0*rc_/rightCutoff_;
			ra_=log(p1)+nu_*log(rightCutoff_)-rb_/rightCutoff_-rc_/(rightCutoff_*rightCutoff_);
			// test if admissable
			Real raf = arbitragefreeRegion(moneynessGrid1).second;
			if( raf < moneynessGrid1.back() * f_ - QL_EPSILON ) {
				af = false;
				rightIndex --;
				while(moneynessGrid1[rightIndex] * f_ > raf) rightIndex--;
			}
		} while(!af && rightIndex >= leftIndex);

		std::cout << "BDK Smile: Right cutoff is " << moneynessGrid1[rightIndex] * f_ << " af=[" << arbitragefreeRegion(moneynessGrid1).first << "," << arbitragefreeRegion(moneynessGrid1).second << "]" << std::endl;
		QL_REQUIRE(af,"No arbitrage free extrapolation to right is possible.");

    }

    Real BdkSmileSection::volatilityImpl(Rate strike) const {
		Real vol=0.0;
		try {
			vol = blackFormulaImpliedStdDev(Option::Call,strike,f_,optionPrice(strike)) / sqrt(exerciseTime());
		} catch(QuantLib::Error) { }
		return vol;
    }

	Real BdkSmileSection::optionPrice(Rate strike, Option::Type type, Real discount) const {
		if(strike < leftCutoff_) {
			//return discount * ( pow(strike,mu_)*exp(la_+lb_*strike+lc_*strike*strike) + (type == Option::Put ? 0.0 : f_ - strike) );
			return discount * ( la_*strike*strike+lb_*strike*strike*strike+lc_*strike*strike*strike*strike + (type == Option::Put ? 0.0 : f_ - strike) ); // quartic polynomial
		}
		if(strike > rightCutoff_) {
			return discount * ( pow(strike,-nu_)*exp(ra_+rb_/strike+rc_/(strike*strike)) + (type == Option::Call ? 0.0 : strike - f_) );
		}
		return source_->optionPrice(strike,type,discount);
	}




}
