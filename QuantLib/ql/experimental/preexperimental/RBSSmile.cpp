#include <RbsSmile.hpp>
#include <stdio.h>

namespace QuantLib {

	RbsSmile::RbsSmile(double leftBound, double rightBound,
				double leftPrice, double rightPrice,
				double left1d, double left2d, double right1d, double right2d,
				double mu, double nu, double minStrike, double blackAccuracy) : leftBound_(leftBound), rightBound_(rightBound),
							leftPrice_(leftPrice), rightPrice_(rightPrice),
							left1d_(left1d), left2d_(left2d), right1d_(right1d), right2d_(right2d),
							mu_(mu), nu_(nu), minStrike_(minStrike), blackAccuracy_(blackAccuracy)
			{
				update();
			}

	RbsSmile::RbsSmile(double leftBound, double rightBound,
				double mu, double nu, double la, double lb, double lc, double ra, double rb, double rc,bool dummy,
				double minStrike,double blackAccuracy): leftBound_(leftBound), rightBound_(rightBound),
							mu_(mu), nu_(nu), la_(la), lb_(lb), lc_(lc), ra_(ra), rb_(rb), rc_(rc),
							minStrike_(minStrike), blackAccuracy_(blackAccuracy)
			{
			}

	void RbsSmile::recalibrate(double mu, double nu) {
		mu_=mu;
		nu_=nu;
		update();
	}

	void RbsSmile::recalibrate(double leftPrice, double rightPrice, 
		double left1d, double left2d, double right1d, double right2d) {
		leftPrice_=leftPrice;
		rightPrice_=rightPrice;
		left1d_=left1d;
		left2d_=left2d;
		right1d_=right1d;
		right2d_=right2d;
		update();
	}

	void RbsSmile::recalibrate(double mu, double nu, double leftPrice, double rightPrice,
		double left1d, double left2d, double right1d, double right2d) {
		mu_=mu;
		nu_=nu;
		leftPrice_=leftPrice;
		rightPrice_=rightPrice;
		left1d_=left1d;
		left2d_=left2d;
		right1d_=right1d;
		right2d_=right2d;
		update();
	}

	double RbsSmile::price(double strike) {

		QL_REQUIRE(strike>=rightBound_ || strike<=leftBound_,"Strike (" << strike << ") must be outside core region.");

		if(strike<minStrike_) strike=minStrike_; // if strike falls inside core region, 0.0 is returned as price

		double p=0.0;

		if(strike<=leftBound_) {
			p=pow(strike,mu_)*exp(la_+lb_*strike+lc_*strike*strike);
		}
		if(strike>=rightBound_) {
			p=pow(strike,-nu_)*exp(ra_+rb_/strike+rc_/(strike*strike));
		}
		return p;
	}

	double RbsSmile::impliedVola(double forward, double strike, double maturity, double annuity) {
			
			double p = price(strike);
			double vol;
			Option::Type type = strike > forward ? Option::Call : Option::Put;
			try {
				// HARDCODED vol guess!
				vol=blackFormulaImpliedStdDev(type,strike,forward,p,annuity,0.0,0.2,blackAccuracy_,100)/sqrt(maturity);
			} catch(QuantLib::Error er) {
				vol = 0.0; 
			}
			return vol;
	}

	void RbsSmile::update() {
		
		double k0=leftBound_;
		if(k0>=minStrike_) {
			double p0=leftPrice_;
			double l1 = left1d_ / p0;
			double l2 = - left1d_ / (p0*p0) * left1d_ + 1.0 / p0 * left2d_;
			logLeft1d_=l1; logLeft2d_=l2;
			// compute paramters for price extrapolation at left bound
			lc_=0.5*(l2+mu_/(k0*k0));
			lb_=l1-mu_/k0-2.0*lc_*k0;
			la_=log(p0)-mu_*log(k0)-lb_*k0-lc_*k0*k0;
		}

		double k1=rightBound_;
		double p1=rightPrice_;
		double r1 = right1d_ / p1;
		double r2 = - right1d_ / (p1*p1) * right1d_ + 1.0 / p1 * right2d_;
		logRight1d_=r1; logRight2d_=r2;
		// compute parameters for price extrapolation at right bound
		rc_=r1*k1*k1*k1+0.5*k1*k1*(k1*k1*r2+nu_);
		rb_=-k1*k1*r1-nu_*k1-2.0*rc_/k1;
		ra_=log(p1)+nu_*log(k1)-rb_/k1-rc_/(k1*k1);

	}

	vector<double> RbsSmile::modelParameters() {
		vector<double> res;
		res.push_back(mu_);
		res.push_back(nu_);
		res.push_back(la_);
		res.push_back(lb_);
		res.push_back(lc_);
		res.push_back(ra_);
		res.push_back(rb_);
		res.push_back(rc_);
		return res;
	}
	

	
}
