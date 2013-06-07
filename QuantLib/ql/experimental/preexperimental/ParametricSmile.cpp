#include <parametricSmile.hpp>
#include <stdio.h>

namespace QuantLib {

	ParametricSmile::ParametricSmile(boost::shared_ptr<SwaptionVolatilityCube> volCube,
		double leftBound, double rightBound, vector<double> maturities,
		vector<double> mu, vector<double> nu, double hDiff) :
			volCube_(volCube), leftBound_(leftBound), rightBound_(rightBound), h_(hDiff),
				maturities_(maturities), mu_(mu), nu_(nu)
			{
				muInterpol_=LinearInterpolation(maturities_.begin(),maturities_.end(),mu_.begin());
				nuInterpol_=LinearInterpolation(maturities_.begin(),maturities_.end(),nu_.begin());
				muInterpol_.update();
				nuInterpol_.update();
			}

	bool ParametricSmile::setPillarMu(int pillar, double mu) {
		mu_[pillar]=mu;
		muInterpol_.update();
		return true;
	}

	bool ParametricSmile::setPillarNu(int pillar, double nu) {
		nu_[pillar]=nu;
		nuInterpol_.update();
		return true;
	}

	double ParametricSmile::callPutPrice(const Date& fixing, const Period& tenor, const double& strike0, bool spread) {
		//QL_REQUIRE(strike>=0.0,"Strike (" << strike << ") in ParametricSmile must be non-negative");
		double atm=volCube_->atmStrike(fixing,tenor);
		double strike = strike0;
		if(spread) strike=strike0+atm;
		if(strike<MINSTRIKE) strike=MINSTRIKE;

		Date refDate = volCube_->referenceDate();
		double tFix=volCube_->dayCounter().yearFraction(refDate,fixing);

		double mu=muInterpol_(years(tenor),true);
		double nu=nuInterpol_(years(tenor),true);

		// if strike in inside boundaries, just return the vol from the original swaption cube
		if(strike>=atm+leftBound_ && strike<=atm+rightBound_) {
			double vol=volCube_->volatility(fixing,tenor,strike,true);
			double p=blackFormula( strike>atm ? Option::Call : Option::Put, strike, atm, vol*sqrt(tFix) );
			return p;//vol;
		}
		// strike is left from left boundary
		if(strike<atm+leftBound_) {
			// compute price, first and second derivative at left bound
			double k0=atm+leftBound_;
			double k1=k0+h_;
			double k2=k0+2.0*h_;
			double v0=volCube_->volatility(fixing,tenor,k0,true);
			double v1=volCube_->volatility(fixing,tenor,k1,true);
			double v2=volCube_->volatility(fixing,tenor,k2,true);
			double p0=blackFormula(Option::Put,k0,atm,v0*sqrt(tFix));
			double p1=blackFormula(Option::Put,k1,atm,v1*sqrt(tFix));
			double p2=blackFormula(Option::Put,k2,atm,v2*sqrt(tFix));
			double d1=(log(p1)-log(p0))/h_;
			double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
			// compute paramters for price extrapolation
			double c=0.5*(d2+mu/(k0*k0));
			double b=d1-mu/k0-2.0*c*k0;
			double a=log(p0)-mu*log(k0)-b*k0-c*k0*k0;
			// compute extrapolated price
			double ep=pow(strike,mu)*exp(a+b*strike+c*strike*strike);
			// convert to implied vol
			/*double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Put,strike,atm,ep,1.0,0.0,v0,IMPLACC,100)/sqrt(tFix);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fixing,tenor,strike,true); // fall back
			}*/
			return ep;//implVola;
		}
		else {
			// compute price, first and second derivative at left bound
			double k0=atm+rightBound_-2.0*h_;
			double k1=k0+h_;
			double k2=k0+2.0*h_;
			double v0=volCube_->volatility(fixing,tenor,k0,true);
			double v1=volCube_->volatility(fixing,tenor,k1,true);
			double v2=volCube_->volatility(fixing,tenor,k2,true);
			double p0=blackFormula(Option::Call,k0,atm,v0*sqrt(tFix));
			double p1=blackFormula(Option::Call,k1,atm,v1*sqrt(tFix));
			double p2=blackFormula(Option::Call,k2,atm,v2*sqrt(tFix));
			double d1=(log(p2)-log(p1))/h_;
			double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
			// compute paramters for price extrapolation
			double c=d1*k2*k2*k2+0.5*k2*k2*(k2*k2*d2+nu);
			double b=-k2*k2*d1-nu*k2-2.0*c/k2;
			double a=log(p2)+nu*log(k2)-b/k2-c/(k2*k2);
			//double c=0.0;
			//double b=-nu*k2-d1*k2*k2;
			//double a=log(p0)+nu*log(k2)-b/k2; /*match only 1st derivative*/
			// compute extrapolated price
			double ep=pow(strike,-nu)*exp(a+b/strike+c/(strike*strike));
			// convert to implied vol
			/*double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Call,strike,atm,ep,1.0,0.0,v0,IMPLACC,100)/sqrt(tFix);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fixing,tenor,strike,true);  // fall back
			}*/
			return ep;//implVola;
		}
	}

	double ParametricSmile::volatility(const Period& option, const Period& tenor, const double& strike0, bool spread) {
		Date rd=volCube_->referenceDate();
		Calendar c=volCube_->calendar();
		BusinessDayConvention bdc=volCube_->businessDayConvention();
		Date fixing=c.advance(rd,option);
		return volatility(fixing,tenor,strike0,spread);
	}

	double ParametricSmile::volatility(const Date& fixing, const Period& tenor, const double& strike0, bool spread) {

		//QL_REQUIRE(strike>=0.0,"Strike (" << strike << ") in ParametricSmile must be non-negative");
		double atm=volCube_->atmStrike(fixing,tenor);
		double strike = strike0;
		if(spread) strike=strike0+atm;
		if(strike<MINSTRIKE) strike=MINSTRIKE;

		Date refDate = volCube_->referenceDate();
		double tFix=volCube_->dayCounter().yearFraction(refDate,fixing);

		double mu=muInterpol_(years(tenor),true);
		double nu=nuInterpol_(years(tenor),true);

		// if strike in inside boundaries, just return the vol from the original swaption cube
		if(strike>=atm+leftBound_ && strike<=atm+rightBound_) {
			double vol=volCube_->volatility(fixing,tenor,strike,true);
			//double p=blackFormula( strike>atm ? Option::Call : Option::Put, strike, atm, vol*sqrt(tFix) );
			return vol;
		}
		// strike is left from left boundary
		if(strike<atm+leftBound_) {
			// compute price, first and second derivative at left bound
			double k0=atm+leftBound_;
			double k1=k0+h_;
			double k2=k0+2.0*h_;
			double v0=volCube_->volatility(fixing,tenor,k0,true);
			double v1=volCube_->volatility(fixing,tenor,k1,true);
			double v2=volCube_->volatility(fixing,tenor,k2,true);
			double p0=blackFormula(Option::Put,k0,atm,v0*sqrt(tFix));
			double p1=blackFormula(Option::Put,k1,atm,v1*sqrt(tFix));
			double p2=blackFormula(Option::Put,k2,atm,v2*sqrt(tFix));
			double d1=(log(p1)-log(p0))/h_;
			double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
			// compute paramters for price extrapolation
			double c=0.5*(d2+mu/(k0*k0));
			double b=d1-mu/k0-2.0*c*k0;
			double a=log(p0)-mu*log(k0)-b*k0-c*k0*k0;
			// TEST check if everything is al right
			/*double km1=k0-h_;
			double km2=k0-2.0*h_;
			double pm0=pow(k0,mu)*exp(a+b*k0+c*k0*k0);
			double pm1=pow(km1,mu)*exp(a+b*km1+c*km1*km1);
			double pm2=pow(km2,mu)*exp(a+b*km2+c*km2*km2);
			double dm1=(log(pm0)-log(pm1))/h_;
			double dm2=(log(pm0)-2.0*log(pm1)+log(pm2))/(h_*h_);
			FILE *out=fopen("parametric.log","a");
			fprintf(out,"LEFT: p0=%f, d1=%f, d2=%f, pm0=%f, dm1=%f, dm2=%f\n",p0,d1,d2,pm0,dm1,dm2);
			fclose(out);*/
			// END TEST
			// compute extrapolated price
			double ep=pow(strike,mu)*exp(a+b*strike+c*strike*strike);
			// convert to implied vol
			double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Put,strike,atm,ep,1.0,0.0,v0,IMPLACC,100)/sqrt(tFix);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fixing,tenor,strike,true); // fall back
			}
			return implVola;
		}
		else {
			// compute price, first and second derivative at left bound
			double k0=atm+rightBound_-2.0*h_;
			double k1=k0+h_;
			double k2=k0+2.0*h_;
			double v0=volCube_->volatility(fixing,tenor,k0,true);
			double v1=volCube_->volatility(fixing,tenor,k1,true);
			double v2=volCube_->volatility(fixing,tenor,k2,true);
			double p0=blackFormula(Option::Call,k0,atm,v0*sqrt(tFix));
			double p1=blackFormula(Option::Call,k1,atm,v1*sqrt(tFix));
			double p2=blackFormula(Option::Call,k2,atm,v2*sqrt(tFix));
			double d1=(log(p2)-log(p1))/h_;
			double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
			// compute paramters for price extrapolation
			double c=d1*k2*k2*k2+0.5*k2*k2*(k2*k2*d2+nu);
			double b=-k2*k2*d1-nu*k2-2.0*c/k2;
			double a=log(p2)+nu*log(k2)-b/k2-c/(k2*k2);
			//double c=0.0;
			//double b=-nu*k2-d1*k2*k2;
			//double a=log(p0)+nu*log(k2)-b/k2; /*match only 1st derivative*/
			// TEST check if everything is al right
			/*double kp1=k2+h_;
			double kp2=k2+2.0*h_;
			double pp0=pow(k2,-nu)*exp(a+b/k2+c/(k2*k2));
			double pp1=pow(kp1,-nu)*exp(a+b/kp1+c/(kp1*kp1));
			double pp2=pow(kp2,-nu)*exp(a+b/kp2+c/(kp2*kp2));
			double dp1=(log(pp1)-log(pp0))/h_;
			double dp2=(log(pp2)-2.0*log(pp1)+log(pp0))/(h_*h_);
			FILE *out=fopen("parametric.log","a");
			fprintf(out,"RIGHT: p2=%f, d1=%f, d2=%f, pp0=%f, dp1=%f, dp2=%f\n",p2,d1,d2,pp0,dp1,dp2);
			fclose(out);*/
			// END TEST

			// compute extrapolated price
			double ep=pow(strike,-nu)*exp(a+b/strike+c/(strike*strike));
			// convert to implied vol
			double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Call,strike,atm,ep,1.0,0.0,v0,IMPLACC,100)/sqrt(tFix);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fixing,tenor,strike,true);  // fall back
			}
			return implVola;
		}
	}

	bool ParametricSmile::setFastVol(const Date& fixing, const Period& tenor) {
		fastFixing_=fixing;
		fastTenor_=tenor;
		fastAtm_=volCube_->atmStrike(fixing,tenor);
		Date refDate = volCube_->referenceDate();
		fastTfix_=volCube_->dayCounter().yearFraction(refDate,fixing);
		fastMu_=muInterpol_(years(tenor),true);
		fastNu_=nuInterpol_(years(tenor),true);
		// compute price, first and second derivative at left bound
		if(fastAtm_+leftBound_>=0.0) { // if this is violated, then never left extrpolation will be used
			double k0=fastAtm_+leftBound_;
			double k1=k0+h_;
			double k2=k0+2.0*h_;
			double v0=volCube_->volatility(fixing,tenor,k0,true); fastLv0_=v0;
			double v1=volCube_->volatility(fixing,tenor,k1,true);
			double v2=volCube_->volatility(fixing,tenor,k2,true);
			double p0=blackFormula(Option::Put,k0,fastAtm_,v0*sqrt(fastTfix_));
			double p1=blackFormula(Option::Put,k1,fastAtm_,v1*sqrt(fastTfix_));
			double p2=blackFormula(Option::Put,k2,fastAtm_,v2*sqrt(fastTfix_));
			double d1=(log(p1)-log(p0))/h_;
			double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
			// compute paramters for price extrapolation
			fastLC_=0.5*(d2+fastMu_/(k0*k0));
			fastLB_=d1-fastMu_/k0-2.0*fastLC_*k0;
			fastLA_=log(p0)-fastMu_*log(k0)-fastLB_*k0-fastLC_*k0*k0;
		}
		// compute price, first and second derivative at right bound
		double k0=fastAtm_+rightBound_-2.0*h_;
		double k1=k0+h_;
		double k2=k0+2.0*h_;
		double v0=volCube_->volatility(fixing,tenor,k0,true); fastRv0_=v0;
		double v1=volCube_->volatility(fixing,tenor,k1,true);
		double v2=volCube_->volatility(fixing,tenor,k2,true);
		double p0=blackFormula(Option::Call,k0,fastAtm_,v0*sqrt(fastTfix_));
		double p1=blackFormula(Option::Call,k1,fastAtm_,v1*sqrt(fastTfix_));
		double p2=blackFormula(Option::Call,k2,fastAtm_,v2*sqrt(fastTfix_));
		double d1=(log(p1)-log(p0))/h_;
		double d2=(log(p2)-2.0*log(p1)+log(p0))/(h_*h_);
		// compute paramters for price extrapolation
		fastRC_=d1*k2*k2*k2+0.5*k2*k2*(d2*k2*k2+fastNu_);
		fastRB_=-d1*k2*k2-fastNu_*k2-2.0*fastRC_/k2;
		fastRA_=log(p2)+fastNu_*log(k2)-fastRB_/k2-fastRC_/(k2*k2);
		//double c=0.0;
		//double b=-nu*k2-d1*k2*k2;
		//double a=log(p0)+nu*log(k2)-b/k2; /*match only 1st derivative*/
		return true;

	}


	double ParametricSmile::getFastVol(const double& strike0) {

		//QL_REQUIRE(strike>=0.0,"Strike (" << strike << ") in ParametricSmile must be non-negative");
		double strike=strike0;
		if(strike<MINSTRIKE) strike=MINSTRIKE;
		//return this->volatility(fastFixing_,fastTenor_,strike);

		// if strike in inside boundaries, just return the vol from the original swaption cube
		if(strike>=fastAtm_+leftBound_ && strike<=fastAtm_+rightBound_) {
			double vol=volCube_->volatility(fastFixing_,fastTenor_,strike,true);
			//double p=blackFormula( strike>fastAtm_ ? Option::Call : Option::Put, strike, fastAtm_, vol*sqrt(fastTfix_) );
			return vol;
		}
		// strike is left from left boundary
		if(strike<fastAtm_+leftBound_) {
			// compute extrapolated price
			double ep=pow(strike,fastMu_)*exp(fastLA_+fastLB_*strike+fastLC_*strike*strike);
			// convert to implied vol
			double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Put,strike,fastAtm_,ep,1.0,0.0,fastLv0_,IMPLACC,100)/sqrt(fastTfix_);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fastFixing_,fastTenor_,strike,true);  // fall back
			}
			return implVola;
		}
		else {
			// compute extrapolated price
			double ep=pow(strike,-fastNu_)*exp(fastRA_+fastRB_/strike+fastRC_/(strike*strike));
			// convert to implied vol
			double implVola;
			try {
				implVola=blackFormulaImpliedStdDev(Option::Call,strike,fastAtm_,ep,1.0,0.0,fastRv0_,IMPLACC,100)/sqrt(fastTfix_);
			} catch(QuantLib::Error er) {
				implVola = volCube_->volatility(fastFixing_,fastTenor_,strike,true);  // fall back
			}
			return implVola;
		}

	}

	bool ParametricSmile::writeMurexFile(string path,vector<Period>& optionTenors,vector<Period>& swapTenors,
		vector<double> strikeSpreads) {

			ofstream out;
			out.open(path.c_str());

			for(int i=0;i<swapTenors.size();i++) {
				for(int j=0;j<optionTenors.size();j++) {
					for(int k=0;k<strikeSpreads.size();k++) {
						double vol=this->volatility(optionTenors[j],swapTenors[i],strikeSpreads[k],true);
						double atmVol=this->volatility(optionTenors[j],swapTenors[i],0.0,true);
						double volSpread=vol-atmVol;
						out << "|" << swapTenors[i] << "|EUR|Swaption|" << optionTenors[j] << "|" << strikeSpreads[k] << "|SMRS|"
							<< volSpread << "|" << volSpread << "|" << endl;
					}
				}
			}

			out.close();

			return true;

	}


	
}
