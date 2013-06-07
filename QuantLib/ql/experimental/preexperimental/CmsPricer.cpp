#include <cmsPricer.hpp>
#include <stdio.h>

namespace QuantLib {

	/*CmsPricer::CmsPricer(boost::shared_ptr<YieldTermStructure> yts,boost::shared_ptr<SabrRbsSmile> volCube,
		boost::shared_ptr<CorrelationTermStructure> corr, Date settlementDate) :
		yts_(yts), volCube_(volCube), corr_(corr), settlementDate_(settlementDate)
		{
			useStdCube_=false;
			vdc=volCube_->volCube()->dayCounter();
			dc_=yts_->dayCounter();
			initDefaultValues();
		}
		*/

	CmsPricer::CmsPricer(boost::shared_ptr<YieldTermStructure> yts,boost::shared_ptr<SwaptionVolatilityStructure> volCube2,
		boost::shared_ptr<CorrelationTermStructure> corr, Date settlementDate) :
		yts_(yts), volCube2_(volCube2), corr_(corr), settlementDate_(settlementDate)
		{
			useStdCube_=true;
			vdc=volCube2_->dayCounter();
			dc_=yts_->dayCounter();
			initDefaultValues();
		}


	void CmsPricer::initDefaultValues() {
			refDate=yts_->referenceDate();
			// default values
			convAdjMode_=2; // replication adjustment
			cmssoMode_=0;   // atm volas
			// HAGAN Integral
			haganLowerBound_=0.0001; // lower integration bound hagan integral
			haganUpperBound_=0.50;   // upper integration bound hagan integral
			haganMinPrice_=0.0;		 // minimum price hagan integral
			HAGANINTEGRALMAXSTEPS=1000; // max steps integral
			HAGANINTEGRALACC=1.0E-6;     // accuracy for hagan integral
			// cms replication parameters
			meanReversion_=0.01;		 // mean reversion for cms repcliation hull white scenario
			lowerStrike_=0.0;
			upperStrike_=0.4;
			hn_=100;
			vegaRatio_=0.01;
			HMATCHACC=1.0E-9;
			MAXSTRIKE=2.0;
			maxStdDevVol_=0.0;
			// spread option
			CMSSOHERMITEPOINTS=32;// points hermite integration for cmsso calculation
			//
			initHermiteIntegrator();
			//
	}

	void CmsPricer::initHermiteIntegrator() {
		ghInt_ = new GaussHermiteIntegration(CMSSOHERMITEPOINTS);
	}

	bool CmsPricer::setCalculationModes(int convAdjMode,  int cmssoMode, long hermitePoints) {
		convAdjMode_=convAdjMode;
		cmssoMode_=cmssoMode;
		CMSSOHERMITEPOINTS=hermitePoints;
		initHermiteIntegrator();
		return true;
	}

	bool CmsPricer::setHaganIntegralParameters(double haganLowerBound, double haganUpperBound, double haganMinPrice,double integralAcc, long maxSteps) {
		haganLowerBound_=haganLowerBound;
		haganUpperBound_=haganUpperBound;
		HAGANINTEGRALACC=integralAcc;
		HAGANINTEGRALMAXSTEPS=maxSteps;
		return true;
	}

	bool CmsPricer::setCmsReplicationParameters(double meanReversion,double ls, double us, double vegaRatio,int hn,double matchAcc, double maxStrike, double maxStdDevVol) {
		meanReversion_=meanReversion;
		lowerStrike_=ls;
		upperStrike_=us;
		hn_=hn;
		vegaRatio_=vegaRatio;
		HMATCHACC=matchAcc;
		MAXSTRIKE=maxStrike;
		maxStdDevVol_=maxStdDevVol;
		return true;
	}

	Real CmsPricer::swapRateAdjustment(const Date& fixingDate, 
				const Date& paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex,
				double strike,
				int flavour) {

		QL_REQUIRE(strike==0.0 || convAdjMode_==2,"For methods different from replication (" << convAdjMode_ << ") strike must be 0.0 (" << strike << ")");
		
		double tFix=vdc.yearFraction(refDate,fixingDate,refDate,fixingDate);
		boost::shared_ptr<VanillaSwap> fwdSwap = swapIndex->underlyingSwap(fixingDate);
		Schedule fixedSchedule = fwdSwap->fixedSchedule();
		DayCounter fdc = fwdSwap->fixedDayCount();
		Period tenor = swapIndex->tenor();
		vector<Date> dates = fixedSchedule.dates();
		double atm = swapRateHullWhite(fixingDate,dates,fdc,0.0);//swapIndex->fixing(fixingDate);//volCube_->atmStrike(fixingDate,tenor);
		
		lastUpperBound_=0.0;

		if(tFix==0.0) { // fixing on reference date
			if(strike==0.0) return 0.0;
			else {
				return max(((double)flavour)*(atm-strike),0.0)-atm;
			}
		}

		Real dffixing = discount(fixingDate);

		boost::shared_ptr<SmileSection> smileSec;
		//if(!useStdCube_) volCube_->setFastVolatility(fixingDate,tenor); // enable fast volatility computation in SabrRbsCube
		//else 
		smileSec = volCube2_->smileSection(fixingDate,tenor);
		
		double vol;
		//if(!useStdCube_) vol=volCube_->getFastVolatility(atm);
		//else
		vol= smileSec->volatility(atm);
		
		double maxVolStrike=MAXSTRIKE, minVolStrike=0.0001;
		if(maxStdDevVol_>0.0) {
			maxVolStrike=atm*exp(maxStdDevVol_*vol*sqrt(tFix));
			minVolStrike=atm*exp(-maxStdDevVol_*vol*sqrt(tFix));
		}
		

		if(convAdjMode_==0) { // classic adjustment
			
			double nom=0.0,denom=0.0;
			for(int i=1;i<dates.size();i++) {
				double t=fdc.yearFraction(dates[i-1],dates[i],dates[i-1],dates[i]);
				double t0=fdc.yearFraction(dates[0],dates[i],dates[0],dates[i]);
				nom+=(-t0)*(-t0-1.0)*t/pow(1.0+atm,t0+2.0);
				denom+=(-t0)*t/pow(1.0+atm,t0+1.0);
			}
			return -0.5*atm*atm*(exp(vol*vol*tFix)-1.0)*nom/denom;
			//return -0.5*atm*atm*vol*vol*tFix*nom/denom;

		}

		if(convAdjMode_==1) { // hagan adjustment

			QL_REQUIRE(!useStdCube_,"Hagan adjustment only implemented for non standard cube...");

			/*Brent b;

			// determine lowerStrike and upperStrike from vegaRatio, if given
			if(vegaRatio_>0.0) {
				//double z1=-vol*vol*tFix/2.0+sqrt(vol*vol*vol*vol*tFix*tFix/4.0-2.0*vol*vol*tFix*log(vegaRatio_));
				//double z2=-vol*vol*tFix/2.0-sqrt(vol*vol*vol*vol*tFix*tFix/4.0-2.0*vol*vol*tFix*log(vegaRatio_));
				//lowerStrike_=atm*exp(-z1);
				//upperStrike_=atm*exp(-z2);
				//if(lowerStrike_<0.0001) lowerStrike_=0.0001;
				/if(upperStrike_>MAXSTRIKE) upperStrike_=MAXSTRIKE;
				VegaRatioHelper vrh1(&(*volCube_),fixingDate,tenor,atm,1,vegaRatio_,MAXSTRIKE);
				if(vrh1(MAXSTRIKE)>0.0) upperStrike_=MAXSTRIKE;
				else upperStrike_=b.solve(vrh1,HMATCHACC,atm,0.01);
				if(upperStrike_>MAXSTRIKE) upperStrike_=MAXSTRIKE; // redundant?
				VegaRatioHelper vrh2(&(*volCube_),fixingDate,tenor,atm,-1,vegaRatio_,MAXSTRIKE);
				if(vrh2(0.0001)>0.0) lowerStrike_=0.0001;
				else lowerStrike_=b.solve(vrh2,HMATCHACC,atm,0.01);
				if(lowerStrike_<0.0001) lowerStrike_=0.0001;
			}

			// calculate G's parameters (Hagan, 2.13a) tau=1/q, Delta, m=n
			double tau=0.0;
			Size m=dates.size()-1;
			for(Size i=1;i<dates.size();i++) {
				tau+=fdc.yearFraction(dates[i-1],dates[i],dates[i-1],dates[i]);
			}
			tau/=(double)m;

			double Delta=(fdc.yearFraction(fixingDate,paymentDate,fixingDate,paymentDate))/tau;
			
			//FILE *out=fopen("hagan.log","a");
			//fprintf(out,"Compute fixing %d index tenor is %d\n",fixingDate,swapIndex->tenor().length());
			// compute critical strike level whithin given interval where arbitrage occurs
			double upperBound=upperStrike_;//haganUpperBound_;//atm+vol*sqrt(tFix)*3.0
			double lowerBound=lowerStrike_;//haganLowerBound_;//atm-vol*sqrt(tFix)*3.0
			if(lowerBound<0.0001) lowerBound=0.0001;
			double upperStrikeVolCap=upperBound;
			double lowerStrikeVolCap=lowerBound;
			lastUpperBound_=upperStrike_;
			/*double strikeAdd=0.0010; // 10 Basispoint steps
			// arbitrage check for upper bound
			double lastPrice=1E+10;
			double criticalRate=atm; // atm level
			bool arbitrage=false;
			do {
				double vol2=volCube_->volatility(fixingDate,tenor,criticalRate,true)*sqrt(tFix);
				if(vol2<=0.0) vol2=0.0001;
				double prc = blackFormula(Option::Call,criticalRate,atm,vol2);
				//fprintf(out,"(up) rate %f, vol %f, price %f ",criticalRate,vol2/sqrt(tFix),prc);
				if(prc>=lastPrice && !arbitrage) { 
					arbitrage=true;
					upperStrikeVolCap=criticalRate;
					//fprintf(out,"arbitrage!");
				}
				if(prc<haganMinPrice_)
					upperBound=criticalRate;
				lastPrice=prc;
				criticalRate+=strikeAdd;
				//fprintf(out,"\n");
			} while(criticalRate < upperBound && !arbitrage);
			//fprintf(out,"\n-up-result: upperBound=%f, upperStrikeVolCap=%f\n",upperBound,upperStrikeVolCap);
			// arbitrage check for lower bound
			lastPrice=0.0;
			criticalRate=atm; // atm level
			arbitrage=false;
			do {
				double vol2=volCube_->volatility(fixingDate,tenor,criticalRate,true)*sqrt(tFix);
				if(vol2<=0.0) vol2=0.0001;
				double prc = blackFormula(Option::Call,criticalRate,atm,vol2);
				//fprintf(out,"(dwn) rate %f, vol %f, price %f",criticalRate,vol2/sqrt(tFix),prc);
				if(prc<=lastPrice && !arbitrage) { 
					arbitrage=true;
					lowerStrikeVolCap=criticalRate;
					//fprintf(out,"arbitrage!");
				}
				if(prc<haganMinPrice_)
					lowerBound=criticalRate;
				lastPrice=prc;
				criticalRate-=strikeAdd;
				//fprintf(out,"\n");
			} while(criticalRate > lowerBound && !arbitrage);
			//fprintf(out,"\n-dwn-result: lowerBound=%f, lowerStrikeVolCap=%f\n",lowerBound,lowerStrikeVolCap);
			//fclose(out);
			*/
			/*

			// set up integrand and compute adjusted swap rate
			HaganConvAdjFunction convAdjFunction(tau,Delta,m,volCube_,fixingDate,tFix,tenor,atm,lowerStrikeVolCap,upperStrikeVolCap);
			//SimpsonIntegral integral(HAGANINTEGRALACC,HAGANINTEGRALMAXSTEPS);
			//GaussKronrodNonAdaptive integral(HAGANINTEGRALACC, HAGANINTEGRALMAXSTEPS, 0.0);
			GaussKronrodAdaptive integral(HAGANINTEGRALACC, HAGANINTEGRALMAXSTEPS);

			double res1=haganG(atm,tau,Delta,m,0);
			double res2=haganG(0.0,tau,Delta,m,0);

			/*double lb=lowerBound;
			double ub=lb+0.05;
			if(ub>upperBound) ub=upperBound;
			double res3=0.0;
			double integ=0.0;
			do {
				integ=integral(convAdjFunction,lb,ub);
				res3+=integ;
				lb=ub;
				ub=lb+0.05;
				if(ub>upperBound) ub=upperBound;
			} while(lb<upperBound);*/
			/*double res3=integral(convAdjFunction,lowerBound,upperBound);

			double res4=0.0;
			//res4+=HAGANLOWERBOUND*convAdjFunction(HAGANLOWERBOUND/2.0); // Correction term because we can not choose 0 as lower bound
			
			double res5=atm;
			
			double adjRate = 1.0/res1*(res2*res5+res3+res4); 

			return adjRate - atm;*/

			return 0.0;

		}	

		if(convAdjMode_==2) { // cms replication adjustment

			Brent b;

			// determine lowerStrike and upperStrike from vegaRatio, if given
			if(vegaRatio_>0.0) {
				/*double z1=-vol*vol*tFix/2.0+sqrt(vol*vol*vol*vol*tFix*tFix/4.0-2.0*vol*vol*tFix*log(vegaRatio_));
				double z2=-vol*vol*tFix/2.0-sqrt(vol*vol*vol*vol*tFix*tFix/4.0-2.0*vol*vol*tFix*log(vegaRatio_));
				lowerStrike_=atm*exp(-z1);
				upperStrike_=atm*exp(-z2);
				if(lowerStrike_<0.0001) lowerStrike_=0.0001;
				if(upperStrike_>MAXSTRIKE) upperStrike_=MAXSTRIKE;*/
				if(useStdCube_) {
					VegaRatioHelper vrh1(&(*volCube2_),&(*smileSec),fixingDate,tenor,atm,1,vegaRatio_,maxVolStrike);
					if(vrh1(MAXSTRIKE)>0.0) upperStrike_=MAXSTRIKE;
					else upperStrike_=b.solve(vrh1,HMATCHACC,atm,0.01);
					if(upperStrike_>MAXSTRIKE) upperStrike_=MAXSTRIKE; // redundant?
					VegaRatioHelper vrh2(&(*volCube2_),&(*smileSec),fixingDate,tenor,atm,-1,vegaRatio_,maxVolStrike);
					if(vrh2(0.0001)>0.0) lowerStrike_=0.0001;
					else lowerStrike_=b.solve(vrh2,HMATCHACC,atm,0.01);
					if(lowerStrike_<0.0001) lowerStrike_=0.0001;
				}
				/*else {
					VegaRatioHelper vrh1(&(*volCube_),fixingDate,tenor,atm,1,vegaRatio_,maxVolStrike);
					if(vrh1(MAXSTRIKE)>0.0) upperStrike_=MAXSTRIKE;
					else upperStrike_=b.solve(vrh1,HMATCHACC,atm,0.01);
					if(upperStrike_>MAXSTRIKE) upperStrike_=MAXSTRIKE; // redundant?
					VegaRatioHelper vrh2(&(*volCube_),fixingDate,tenor,atm,-1,vegaRatio_,maxVolStrike);
					if(vrh2(0.0001)>0.0) lowerStrike_=0.0001;
					else lowerStrike_=b.solve(vrh2,HMATCHACC,atm,0.01);
					if(lowerStrike_<0.0001) lowerStrike_=0.0001;
				}*/
				
			}
			lastUpperBound_=upperStrike_;
			// determine hmin and hmax from given or computed lowerStrike and upperStrike
			// compute hmin and hmax values from lowerStrike and upperStrike bounds
			/*hmin_=-1000.0; hmax_=0.0;
			double df=1.0,dcf,hg,dfr;
			for(int z=1;z<dates.size();z++) {
				df=discountHullWhite(fixingDate,dates[z],0.0);
				dcf=fdc.yearFraction(fixingDate,dates[z]);
				dfr=1.0/pow(1.0+lowerStrike_,dcf);
				// all discount factor should be <= discount factor determined by lowerStrike
				hg=-meanReversion_*log(dfr/df)/(1.0-exp(-meanReversion_*dcf)); // mean reversion scenario
				if(hg>hmin_) hmin_=hg;
			}
			// at least the upperStrike must be reached by the last rate
			dfr=1.0/pow(1.0+upperStrike_,dcf);
			hg=-meanReversion_*log(dfr/df)/(1.0-exp(-meanReversion_*dcf)); // mean reversion scenario
			hmax_=hg;*/
			HullWhiteScenarioHelper hwhlp1(this,fixingDate,dates,fdc,lowerStrike_);
			hmin_=b.solve(hwhlp1,HMATCHACC,0.0,0.2);
			HullWhiteScenarioHelper hwhlp2(this,fixingDate,dates,fdc,upperStrike_);
			hmax_=b.solve(hwhlp2,HMATCHACC,0.0,0.2);
#ifdef CMSPLOGGING 
			FILE *out=fopen("cmsrepl.log","a");
			//fprintf(out,"Date;%d;Tenor;%d;strike;%f;flavour;%d;vegaR;%f;low;%f;high;%f;hmin;%f;hmax;%f\n",fixingDate,swapIndex->fixedLegTenor().units(),strike,flavour,
			//	vegaRatio_,lowerStrike_,upperStrike_,hmin_,hmax_);
#endif
			
			// we start at atm and move up in strike direction
			vector<double> w, strikes;

			double h,hnew,swpo,bef,srnew,sr,vol,priceSng,priceBef,annuity;
			double h0=0.0;
			double k0=swapRateHullWhite(fixingDate,dates,fdc,0.0);
			double forward=k0;
			double annuity0=annuityHullWhite(fixingDate,dates,fdc,0.0);

			int bup=1,bdwn=-1;
			
			if(strike!=0.0) {
				// compute h such that strike is matched
				HullWhiteScenarioHelper hwhlp(this,fixingDate,dates,fdc,strike);
				h0=b.solve(hwhlp,HMATCHACC,0.0,0.2);
				k0=strike;
				if(flavour==1) { bup=1; bdwn=1; }
				else { bup=-1;bdwn=-1; }
				// in case h0 does not fall into hmin_, hmax_ adjust these boundaries
				if(h0<2.0*hmin_) hmin_=h0/2.0;
				if(h0>hmax_/2.0) hmax_=h0*2.0;
			}
#ifdef CMSPLOGGING
			//fprintf(out,"h0=%1.12f, hw(h0)=%1.12f, strike=%f, forward=%f\n",h0,swapRateHullWhite(fixingDate,dates,fdc,h0),strike,forward);
			//fprintf(out,"h;sr;srnew;i;ud;vol;adjustments;w;vega\n");
#endif
			
			double hstepu = (hmax_-h0) / ((double)(hn_-2));
			double hstepd = (hmin_-h0) / ((double)(hn_-2));

			double price=0.0;
			for(int ud=bdwn;ud<=bup;ud+=2) {
				priceSng=1.0E+15;
				h=h0;
				w.clear();
				strikes.clear();
				srnew=k0;
				for(int i=0;i<hn_-1;i++) {
					priceBef=priceSng;
					hnew=h+ (ud==1 ? hstepu : hstepd);
					sr=srnew;
					annuity=annuityHullWhite(fixingDate,dates,fdc,hnew);
					srnew=swapRateHullWhite(fixingDate,dates,fdc,hnew,annuity);
					if(srnew<0.0) srnew=0.0;
					strikes.push_back(sr);
#ifdef CMSPLOGGING
					fprintf(out,"%d;%f;%d;",fixingDate,strike,flavour);
					fprintf(out,"%1.12f;%1.12f;%1.12f;",h,sr,srnew);
#endif
					// compute annuity for new scenario
					/*for(int z=1;z<dates.size();z++) {
						//cash settled //swpo+=((double)ud)*(srnew-strikes[j])*fdc.yearFraction(dates[z-1],dates[z])/pow(1.0+srnew,fdc.yearFraction(fixingDate,dates[z]));
						annuity+=fdc.yearFraction(dates[z-1],dates[z])*discountHullWhite(fixingDate,dates[z],hnew);
						//fprintf(out,"fixing=%d, dates0=%d, dates1=%d\n",fixingDate,dates[z-1],dates[z]);
					}*/
					// compute hedged part by previous swaptions
					bef=0.0;
					for(int j=0;j<i;j++) {
						bef+=w[j]*((double)ud)*(srnew-strikes[j])*annuity;
					}
					// compute new weight
					swpo=((double)ud)*(srnew-strikes[i])*annuity;
					if(swpo==0.0) {
						w.push_back(0.0);
					}
					else {
						w.push_back( 
							(((double)ud)*discountHullWhite(fixingDate,paymentDate,hnew)*(srnew-k0) - bef ) / swpo );
					}
					// plain 
					double volStrike=sr;
					if(sr<minVolStrike) volStrike=minVolStrike;
					if(sr>maxVolStrike) volStrike=maxVolStrike;
					//if(!useStdCube_) vol=volCube_->getFastVolatility(volStrike)*sqrt(tFix);
					else vol=smileSec->volatility(volStrike)*sqrt(tFix);
					
					//vol=volCube_->volCube()->volatility(fixingDate,tenor,sr,true)*sqrt(tFix); // just take underlying cube
					//smoothing (mx doc)
					/*vol=0.0;
					for(int m=-20;m<=20;m++) {
						double volStrike=sr+((double)m)*0.0001;
						vol+=volCube_->getFastVolatility(volStrike);
					}
					vol*=sqrt(tFix)/41.0;
					*/

					if(vol<=0.0) vol=0.0001;
					priceSng=annuity0*blackFormula(ud==1 ? Option::Call : Option::Put, 
						strikes[i],forward,vol);
					
					if(priceSng>priceBef) priceSng=priceBef; // cap price at previous strike price
					price+=((double)ud)*w[i]*priceSng;
					
#ifdef CMSPLOGGING
					double vega=dffixing*0.01*annuity0*blackFormulaVolDerivative(strikes[i],forward,vol,tFix);
					fprintf(out,"%d;%d;%f;%1.12f;%1.12f;%1.12f\n",i,ud,vol/sqrt(tFix),dffixing*priceSng/* / discountHullWhite(fixingDate,paymentDate,0.0)*/,w[i],vega);
#endif

					h=hnew;
				}

				// Diagnosis: how good is the portfolio hedged
				//fprintf(out,"HEDGE DIAGNOSIS\n");
				/*h=h0;
				double cfPayoff,hedgePayoff;
				for(int i=0;i<2*hn_;i++) {
					sr=swapRateHullWhite(fixingDate,dates,fdc,h);
					if(sr<0.0) sr=0.0;
					cfPayoff=discountHullWhite(fixingDate,paymentDate,h)*max(((double)ud)*(sr-k0),0.0);
					// compute annuity for new scenario
					annuity=0.0;
					for(int z=1;z<dates.size();z++) {
						//cash settled //swpo+=((double)ud)*(srnew-strikes[j])*fdc.yearFraction(dates[z-1],dates[z])/pow(1.0+srnew,fdc.yearFraction(fixingDate,dates[z]));
						annuity+=fdc.yearFraction(dates[z-1],dates[z])*discountHullWhite(fixingDate,dates[z],h);
					}
					hedgePayoff=0.0;
					for(int j=0;j<strikes.size();j++) {
						hedgePayoff+=max(((double)ud)*(sr-strikes[j]),0.0)*annuity*w[j];
					}
					//fprintf(out,"ud=%d,Scen #%d,sr=%f: CapFloor=%1.12f, Hedge=%1.12f\n",ud,i,sr,cfPayoff,hedgePayoff);
					h+=(ud==1 ? hstepu : hstepd);
				}*/
				// fprintf(out,"END OF HEDGBE DIAGNOSIS\n");
				// End Diagnosis
			}
#ifdef CMSPLOGGING
			fclose(out);
#endif
			double res = price / discountHullWhite(fixingDate,paymentDate,0.0);
			if(strike!=0.0) {
				res*=(double)bup; // invert floor price
				res-=atm;
			}
			return res;
		}

		if(convAdjMode_==3) { // no adjustment
			return 0.0;
		}

		QL_FAIL("Swap Rate Adjustment mode = " << convAdjMode_ << " is not supported");
		return 0.0;
	}

	Real CmsPricer::cmsPrice(Date calculationStartDate,
				Date calculationEndDate,
				Date fixingDate, 
				Date paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				double strike, int flavour, double margin) {

		boost::shared_ptr<VanillaSwap> fwdSwap = swapIndex->underlyingSwap(fixingDate);
		Schedule fixedSchedule = fwdSwap->fixedSchedule();
		DayCounter fdc = fwdSwap->fixedDayCount();
		vector<Date> dates = fixedSchedule.dates();

		double atm = swapRateHullWhite(fixingDate,dates,fdc,0.0);//swapIndex->fixing(fixingDate);
		double adj = swapRateAdjustment(fixingDate,paymentDate,swapIndex,strike,flavour);
		lastRate1_=atm;
		lastAdjustedRate1_=atm+adj;

		double yf=couponDayCounter.yearFraction(calculationStartDate,calculationEndDate,calculationStartDate,calculationEndDate);
		double dis = this->discount(paymentDate);
		
		return dis*yf*(atm+adj+margin);
	}

	Real CmsPricer::cmssoPrice(Date calculationStartDate,
						   Date calculationEndDate,
						   Date fixingDate, 
						   Date paymentDate,
						   boost::shared_ptr<SwapIndex> swapIndex1,
						   boost::shared_ptr<SwapIndex> swapIndex2,
						   DayCounter couponDayCounter,
						   double strike, 
						   int flavour,
						   bool usePrevRates, double pr1, double pr2, double par1, double par2) {

		return cmssoPrice(fixingDate,paymentDate,swapIndex1,swapIndex2,strike,flavour,usePrevRates,
			pr1,pr2,par1,par2)*couponDayCounter.yearFraction(calculationStartDate,calculationEndDate,calculationStartDate,calculationEndDate);
	}

	Real CmsPricer::cmssoPrice(Date fixingDate, Date paymentDate,
				boost::shared_ptr<SwapIndex> swapIndex1,
				boost::shared_ptr<SwapIndex> swapIndex2,
				double strike, 
				int flavour,
				bool usePrevRates, double pr1, double pr2, double par1, double par2) {
					
		double tFix=vdc.yearFraction(refDate,fixingDate,refDate,fixingDate);
		boost::shared_ptr<VanillaSwap> fwdSwap1 = swapIndex1->underlyingSwap(fixingDate);
		boost::shared_ptr<VanillaSwap> fwdSwap2 = swapIndex2->underlyingSwap(fixingDate);
		Schedule fixedSchedule1 = fwdSwap1->fixedSchedule();
		Schedule fixedSchedule2 = fwdSwap2->fixedSchedule();
		DayCounter fdc1 = fwdSwap1->fixedDayCount();
		DayCounter fdc2 = fwdSwap1->fixedDayCount();
		Period tenor1 = swapIndex1->tenor();
		Period tenor2 = swapIndex2->tenor();
		vector<Date> dates1 = fixedSchedule1.dates();
		vector<Date> dates2 = fixedSchedule2.dates();
		double atm1,atm2;
		if(usePrevRates) {
			atm1=pr1; atm2=pr2;
		}
		else {
			atm1 = swapRateHullWhite(fixingDate,dates1,fdc1,0.0);//swapIndex1->fixing(fixingDate);//volCube_->atmStrike(fixingDate,tenor1);
			atm2 = swapRateHullWhite(fixingDate,dates2,fdc2,0.0);//swapIndex2->fixing(fixingDate);//volCube_->atmStrike(fixingDate,tenor2);
		}
		
		double adj1,adj2;
		if(usePrevRates) {
			adj1=par1-atm1; adj2=par2-atm2;
		}
		else {
			adj1 = swapRateAdjustment(fixingDate,paymentDate,swapIndex1);
			adj2 = swapRateAdjustment(fixingDate,paymentDate,swapIndex2);
		}
		double dis = this->discount(paymentDate);

		lastRate1_=atm1;
		lastRate2_=atm2;
		lastAdjustedRate1_=atm1+adj1;
		lastAdjustedRate2_=atm2+adj2;

		double correlation=corr_->correlation(fixingDate,true);

		//volas
		double vol1,vol2;
		//simplest method (atm volas)
		if(cmssoMode_==0) {
			if(useStdCube_) {
				vol1 = volCube2_->volatility(fixingDate,tenor1,atm1,true);
				vol2 = volCube2_->volatility(fixingDate,tenor2,atm2,true);
			}
			//else {
			//	vol1 = volCube_->volatility(fixingDate,tenor1,atm1);
			//	vol2 = volCube_->volatility(fixingDate,tenor2,atm2);
			//}
		}
		//partial smile
		if(cmssoMode_==1) {
			if(useStdCube_) {
				vol1 = volCube2_->volatility(fixingDate,tenor1,atm2+strike,true);
				vol2 = volCube2_->volatility(fixingDate,tenor2,atm1-strike,true);
			}
			//else {
			//	vol1=volCube_->volatility(fixingDate,tenor1,atm2+strike,true);
			//	vol2=volCube_->volatility(fixingDate,tenor2,atm1-strike,true);
			//}
		}

		// integral Brigo 13.6.2
		double in=(*ghInt_)(CmssoFunction(atm2,atm1,atm2+adj2,atm1+adj1,vol2,vol1,
			correlation,strike,tFix,1.0,1.0,flavour));

		double res=dis*in;
		
		lastAdjustedSpread_=in;
		//FILE *out = fopen("peter.log","a");
		//fprintf(out,"fixing;%d;correlation;%f;price;%f\n",fixingDate,correlation,res);
		//fclose(out);
		//FILE* out=fopen("Y:\daten\caspers\CMSSO.log","a");
		//fprintf(out,"***;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f\n",vol2,vol1,atm2,atm1,adj2,adj1,correlation,strike,tFix,dis,in);
		//fclose(out);
		
		return res;
	}

	
	CmssoFunction::CmssoFunction(const double swapRate1,const double swapRate2,const double adjustedSwapRate1,const double adjustedSwapRate2,
							 const double vol1,const double vol2,const double rho,const double strike,const double t,
							 const double mult1,const double mult2, const int flavor):
	swapRate1_(swapRate1), swapRate2_(swapRate2), adjustedSwapRate1_(adjustedSwapRate1), adjustedSwapRate2_(adjustedSwapRate2),
	vol1_(vol1),vol2_(vol2),rho_(rho),strike_(strike),t_(t),
	mult1_(mult1),mult2_(mult2),flavor_(flavor) {}

	double CmssoFunction::operator ()(const double& x) const {
		// this is Brigo, 13.16.2 with x = v/sqrt(2), v=sqrt(2)*x
		CumulativeNormalDistribution cnd(0.0,1.0);
		double mu1_=1.0/t_*log(adjustedSwapRate1_/swapRate1_);
		double mu2_=1.0/t_*log(adjustedSwapRate2_/swapRate2_);
		double v_=sqrt(2.0)*x;
		double h_=strike_+mult1_*swapRate1_*exp((mu1_-0.5*vol1_*vol1_)*t_+vol1_*sqrt(t_)*v_);
		double phi1_,phi2_;
		if(mult2_*swapRate2_/h_>0) {
			phi1_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_+(0.5-rho_*rho_)*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
				(vol2_*sqrt(t_*(1.0-rho_*rho_))));
			phi2_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_-0.5*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
				(vol2_*sqrt(t_*(1.0-rho_*rho_))));
		}
		else {
			phi1_= flavor_ == 1 ? 1.0 : 0.0;
			phi2_= phi1_;
		}
		double f=mult2_*(double)flavor_*swapRate2_*exp(mu2_*t_-0.5*rho_*rho_*vol2_*vol2_*t_+rho_*vol2_*sqrt(t_)*v_)*phi1_-(double)flavor_*h_*phi2_;
		return 1.0/sqrt(M_PI)*exp(-x*x)*f;
	}

	double CmsPricer::haganG(const double x, const double t, const double d, const Size n, const int diffOrder) {
		double eps=1.0E-5;
		switch(diffOrder) {
			case 0:	if(x==0)
						return 1.0/(t*(double)n);
					else
						return x/pow(1.0+t*x,d)*1.0/(1.0-(1.0/pow(1.0+t*x,(int)n)));
					break;
			case 1: return /*(haganG(x+eps,t,d,n,0)-haganG(x,t,d,n,0))/eps;*/ 										
						(pow(1.0+t*x,n-1.0)*(1.0+t*x-t*x*n)-1.0)/pow(pow(1.0+t*x,(double)n)-1.0,2.0)*
						pow(1.0+t*x,n-d)+x*t*(n-d)*pow(1.0+t*x,n-d-1.0)/(pow(1.0+t*x,(double)n)-1.0);
					break;
			case 2: return /*(haganG(x+2.0*eps,t,d,n,0)+haganG(x,t,d,n,0))/(eps*eps);*/
						(((n-1.0)*pow(1.0+t*x,n-2.0)*(1.0+t*x-t*x*n)+pow(1.0+t*x,n-1.0)*(t-t*n))*pow((pow(1.0+t*x,(double)n)-1.0),2)-
						(pow(1.0+t*x,n-1.0)*(1.0+t*x-t*x*n)-1.0)*2.0*(pow(1.0+t*x,(double)n)-1.0)*n*t*pow(1.0+t*x,n-1.0))/pow(pow(1.0+t*x,(double)n)-1.0,4)*
						pow(1.0+t*x,(double)n-d)+(pow(1.0+t*x,(double)n-1.0)*(1.0+t*x-t*x*n)-1.0)/pow(pow(1.0+t*x,(double)n)-1.0,2)*(n-d)*t*pow(1.0+t*x,n-d-1.0)+
						((t*(n-d)*pow(1.0+t*x,(double)n-d-1.0)+x*t*(n-d)*t*(n-d-1.0)*pow(1.0+t*x,(double)n-d-2.0))*(pow(1.0+t*x,(double)n)-1.0)-
						n*t*pow(1.0+t*x,(double)n-1.0)*x*t*(n-d)*pow(1.0+t*x,(double)n-d-1.0))/pow((pow(1.0+t*x,(double)n)-1.0),2); 
					break;
			default: QL_FAIL("Hagans G can not computed for given diffOrder");
					return 0.0;
					break;
		}
	}

	double CmsPricer::discountHullWhite(const Date& d1, const Date& d2, double h) {
		double sc0=this->discount(d2)/this->discount(d1);
		double dt=dc_.yearFraction(d1,d2);
		// parallel start
		//double rate = -log(sc0)/dt;
		//double rateN = rate + h;
		//return exp(-rateN*dt);
		// parallel end
		return sc0*exp(-h/meanReversion_*(1.0-exp(-meanReversion_*dt)));
	}
		
	double CmsPricer::swapRateHullWhite(const Date& dfix, const vector<Date>& fixedDates,const DayCounter& fdc, double h, double ann) {
		
		int sz = fixedDates.size();
		//DayCounter fdc = fwdSwap->fixedDayCount();
		//double sum=0.0;
		//for(int i=1;i<sz;i++) {
		//	sum+=fdc.yearFraction(fixedDates[i-1],fixedDates[i])*discountHullWhite(dfix,fixedDates[i],h);
		//}
		double disc0=discountHullWhite(dfix,fixedDates[0],h);
		double disc1=discountHullWhite(dfix,fixedDates[sz-1],h);
		if(ann==0.0) ann=annuityHullWhite(dfix,fixedDates,fdc,h);
		double res=(disc0-disc1)/ann;
		/*if(h==0.0) {
			FILE* out=fopen("swap.log","a");
			fprintf(out,"SWAP RATE Fixing %d\n",dfix);
			for(int i=0;i<fixedDates.size();i++) {
				fprintf(out,"%d\n",fixedDates[i]);
			}
			fprintf(out, "Discount fixing - end = %f, annuity=%f, result=%f\n",disc,ann,res);
			fclose(out);
		}*/
		//if(res<0.0) res=0.0;
		return res;
	}

	double CmsPricer::annuityHullWhite(const Date& dfix, const vector<Date>& fixedDates, const DayCounter& fdc,double h) {
		//boost::shared_ptr<VanillaSwap> fwdSwap = index->underlyingSwap(dfix);
		//std::vector<Date> fixedDates = fwdSwap->fixedSchedule().dates();
		int sz = fixedDates.size();
		//DayCounter fdc = fwdSwap->fixedDayCount();
		double sum=0.0;
		for(int i=1;i<sz;i++) {
			sum+=fdc.yearFraction(fixedDates[i-1],fixedDates[i])*discountHullWhite(dfix,fixedDates[i],h);
		}
		return sum;
	}

	double CmsPricer::discount(const Date& d) {
		return yts_->discount(d,true) / yts_->discount(settlementDate_,true);
	}

	/*HaganConvAdjFunction::HaganConvAdjFunction(const double& tau,const double& Delta,const Size& m,
			boost::shared_ptr<SabrRbsSmile> volCube,const Date& dSwapStart,const Time& tSwapStart, const Period& tSwpLng,
			const Rate& swapRate,const double strikeVolFloor,const double strikeVolCap):
			tau_(tau),Delta_(Delta),m_(m),volCube_(volCube),tSwapStart_(tSwapStart),dSwapStart_(dSwapStart),
				tSwpLng_(tSwpLng), swapRate_(swapRate), strikeVolCap_(strikeVolCap), strikeVolFloor_(strikeVolFloor) {}

	double HaganConvAdjFunction::operator()(double x) const {
	
		double res1=CmsPricer::haganG(x,tau_,Delta_,m_,2)*(x)+2.0*CmsPricer::haganG(x,tau_,Delta_,m_,1);
		double res2;
		double xCap=x;
		double xFloor=x;
		if(strikeVolCap_>0.0 && x>strikeVolCap_) xCap=strikeVolCap_;
		if(strikeVolFloor_>0.0 && x<strikeVolFloor_) xFloor=strikeVolFloor_;
		double vol=volCube_->volatility(dSwapStart_,tSwpLng_,xCap,false,false)*sqrt(tSwapStart_);
		if(vol<=0.0) vol=0.0001;
		res2=blackFormula(Option::Call,x,swapRate_,vol);
		//FILE *out=fopen("hagan.log","a");
		//fprintf(out,"%f;%d;%f;%f\n",tSwapStart_,tSwpLng_.length(),x,res1*res2);
		//fclose(out);
		return res1*res2;

	}*/

	HullWhiteScenarioHelper::HullWhiteScenarioHelper(CmsPricer *pricer, const Date& dFix, const vector<Date>& dates, const DayCounter& fdc,double targetSwapRate):
		pricer_(pricer), dates_(dates), fdc_(fdc), dFix_(dFix), targetSwapRate_(targetSwapRate) {}

	
	double HullWhiteScenarioHelper::operator()(const double& x) const {
		return pricer_->swapRateHullWhite(dFix_,dates_,fdc_,x)-targetSwapRate_;
	}

	/*VegaRatioHelper::VegaRatioHelper(SabrRbsSmile* volCube, const Date& dFix, const Period& tenor, const double forward, const int direction, const double targetRatio, const double maxStrike) :
	volCube_(volCube), dFix_(dFix), tenor_(tenor), direction_(direction), targetRatio_(targetRatio), MAXSTRIKE(maxStrike), forward_(forward) {

		DayCounter vdc;
		Date refDate;
		vdc = volCube_->volCube()->dayCounter();
		refDate = volCube_->volCube()->referenceDate();
				
		tFix_=vdc.yearFraction(refDate,dFix_);

		useStdCube_=false;
	
	}*/

	VegaRatioHelper::VegaRatioHelper(SwaptionVolatilityStructure* volCube2, SmileSection* smileSec, const Date& dFix, const Period& tenor, const double forward, const int direction, const double targetRatio, const double maxStrike) :
	volCube2_(volCube2), smileSec_(smileSec), dFix_(dFix), tenor_(tenor), direction_(direction), targetRatio_(targetRatio), MAXSTRIKE(maxStrike), forward_(forward) {

		DayCounter vdc;
		Date refDate;
		vdc = volCube2_->dayCounter();
		refDate = volCube2_->referenceDate();
				
		tFix_=vdc.yearFraction(refDate,dFix_);

		useStdCube_=true;
	
	}

	double VegaRatioHelper::operator()(const double& x) const {
		double r = vega(x) / vega(forward_);
		if( (direction_==1 && x < forward_) || (direction_==-1 && x > forward_) )
			r = 1.0 / r;
		//FILE *out=fopen("cmsrepl.log","a");
		//fprintf(out,"BRENT. strike=%f, forward=%f, vega(strike)=%f, vega(forward)=%f, target=%f\n",x,forward_,vega(x),vega(forward_),r - targetRatio_);
		//fclose(out);
		return r - targetRatio_;
	}

	double VegaRatioHelper::vega(const double& strike) const {
		if(strike<=0.0) return 0.0;
		double volStrike=strike;//forward_;
		if(strike>MAXSTRIKE) volStrike=MAXSTRIKE;
		double vol;
		if(useStdCube_) vol = smileSec_->volatility(volStrike);
		//else vol = volCube_->getFastVolatility(volStrike);
		//vol = volCube_->volatility(dFix_,tenor_,volStrike,false,false);
		double vega=blackFormulaVolDerivative(strike,forward_,vol*sqrt(tFix_),tFix_);
		return vega;
	}
	
}