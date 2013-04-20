#include "HW2FAdj.hpp"

namespace QuantLib {

	HW1FAdj::HW1FAdj(const double a, 
				const std::vector<double>& times,
				const std::vector<double>& sigma,
				boost::shared_ptr<YieldTermStructure> yieldTS) :
		a_(a), times_(times), sigma_(sigma), yieldTS_(yieldTS) {
	}


	double HW1FAdj::expectationX(const Size index, const Size payIndex) {
		double sum=0.0;
		double T=times_[payIndex];
		double t=times_[index];
		for(int i=0;i<=index;i++) {
			double sigma=sigma_[i];
			double eta=eta_[i];
			double rho=rho_[i];
			double t1=times_[i];
			double t0= i==0 ? 0.0 : times_[i-1];
			/*sum+=(sigma*sigma/(a_*a_)+rho_*sigma*eta/(a_*b_))*(1.0-exp(-a_*(t1-t0)))-
					sigma*sigma/(2.0*a_*a_)*(exp(-a_*(T-t1))-exp(-a_*(T+t1-2.0*t0)))-
					rho_*sigma*eta/(b_*(a_+b_))*(exp(-b_*(T-t1))-exp(-b_*T-a_*t1+(a_+b_)*t0));*/
			sum+=(sigma*sigma)/(a_*a_)*((exp(-a_*(t-t1))-exp(-a_*(t-t0)))-0.5*(exp(-a_*(T+t-2.0*t1))-exp(-a_*(T+t-2.0*t0))))+
				  rho*sigma*eta/b_*((exp(-a_*(t-t1))-exp(-a_*(t-t0)))/a_-(exp((a_+b_)*t1-a_*t-b_*T)-exp((a_+b_)*t0-a_*t-b_*T))/(a_+b_));
		}
		return -sum;
	}
	
	double HW2FAdj::expectationY(const Size index, const Size payIndex) {
		double sum=0.0;
		double T=times_[payIndex];
		double t=times_[index];
		for(int i=0;i<=index;i++) {
			double sigma=sigma_[i];
			double eta=eta_[i];
			double rho=rho_[i];
			double t1=times_[i];
			double t0= i==0 ? 0.0 : times_[i-1];
			/*sum+=(eta*eta/(b_*b_)+rho_*sigma*eta/(a_*b_))*(1.0-exp(-b_*(t1-t0)))-
					eta*eta/(2.0*b_*b_)*(exp(-b_*(T-t1))-exp(-b_*(T+t1-2.0*t0)))-
					rho_*sigma*eta/(a_*(a_+b_))*(exp(-a_*(T-t1))-exp(-a_*T-b_*t1+(a_+b_)*t0));*/
			sum+=(eta*eta)/(b_*b_)*((exp(-b_*(t-t1))-exp(-b_*(t-t0)))-0.5*(exp(-b_*(T+t-2.0*t1))-exp(-b_*(T+t-2.0*t0))))+
				  rho*sigma*eta/a_*((exp(-b_*(t-t1))-exp(-b_*(t-t0)))/b_-(exp((a_+b_)*t1-b_*t-a_*T)-exp((a_+b_)*t0-b_*t-a_*T))/(a_+b_));
		}
		return -sum;
	}

	double HW2FAdj::varianceX(const Size index) {
		double sum=0.0;
		double t=times_[index];
		for(int i=0;i<=index;i++) {
			double sigma=sigma_[i];
			double t1=times_[i];
			double t0= i==0 ? 0.0 : times_[i-1];
			//sum+=sigma*sigma/(2.0*a_)*(1.0-exp(-2.0*a_*(t1-t0)));
			sum+=sigma*sigma*(exp(-2*a_*(t-t1))-exp(-2*a_*(t-t0)))/(2.0*a_);
		}
		return sum;
	}

	double HW2FAdj::varianceY(const Size index) {
		double sum=0.0;
		double t=times_[index];
		for(int i=0;i<=index;i++) {
			double eta=eta_[i];
			double t1=times_[i];
			double t0= i==0 ? 0.0 : times_[i-1];
			//sum+=eta*eta/(2.0*b_)*(1.0-exp(-2.0*b_*(t1-t0)));
			sum+=eta*eta*(exp(-2*b_*(t-t1))-exp(-2*b_*(t-t0)))/(2.0*b_);
		}
		return sum;
	}

	double HW2FAdj::covarianceXY(const Size index) {
		double sum=0.0;
		double t=times_[index];
		for(int i=0;i<=index;i++) {
			double sigma=sigma_[i];
			double eta=eta_[i];
			double rho=rho_[i];
			double t1=times_[i];
			double t0= i==0 ? 0.0 : times_[i-1];
			//sum+=sigma*eta*rho_/(a_+b_)*(1.0-exp(-(a_+b_)*(t1-t0)));
			sum+=sigma*eta*rho/(a_+b_)*(exp((a_+b_)*(t1-t))-exp((a_+b_)*(t0-t)));
		}
		return sum;
	}

	double HW2FAdj::correlationXY(const Size index) {
		return covarianceXY(index)/(sqrt(varianceX(index))*sqrt(varianceY(index)));
	}

	double HW2FAdj::zeroCouponPrice(const Size startIndex, const Size endIndex, const double x, const double y,const double adjuster,
		const double startTimeMin, const double endTimeMax) {
			double T=times_[endIndex] <= endTimeMax ? times_[endIndex] : endTimeMax;
			double th=startIndex==-1 ? 0.0 : times_[startIndex];
			double t=th >= startTimeMin ? th : startTimeMin;
		// calculate exp(-integral_t^T(phi))
		double ephi=yieldTS_->discount(T)/yieldTS_->discount(t)
			*exp(-0.5*(varianceI(-1,endIndex,adjuster,0.0,T)-varianceI(-1,startIndex==-1?0:startIndex,adjuster,0.0,t)));
		// return result
		return ephi*exp(-(1.0-exp(-a_*(T-t)))/a_*adjuster*x-(1.0-exp(-b_*(T-t)))/b_*adjuster*y+0.5*varianceI(startIndex,endIndex,adjuster,t,T));
	}

	double HW2FAdj::varianceI(const int index0, const int index1,const double adjuster, const double startTimeMin, const double endTimeMax) {
		double sum=0.0;
		double T=times_[index1]<=endTimeMax ? times_[index1] : endTimeMax;
		for(int i=index0+1;i<=index1;i++) {
			double sigma=sigma_[i]*adjuster;
			double eta=eta_[i]*adjuster;
			double rho=rho_[i];
			double t1=times_[i] <= endTimeMax ? times_[i] : endTimeMax;
			double t0= i==0 ? 0.0 : (times_[i-1]>=startTimeMin ? times_[i-1] : startTimeMin);
			sum+=sigma*sigma/(a_*a_)*(t1-t0 - 2.0/a_*(exp(-a_*(T-t1))-exp(-a_*(T-t0))) +
											  1.0/(2.0*a_)*(exp(-2.0*a_*(T-t1))-exp(-2.0*a_*(T-t0))));
			sum+=eta*eta/(b_*b_)*(t1-t0 - 2.0/b_*(exp(-b_*(T-t1))-exp(-b_*(T-t0))) +
											  1.0/(2.0*b_)*(exp(-2.0*b_*(T-t1))-exp(-2.0*b_*(T-t0))));
			sum+=2.0*rho*sigma*eta/(a_*b_)*(t1-t0 - 1.0/a_*(exp(-a_*(T-t1))-exp(-a_*(T-t0))) -
													 1.0/b_*(exp(-b_*(T-t1))-exp(-b_*(T-t0))) +
													 1.0/(a_+b_)*(exp(-(a_+b_)*(T-t1))-exp(-(a_+b_)*(T-t0))));
			//test
			/*sum=(sigma*sigma)/(a_*a_)*(T-t0+2.0/a_*exp(-a_*(T-t0))-1.0/(2.0*a_)*exp(-2.0*a_*(T-t0))-3.0/(2.0*a_))+
				(eta*eta)/(b_*b_)*(T-t0+2.0/b_*exp(-b_*(T-t0))-1.0/(2.0*b_)*exp(-2.0*b_*(T-t0))-3.0/(2.0*b_))+
				2.0*rho_*sigma*eta/(a_*b_)*(T-t0+1.0/a_*(exp(-a_*(T-t0))-1.0)+1.0/b_*(exp(-b_*(T-t0))-1.0)-1.0/(a_+b_)*(exp(-(a_+b_)*(T-t0))-1.0));
			*/
		}
		return sum;
	}

	double HW2FAdj::swapRate(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y, const double adjuster) {
		return (1.0-zeroCouponPrice(startIndex,startIndex+swapLength,x,y,adjuster))/swapAnnuity(startIndex,swapLength,payFreq,x,y,adjuster);
	}

	double HW2FAdj::swapAnnuity(const Size startIndex, const Size swapLength, const Size payFreq, const double x, const double y, const double adjuster) {
		QL_REQUIRE(swapLength>0,"Swap Length must be >0");
		QL_REQUIRE(payFreq>0,"Pay Freq must be >0");
		double annuity=0.0;
		for(int i=startIndex+payFreq;i<=startIndex+swapLength;i+=payFreq) {
			annuity+=zeroCouponPrice(startIndex,i,x,y,adjuster)*(times_[i]-times_[i-payFreq]);
		}
		return annuity;
	}
	
	// Achtung: Payment=Fixing wird hier unterstellt. Ansonsten muss noch der Payoff mit P(fixing,payment) multipliziert werden unten
	double HW2FAdj::spreadOptionPrice(const Size fixing, const Size payment,const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, 
			const Size mode,const double adjuster, const double lastCorr, const bool useMC) {
		

		if(lastCorr>=-1.0 && lastCorr<=1.0) // set last correlation if valid
			rho_[fixing] = lastCorr;

		if(useMC) {
			MersenneTwisterUniformRng mt(mcseed_);
			InverseCumulativeNormal in(0.0,1.0);

			int nods=4; // number of discretization steps between fixing times
			double prem=0.0;

			for(int mcs=0;mcs<mcpaths_;mcs++) {
				double t=0.0;
				double x=0.0;
				double y=0.0;
				double df=1.0;
				for(int tind=0;tind<=fixing;tind++) {
					double sigma=sigma_[tind];
					double eta=eta_[tind];
					double rho=rho_[tind];
					double t1=times_[tind];
					double t0= tind==0 ? 0.0 : times_[tind-1];
					double ddt=(t1-t0)/(double)nods;
					for(int dind=0;dind<nods;dind++) {
						// discount factor
						df*=zeroCouponPrice(tind-1,tind,x,y,1.0,t,t+ddt);
						//
						double dw1=in(mt.next().value);
						double dw2=in(mt.next().value);
						dw2 = dw1*rho+dw2*sqrt(1.0-rho*rho);
						x+=-a_*x*ddt+sigma*sqrt(ddt)*dw1;
						y+=-b_*y*ddt+eta*sqrt(ddt)*dw2;
						t+=ddt;
					}
				}
				double payoff;
				double p1=swapRate(fixing,swapLength1,swapPayFreq,x,y,adjuster);
				double p2,p;
				if(mode==4) {
					p=p1-strike;
				}
				else {
					p2=swapRate(fixing,swapLength2,swapPayFreq,x,y,adjuster);
					p=p1-p2-strike;
				}
				switch(mode) {
					case 0:	payoff=std::max(w*p,0.0)*df; break;
					case 1: payoff=(w*p>0.0 ? 1.0:0.0)*df; break;
					case 2: payoff=p1; break;
					case 3: payoff=p2; break;
					case 4: payoff=std::max(w*p,0.0)*swapAnnuity(fixing,swapLength1,swapPayFreq,x,y,adjuster)*df; break;
					default: return 0.0;
				};
				prem+=payoff;
			}
			return prem/(double)mcpaths_;
		}
		else {
			PayOff p(this,fixing,payment,strike,swapLength1,swapLength2,swapPayFreq,w,mode,
					expectationX(fixing,payment),expectationY(fixing,payment),
					sqrt(varianceX(fixing)),sqrt(varianceY(fixing)),
					correlationXY(fixing),adjuster);
			PayOffX px(&p);
			//GaussKronrodNonAdaptive outerInt(1.0E-4, 1000000, 0.0);
			//SimpsonIntegral outerInt(1.0E-3,10000);
			return outerInt_->operator()(px)* ( mode == 0 || mode == 1 ? yieldTS_->discount(times_[payment]) : 1.0 );
		}

	}

	// Achtung: Payment=Fixing wird hier unterstellt. Ansonsten muss noch der Payoff mit P(fixing,payment) multipliziert werden unten
	double HW2FAdj::spreadOptionPriceDC(const Size fixing, const Size payment,const double strike,
			const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, 
			const Size mode,const double adjuster,const bool useMC) {

		
			PayOffD p(this,fixing,payment,strike,swapLength1,swapLength2,swapPayFreq,w,mode,
					expectationX(fixing,payment),expectationY(fixing,payment),
					sqrt(varianceX(fixing)),sqrt(varianceY(fixing)),
					correlationXY(fixing),adjuster);
			return outerInt_->operator()(p)* ( mode == 0 || mode == 1 || mode == 4 ? yieldTS_->discount(times_[payment]) : 1.0 );
		

	}

	PayOff::PayOff(HW2FAdj* hw, const Size fixing, const Size payment, const double strike,
		const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, const Size mode,
		const double mux, const double muy, const double sigmax, const double sigmay, const double rho,const double adjuster) {
		hw_=hw;
		fixing_=fixing;
		payment_=payment;
		strike_=strike;
		swapLength1_=swapLength1;
		swapLength2_=swapLength2;
		swapPayFreq_=swapPayFreq;
		w_=w;
		mode_=mode;
		mux_=mux;
		muy_=muy;
		sigmax_=sigmax;
		sigmay_=sigmay;
		rho_=rho;
		adjuster_=adjuster;
		y_=0.0;
	}

	double PayOff::operator()(double x, double y) const {
		// transform x,y back to original paramters xo, yo
		double yo=sqrt(2.0)*sigmay_*y+muy_;
		double xo=(sigmax_*sigmay_*sqrt(2.0)*sqrt(1.0-rho_*rho_)*x+sigmay_*mux_+rho_*sigmax_*yo-rho_*sigmax_*muy_)/sigmay_;
		// compute payoff
		double p1=hw_->swapRate(fixing_,swapLength1_,swapPayFreq_,xo,yo,adjuster_);
		double p=0.0,p2=0.0,annuity=0.0;
		if(mode_==4) {
			annuity=hw_->swapAnnuity(fixing_,swapLength1_,swapPayFreq_,xo,yo,adjuster_);
			p=p1-strike_;
		}
		else {
			p2=hw_->swapRate(fixing_,swapLength2_,swapPayFreq_,xo,yo,adjuster_);
			p=p1-p2-strike_;
		}
		// compute density for x
		double d=1.0/sqrt(3.141592653589793238462643)*exp(-x*x);
		// density d2 without transformation
		//double d2=1.0/sqrt(2.0*3.141592653589793238462643)*1.0/(sigmax_*sqrt(1.0-rho_*rho_))*exp(-0.5*((x-mux_)/sigmax_-rho_*(y-muy_)/sigmay_)*((x-mux_)/sigmax_-rho_*(y-muy_)/sigmay_)/((1.0-rho_*rho_)));
		double z=0.0;
		switch(mode_) {
			case 0:	z= d*std::max(w_*p,0.0); break;
			case 1: z= d*(w_*p>0.0 ? 1.0:0.0); break;
			case 2: z= d*p1; break;
			case 3: z= d*p2; break;
			case 4: z= d*std::max(w_*p,0.0)*annuity; break;
			default: z= 0.0;
		};
		return z;
	}

	double PayOff::operator()(double x) const {
		return operator()(x,y_);
	}

	PayOffX::PayOffX(PayOff* p) {
		p_=p;
	}

	double PayOffX::operator()(double y) const {
		//GaussHermiteIntegration innerInt(16);
		//GaussKronrodNonAdaptive innerInt(1.0E-4, 1000000, 0.0);
		//SimpsonIntegral innerInt(1.0E-3,10000);
		p_->y_=y;
		double py=p_->hw_->innerInt_->operator()(*p_);
		// compute density for y
		double d=1.0/sqrt(3.141592653589793238462643)*exp(-y*y);
		// density d2 without transformation
		//double sigmay=p_->sigmay_;
		//double muy=p_->muy_;
		//double d2=1.0/sqrt(2.0*3.141592653589793238462643)*1.0/sigmay*exp(-0.5*(y-muy)/sigmay*(y-muy)/sigmay);
		// return result
		/*FILE *out=fopen("hw.log","a");
		fprintf(out,"---- py=%f, d2=%f\n",py,d2);
		fclose(out);*/
		return d*py;
	}

	PayOffD::PayOffD(HW2FAdj* hw, const Size fixing, const Size payment, const double strike,
		const Size swapLength1, const Size swapLength2, const Size swapPayFreq, const double w, const Size mode,
		const double mux, const double muy, const double sigmax, const double sigmay, const double rho,const double adjuster) {
		
		hw_=hw;
		fixing_=fixing;
		payment_=payment;
		strike_=strike;
		swapLength1_=swapLength1;
		swapLength2_=swapLength2;
		swapPayFreq_=swapPayFreq;
		w_=w;
		mode_=mode;
		mux_=mux;
		muy_=muy;
		sigmax_=sigmax;
		sigmay_=sigmay;
		rho_=rho;
		adjuster_=adjuster;
	}

	double PayOffD::operator()(double x, double y) const {
			// transform x,y back to original paramters xo, yo
			double yo=sqrt(2.0)*sigmay_*y+muy_;
			double xo=(sigmax_*sigmay_*sqrt(2.0)*sqrt(1.0-rho_*rho_)*x+sigmay_*mux_+rho_*sigmax_*yo-rho_*sigmax_*muy_)/sigmay_;
			// compute payoff
			double p1=hw_->swapRate(fixing_,swapLength1_,swapPayFreq_,xo,yo,adjuster_);
			double p2=hw_->swapRate(fixing_,swapLength2_,swapPayFreq_,xo,yo,adjuster_);
			double p=p1-p2-strike_;
			return p;
	}

	double PayOffD::operator()(double y) const {
		PayOffH f(this,y);
		CumulativeNormalDistribution c;
		Brent b;
		// we have to find the intervals where f > 0 ...
		// this is a heuristic where we assume, that not too many sign changes happen
		const double q[] = { // 0.1%, 5%, 10%, 20%, 30%, ... , 90%, 95%, 99.9% normal quantiles divided by sqrt(2)
			-2.18512421913299,-1.16308715367668,-0.906193802436823,-0.595116081449995,
			-0.370807158593558,-0.179143454621292,0.0,0.179143454621292,
			0.370807158593558,0.595116081449995,0.906193802436822,1.16308715367667,
			2.18512421913298
		};
		double x0=q[0];
		double sum=0.0;
		double x=x0;
		double xn;
		bool done=false, signChange=false;
		for(int i=0;i<12;i++) {
			xn=q[i+1];
			if(f(x)*f(xn)<0) {
				double s=b.solve(f,1E-12,(x+xn)/2.0,x,xn);
				if(f(x)>0.0) {
					sum+=c(sqrt(2.0)*s)- (i>0 ? c(sqrt(2.0)*x0) : 0.0 );
					done=true;
				}
				else {
					done=false;
				}
				x0=s;
				signChange=true;
				//fprintf(out,"FOUND s=%f sum=%f\n",s,sum);
			}
			x=xn;
		}
		if(!done && f(x)>0) {
			sum+=c(sqrt(2.0)*x)- (signChange ? c(sqrt(2.0)*x0) : 0.0);
		}
		//fclose(out);
		double d=1.0/sqrt(3.141592653589793238462643)*exp(-y*y);
		return sum*d;
	}

	PayOffH::PayOffH(const PayOffD* p, double a): p_(p), y(a) {}

	double PayOffH::operator()(double x) const {
		return p_->operator()(x,y);
	}

	

}