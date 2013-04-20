#include <effectiveParametersSVLfm.hpp>
#include <stdio.h>


namespace QuantLib {

	/*// for zero strike cmsso integration
	double cmsFctZeroSin(double t, void * params) {
		CharFctHeston cf = *(CharFctHeston*)params;
		complex<double> xi(2.0,t);
		complex<double> res=cf(xi,1.0-xi)/(xi*(xi-1.0));
		//printf("%f;%f\n",t,-res.imag());
		return -res.imag();
	}
	double cmsFctZeroCos(double t, void * params) {
		CharFctHeston cf = *(CharFctHeston*)params;
		complex<double> xi(2.0,t);
		complex<double> res=cf(xi,1.0-xi)/(xi*(xi-1.0));
		return res.real();
	}
	double cmsFctZeroOmega_=0.0;
	double cmsFctZero(double t, void * params) {
		CharFctHeston cf = *(CharFctHeston*)params;
		complex<double> xi(2.0,t);
		complex<double> res=cf(xi,1.0-xi)/(xi*(xi-1.0))*exp(-xi*cmsFctZeroOmega_);
		return res.real();
	}
	
	// for non zero strike cmsso integration helpers
	gsl_integration_qawo_table *qawo_table_sine_I, *qawo_table_cosine_I; // for gsl qawo integration
	gsl_integration_qawo_table *qawo_table_sine_O, *qawo_table_cosine_O; 
	gsl_integration_workspace *qawo_ws;
	gsl_function fIntRe, fIntIm; // inner integrand re, im (without sin, cosine terms)
	gsl_function fOutRe, fOutIm; // outer integrand re, im (without sin, cosine terms)
	complex<double> fIntXi1_; // fixed xi1 for evaluation of inner integral
	CharFctHeston *charFctHest; // characteristic function
	double omegaO,omegaI; // ln alpha, ln beta
	double alpha=-0.0; // transformation (u,v) -> (xi1=u, xi2=alpha*u+v)

	complex<double> cmsFctInnerInt(complex<double> xi1) { // evaluate inner integral for given xi1
		fIntXi1_=xi1;
		double result1, result2, result3, result4;
		double abserr1, abserr2, abserr3, abserr4;
		cout << "Inner Integral evaluation at xi1=" << xi1 << ":" << endl;
		int rc1=gsl_integration_qawo(&fIntRe, -INTEGRATIONBOUNDV, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_cosine_I , &result1, &abserr1);
		cout << "     rc1 " << rc1 << " res " << result1 << " err " << abserr1 << " size " << qawo_ws->size << endl;
		int rc2=gsl_integration_qawo(&fIntIm, -INTEGRATIONBOUNDV, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_sine_I , &result2, &abserr2);
		cout << "     rc2 " << rc2 << " res " << result2 << " err " << abserr2 << " size " << qawo_ws->size << endl;
		int rc3=gsl_integration_qawo(&fIntRe, -INTEGRATIONBOUNDV, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_sine_I , &result3, &abserr3);
		cout << "     rc3 " << rc3 << " res " << result3 << " err " << abserr3 << " size " << qawo_ws->size << endl;
		int rc4=gsl_integration_qawo(&fIntIm, -INTEGRATIONBOUNDV, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_cosine_I , &result4, &abserr4);
		cout << "     rc4 " << rc4 << " res " << result4 << " err " << abserr4 << " size " << qawo_ws->size << endl;
		complex<double> res(result1+result2,result4-result3);
		return res;
	}
	double cmsFctOuterRe(double t, void* params) { // outer integrand real part
		complex<double> xi1(3.0,t);
		complex<double> I = cmsFctInnerInt(xi1);
		//complex<double> res = I / (xi1*(xi1-1.0)); // periodic assumption
		complex<double> res = I / (xi1*(xi1-1.0)) * exp(-omegaO*(xi1-1.0));
		// debug
		FILE *out=fopen("cmssoI.txt","a");
		fprintf(out,"%1.12f %1.12f %1.12f\n",xi1.imag(),res.real(),res.imag());
		fclose(out);		
		return res.real();
	}
	double cmsFctOuterIm(double t, void* params) { // outer integrand im part
		complex<double> xi1(3.0,t);
		complex<double> I = cmsFctInnerInt(xi1);
		//complex<double> res = I / (xi1*(xi1-1.0)); // periodic assumption
		complex<double> res = I / (xi1*(xi1-1.0)) * exp(-omegaO*(xi1-1.0));
		return res.imag();
	}
	double cmsFctOuterInt() { // compute outer integral (final result)
		// periodic
		double result1, result2;
		double abserr1, abserr2;
		int rc1=gsl_integration_qawo(&fOutRe, -INTEGRATIONBOUNDU, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_cosine_O , &result1, &abserr1);
		int rc2=gsl_integration_qawo(&fOutIm, -INTEGRATIONBOUNDU, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, qawo_ws, qawo_table_sine_O , &result2, &abserr2);
		cout << "Outer Integral evaluation rc1 " << rc1 << " res1 " << result1 << " err1 " << abserr1 << " rc2 " << rc2 << " res2 " << result2 << " err2 " << abserr2 << endl;
		return result1+result2;
		
		// non periodic
		double result1;
		double abserr1;
		unsigned int size;
		//int rc=gsl_integration_qag (&fOutRe, -INTEGRATIONBOUND,INTEGRATIONBOUND, INTEGRALACCURACY, 0.0, MAXINTEGRATIONSIZE, 3, qawo_ws, &result1, &abserr1);
		int rc=gsl_integration_qng (&fOutRe, -INTEGRATIONBOUNDU,INTEGRATIONBOUNDU, OUTERINTEGRALACCURACY, 0.0, &result1, &abserr1, &size);
		cout << "Outer Integral evaluation rc " << rc << " res " << result1 << " err " << abserr1 << " size " << size << endl;
		// **** gauss legendre 
		boost::shared_ptr<GaussLegendreIntegration> legendre = boost::shared_ptr<GaussLegendreIntegration>(new GaussLegendreIntegration(32)); //LAPLACEINTEGRALPOINTS
		Array legendreRoots = legendre->roots();
		Array legendreWeights = legendre->weights();
		double result1=0.0;		
		for(int i=0;i<legendreRoots.size();i++) {
			result1+=legendreWeights[i]*cmsFctOuterRe(legendreRoots[i]*INTEGRATIONBOUND,0);
		}
		result1*=INTEGRATIONBOUND;
		cout << "GAUSS LEGENDRE with " << LAPLACEINTEGRALPOINTS << " points => " << result1;
		return result1;
	}
	double cmsFctInnerRe(double t, void* params) { // inner integrand real part
		//complex<double> xi2(-1.0,t); // untransformed
		complex<double> xi2(-1.0,alpha*fIntXi1_.imag()+t); // transformed
		// debug
		complex<double> res1=(*charFctHest)(fIntXi1_,xi2);
		complex<double> res2=exp( gammaLn(-xi2)+gammaLn(fIntXi1_+xi2-1.0)-gammaLn(fIntXi1_-1.0) );
		complex<double> res=res1*res2;
		char name[30];
		sprintf(name,"cmssoI_%1.3f.txt",fIntXi1_.imag());
		FILE *out=fopen(name,"a");
		fprintf(out,"%1.12f %1.12f %1.12f %1.12f %1.12f\n",t,res1.real(),res1.imag(),res2.real(),res2.imag());
		fclose(out);		
		out=fopen("cmsso.txt","a");
		fprintf(out,"%1.12f %1.12f %1.12f %1.12f\n",fIntXi1_.imag(),t,res1*res2/(fIntXi1_*(fIntXi1_-1.0))*exp(-(fIntXi1_-1.0)*omegaO)*exp(-xi2*omegaI));
		fclose(out);
		// end of debug
		complex<double> res=(*charFctHest)(fIntXi1_,xi2)*exp( gammaLn(-xi2)+gammaLn(fIntXi1_+xi2-1.0)-gammaLn(fIntXi1_-1.0) );
		complex<double> resFull=res/(fIntXi1_*(fIntXi1_-1.0))*exp(-(fIntXi1_-1.0)*omegaO)*exp(-xi2*omegaI);
		FILE* out=fopen("cmsso.txt","a");
		fprintf(out,"%1.12f %1.12f %1.12f %1.12f\n",fIntXi1_.imag(),xi2.imag(),resFull.real(),resFull.imag());
		fclose(out);
		char name[30];
		sprintf(name,"cmssoI_%1.3f.txt",fIntXi1_.imag());
		out=fopen(name,"a");
		fprintf(out,"%1.12f %1.12f %1.12f\n",t,res.real(),res.imag());
		//fprintf(out,"%1.12f %1.12f %1.12f %1.12f %1.12f\n",t,cos(omegaI*t)*res.real(),sin(omegaI*t)*res.imag(),sin(omegaI*t)*res.real(),cos(omegaI*t)*res.imag());
		fclose(out);
		return res.real();
	}
	double cmsFctInnerIm(double t, void* params) { // inner integrand im part
		//complex<double> xi2(-1.0,t); // untransformed
		complex<double> xi2(-1.0,alpha*fIntXi1_.imag()+t); // transformed
		complex<double> res=(*charFctHest)(fIntXi1_,xi2)*exp( gammaLn(-xi2)+gammaLn(fIntXi1_+xi2-1.0)-gammaLn(fIntXi1_-1.0) );
		return res.imag();
	}
	void initGslQuawo(double omegaOuter, double omegaInner, CharFctHeston* cf) { // init gsl qawo enviroment, (omegaOuter=ln alpha, alpha=k/c1), (omegaInner=ln beta, beta=k/c2)
		qawo_table_sine_I = gsl_integration_qawo_table_alloc(omegaInner, INTEGRATIONBOUNDV*2.0, GSL_INTEG_SINE, MAXINTEGRATIONSIZE);
		qawo_table_cosine_I = gsl_integration_qawo_table_alloc(omegaInner, INTEGRATIONBOUNDV*2.0, GSL_INTEG_COSINE, MAXINTEGRATIONSIZE);
		qawo_table_sine_O = gsl_integration_qawo_table_alloc(omegaOuter, INTEGRATIONBOUNDU*2.0, GSL_INTEG_SINE, MAXINTEGRATIONSIZE);
		qawo_table_cosine_O = gsl_integration_qawo_table_alloc(omegaOuter, INTEGRATIONBOUNDU*2.0, GSL_INTEG_COSINE, MAXINTEGRATIONSIZE);
		qawo_ws=gsl_integration_workspace_alloc(MAXINTEGRATIONSIZE);
		charFctHest = cf;
		fIntRe.function = &cmsFctInnerRe;
		fIntRe.params = cf;
		fIntIm.function = &cmsFctInnerIm;
		fIntIm.params = cf;
		fOutRe.function = &cmsFctOuterRe;
		fOutRe.params = cf;
		fOutIm.function = &cmsFctOuterIm;
		fOutIm.params = cf;
		//debug:
		omegaO=omegaOuter;
		omegaI=omegaInner;
	}
	void freeGslQuawo() {
		gsl_integration_workspace_free(qawo_ws);
		gsl_integration_qawo_table_free(qawo_table_sine_I);
		gsl_integration_qawo_table_free(qawo_table_cosine_I);
		gsl_integration_qawo_table_free(qawo_table_sine_O);
		gsl_integration_qawo_table_free(qawo_table_cosine_O);
	}*/
	// end of non zero strike cmsso integration helpers

	EffectiveParametersSVLfm::EffectiveParametersSVLfm(boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<PiecewiseConstantBeta> beta,
				std::vector<Time>& rateTimes,
				std::vector<Rate>& initialRates,
				Real meanLevel, // omega
				Real reversionSpeed, // theta
				Real volVar, // eta
				Real v0,// start value variance process (=z0 in Piterbargs paper)
				std::vector<Size>& fixingIndices, // fixing indices of calibrating swaptions
				std::vector<Size>& swapLengths, // swap lengths of calibrating swaptions
				std::vector<Size>& payFreqs, // payment frequencies of calibrating swaptions
				std::vector<Real>& marketSkews, // market skew parameters
				std::vector<Real>& marketVolas, // market vola parameters --------------------------------------------------
				std::vector<Size>& cmssoFixings, // spread option fixing indices
				std::vector<Size>& cmssoPaymentDelays, // payment delays of spread options
				std::vector<Size>& cmssoLengths1, // swap rate 1 length for cmsso
				std::vector<Size>& cmssoLengths2, // swap rate 2 length for cmsso
				std::vector<Size>& cmssoPayFreqs, // payment frequencies of cmsso underlying swap rates
				std::vector<int>& cmssoFlavors, // flavour of cmsso (+1 call -1 put)
				std::vector<double>& cmssoStrikes, // strike of cmsso
				std::vector<Size>& cmssoGroupStarts, // defines first index for each group of optionlets
				std::vector<double>& cmssoGroupPremia // market premia for each optionlet group
				) :
	sigma_(sigma), beta_(beta),rateTimes_(rateTimes), initialRates_(initialRates),omega_(meanLevel), theta_(reversionSpeed), 
		eta_(volVar), v0_(v0), fixingIndices_(fixingIndices), swapLengths_(swapLengths), payFreqs_(payFreqs), 
		marketSkews_(marketSkews), marketVolas_(marketVolas),
		cmssoFixings_(cmssoFixings), cmssoPaymentDelays_(cmssoPaymentDelays), cmssoLengths1_(cmssoLengths1),
		cmssoLengths2_(cmssoLengths2), cmssoPayFreqs_(cmssoPayFreqs), cmssoFlavors_(cmssoFlavors), cmssoStrikes_(cmssoStrikes), cmssoGroupStarts_(cmssoGroupStarts),
		cmssoGroupPremia_(cmssoGroupPremia),
		numberOfSwaptions_(fixingIndices.size()),
		numberOfCmsso_(cmssoFixings.size()),
		numberOfCmssoGroups_(cmssoGroupStarts.size())
	{

		// compute the approximate autonomous dynamics of the swap rates S_n,m (Piterbarg, Theorem 5.1)
		// Notation: S_n,m : swap rate with fixing at rateTimes[n] and last payment at rateTimes[m]
		//           L_i : Libor forward rate initialRates[i] fixing at rateTimes[i] and maturing at rateTimes[i+1], 
		//           tau_k : rateTimes[k]-rateTimes[k-1]
		
		forwardSwapRates_=std::vector<Real>(numberOfSwaptions_);
		cmssoS1forwardRates_=std::vector<Real>(numberOfCmsso_);
		cmssoS2forwardRates_=std::vector<Real>(numberOfCmsso_);
		cmssoR1forwardRates_=std::vector<Real>(numberOfCmsso_);
		cmssoR2forwardRates_=std::vector<Real>(numberOfCmsso_);
		sigmaNm_=std::vector<boost::shared_ptr<FunctionSigma>>(numberOfSwaptions_);
		cmssoS1Sigma_=std::vector<boost::shared_ptr<FunctionSigma>>(numberOfCmsso_);
		cmssoS2Sigma_=std::vector<boost::shared_ptr<FunctionSigma>>(numberOfCmsso_);
		cmssoR1Sigma_=std::vector<boost::shared_ptr<FunctionSigma>>(numberOfCmsso_);
		cmssoR2Sigma_=std::vector<boost::shared_ptr<FunctionSigma>>(numberOfCmsso_);
		pNm_=std::vector<boost::shared_ptr<FunctionP>>(numberOfSwaptions_);
		cmssoS1pNm_=std::vector<boost::shared_ptr<FunctionP>>(numberOfCmsso_);
		cmssoS2pNm_=std::vector<boost::shared_ptr<FunctionP>>(numberOfCmsso_);
		cmssoR1pNm_=std::vector<boost::shared_ptr<FunctionP>>(numberOfCmsso_);
		cmssoR2pNm_=std::vector<boost::shared_ptr<FunctionP>>(numberOfCmsso_);
		vSsS_=vector<boost::shared_ptr<FunctionVsSs>>(numberOfSwaptions_);
		cmssoS1vSsS_=vector<boost::shared_ptr<FunctionVsSs>>(numberOfCmsso_);
		cmssoS2vSsS_=vector<boost::shared_ptr<FunctionVsSs>>(numberOfCmsso_);
		cmssoR1vSsS_=vector<boost::shared_ptr<FunctionVsSs>>(numberOfCmsso_);
		cmssoR2vSsS_=vector<boost::shared_ptr<FunctionVsSs>>(numberOfCmsso_);
		cmssoCov_=vector<boost::shared_ptr<SigmaCovariance>>(numberOfCmsso_);

		// hermite integration for inner lutz integral (cmsso)
		hermite_ = boost::shared_ptr<GaussHermiteIntegration>(new GaussHermiteIntegration(HERMITEPOINTS));
		hermiteRoots_ = hermite_->x();
		hermiteWeights_ = hermite_->weights();


		// set up integrator for effective skew integral (swaptions)
		legendre_ = boost::shared_ptr<GaussLegendreIntegration>(new GaussLegendreIntegration(SKEWINTEGRALPOINTS));
		legendreRoots_ = legendre_->x();
		legendreWeights_ = legendre_->weights();
		alpha_=vector<vector<vector<Real>>>(numberOfSwaptions_);
		alphaDenom_=vector<Real>(numberOfSwaptions_);
		for(int i=0;i<numberOfSwaptions_;i++) {
			alpha_[i]=vector<vector<Real>>(swapLengths_[i]);
				for(int j=0;j<swapLengths_[i];j++) {
					alpha_[i][j]=vector<Real>(SKEWINTEGRALPOINTS);
				}
		}
		// (cmsso)
		cmssoS1alpha_=vector<vector<vector<Real>>>(numberOfCmsso_);
		cmssoS2alpha_=vector<vector<vector<Real>>>(numberOfCmsso_);
		cmssoR1alpha_=vector<vector<vector<Real>>>(numberOfCmsso_);
		cmssoR2alpha_=vector<vector<vector<Real>>>(numberOfCmsso_);
		cmssoS1alphaDenom_=vector<Real>(numberOfCmsso_);
		cmssoS2alphaDenom_=vector<Real>(numberOfCmsso_);
		cmssoR1alphaDenom_=vector<Real>(numberOfCmsso_);
		cmssoR2alphaDenom_=vector<Real>(numberOfCmsso_);
		for(int i=0;i<numberOfCmsso_;i++) {
			cmssoS1alpha_[i]=vector<vector<Real>>(cmssoLengths1_[i]);
			cmssoS2alpha_[i]=vector<vector<Real>>(cmssoLengths2_[i]);
			cmssoR1alpha_[i]=vector<vector<Real>>(cmssoLengths1_[i]);
			cmssoR2alpha_[i]=vector<vector<Real>>(cmssoLengths2_[i]);
			for(int j=0;j<cmssoLengths1_[i];j++) cmssoS1alpha_[i][j]=vector<Real>(SKEWINTEGRALPOINTS);
			for(int j=0;j<cmssoLengths2_[i];j++) cmssoS2alpha_[i][j]=vector<Real>(SKEWINTEGRALPOINTS);
			for(int j=0;j<cmssoLengths1_[i];j++) cmssoR1alpha_[i][j]=vector<Real>(SKEWINTEGRALPOINTS);
			for(int j=0;j<cmssoLengths2_[i];j++) cmssoR2alpha_[i][j]=vector<Real>(SKEWINTEGRALPOINTS);
		}

		// set up integrator for cmsso analytical approximation (inverse laplace transform)
		//legendre2_ = boost::shared_ptr<GaussLegendreIntegration>(new GaussLegendreIntegration(LAPLACEINTEGRALPOINTS));
		//legendreRoots2_ = legendre2_->roots();
		//legendreWeights2_ = legendre2_->weights();

		// do precomputations for integration of sigma function
		sigma_->precomputeIntegral(theta_);
				
		// precompute partial derivs of Swap rates by libors (swaptions)
		for(int instr=0;instr<numberOfSwaptions_;instr++) {
			Size n = fixingIndices_[instr];
			double T = rateTimes_[n];
			Size m = n + swapLengths_[instr];
			Size payFreq = payFreqs_[instr];
			// compute q_i = L_i (0) / S_n,m (0) x dS_n,m/dL_i (0), i = n,...,m-1, Piterbarg 5.4
			boost::shared_ptr<std::vector<Real>> q = boost::shared_ptr<std::vector<Real>>(new std::vector<Real>(m-n,0.0));
			double u,v;
			for(Size i=n;i<=m-1;i++) {
				double p = 1.0;
				double p2 = 1.0;
				for(Size l=n+1;l<=m;l++) {
					double f=1.0/(1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1]);
					p*=f;
					p2*= l != i+1 ? f : -1.0/((1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1])*
								  (1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1]))*
								  (rateTimes_[l]-rateTimes_[l-1]);
				}
				u = 1.0 - p;
				double uprime = -p2;
				v=0.0;
				double vprime=0.0;
				for(Size k=n+payFreq;k<=m;k+=payFreq) {
					double p=rateTimes_[k]-rateTimes_[k-payFreq];
					double p2=p;
					for(Size l=n+1;l<=k;l+=1) {
						double f=1.0/(1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1]);
						p*=f;
						p2*= l != i+1 ? f : -1.0/((1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1])*
								  (1.0+(rateTimes_[l]-rateTimes_[l-1])*initialRates_[l-1]))*
								  (rateTimes_[l]-rateTimes_[l-1]);
					}
					v+=p;
					vprime+= k >= i+1 ? p2 : 0;
				}
				(*q)[i-n] = (uprime*v-vprime*u) / (v*v)* initialRates_[i] * v/u;
			}
			forwardSwapRates_[instr] = u/v; // forward swap rate
			/*printf("swaption libor derivs: ");
			for(int i=n;i<=m-1;i++) {
				printf("%f;",(*q)[i-n]);
			}
			printf("\n");*/
			// TEST: compute derivs manually
			/*LiborCurve curve(rateTimes_,initialRates_,sigma_,beta_,1.0,0.15,0.01,1.0);
			double h=0.000001;
			for(int i=n;i<=m-1;i++) {
				initialRates_[i]+=h;
				curve.resetPath();
				double sr2=curve.swapRate(n,m-n,payFreq);
				initialRates_[i]-=h;
				curve.resetPath();
				double sr1=curve.swapRate(n,m-n,payFreq);
				printf("%f;",(sr2-sr1)/h*initialRates_[i] / sr1);
			}
			printf("\n");*/

			// compute sigma_i_n,m
			sigmaNm_[instr] = boost::shared_ptr<FunctionSigma>(new FunctionSigma(sigma_,q,n,m));
			// compute p_i_n,m
			pNm_[instr] = boost::shared_ptr<FunctionP>(new FunctionP(sigma_,sigmaNm_[instr])); 
			// compute vSsS
			vSsS_[instr] = boost::shared_ptr<FunctionVsSs>(new FunctionVsSs(sigmaNm_[instr],v0_,eta_,theta_));
		}

		// precompute partial derivs of swap rates and R rates by libors (cmsso)
		for(int instr=0;instr<numberOfCmsso_;instr++) {
			Size n = cmssoFixings_[instr];
			Size delay = cmssoPaymentDelays_[instr];
			double T = rateTimes_[n];
			Size m1 = n + cmssoLengths1_[instr];
			Size m2 = n + cmssoLengths2_[instr];
			boost::shared_ptr<std::vector<Real>> qS1 = boost::shared_ptr<std::vector<Real>>(new std::vector<Real>(m1-n,0.0));
			boost::shared_ptr<std::vector<Real>> qS2 = boost::shared_ptr<std::vector<Real>>(new std::vector<Real>(m2-n,0.0));
			boost::shared_ptr<std::vector<Real>> qR1 = boost::shared_ptr<std::vector<Real>>(new std::vector<Real>(m1-n,0.0));
			boost::shared_ptr<std::vector<Real>> qR2 = boost::shared_ptr<std::vector<Real>>(new std::vector<Real>(m2-n,0.0));
			Size payFreq = cmssoPayFreqs_[instr];
			LiborCurve curve(rateTimes_,initialRates_,sigma_,beta_,omega_,theta_,eta_,v0_);
			double S10=curve.swapRate(n,m1-n,payFreq);
			double S20=curve.swapRate(n,m2-n,payFreq);
			double R10=curve.rRate(0,n,delay,m1-n,payFreq);
			double R20=curve.rRate(0,n,delay,m2-n,payFreq);
			for(int i=0;i<=m1-1;i++) {
				double s1 = curve.swapRate(n,m1-n,payFreq);
				double r1 = curve.rRate(0,n,delay,m1-n,payFreq);
				curve.shift(i,NUMDIFFH,true);
				double s1h = curve.swapRate(n,m1-n,payFreq);
				double r1h = curve.rRate(0,n,delay,m1-n,payFreq);
				curve.shift(i,-NUMDIFFH,true);
				if(i>=n) (*qS1)[i-n] = (s1h-s1)/NUMDIFFH * initialRates_[i]/S10;
				if(i>=n) (*qR1)[i-n] = R10>0.0 ? (r1h-r1)/NUMDIFFH * initialRates_[i]/R10 : 1.0; // R10=0 <=> no measure change
			}
			for(int i=0;i<=m2-1;i++) {
				double s2 = curve.swapRate(n,m2-n,payFreq);
				double r2 = curve.rRate(0,n,delay,m2-n,payFreq);
				curve.shift(i,NUMDIFFH,true);
				double s2h = curve.swapRate(n,m2-n,payFreq);
				double r2h = curve.rRate(0,n,delay,m2-n,payFreq);
				curve.shift(i,-NUMDIFFH,true);
				if(i>=n) (*qS2)[i-n] = (s2h-s2)/NUMDIFFH * initialRates_[i]/S20;
				if(i>=n) (*qR2)[i-n] = R20>0.0 ? (r2h-r2)/NUMDIFFH * initialRates_[i]/R20 : 1.0; // R20=0 <=> no measure change
			}
			// output partial derivatives for debug
			/*printf("cmsos libor derivs: ");
			for(int i=0;i<=m1-1;i++) {
				printf("%f;",(*qR1)[i]);
			}
			printf("\n");*/
			// set forward rates
			cmssoS1forwardRates_[instr]=S10;
			cmssoS2forwardRates_[instr]=S20;
			cmssoR1forwardRates_[instr]=R10;
			cmssoR2forwardRates_[instr]=R20;
			// compute sigmaCovariance
			cmssoCov_[instr] =boost::shared_ptr<SigmaCovariance>(new SigmaCovariance(sigma_,qS1,qS2,n,m1,m2));
			// compute sigma_i_n,m
			cmssoS1Sigma_[instr] = boost::shared_ptr<FunctionSigma>(new FunctionSigma(sigma_,qS1,n,m1));
			cmssoS2Sigma_[instr] = boost::shared_ptr<FunctionSigma>(new FunctionSigma(sigma_,qS2,n,m2));
			cmssoR1Sigma_[instr] = boost::shared_ptr<FunctionSigma>(new FunctionSigma(sigma_,qR1,n,m1));
			cmssoR2Sigma_[instr] = boost::shared_ptr<FunctionSigma>(new FunctionSigma(sigma_,qR2,n,m2));
			// compute p_i_n,m
			cmssoS1pNm_[instr] = boost::shared_ptr<FunctionP>(new FunctionP(sigma_,cmssoS1Sigma_[instr])); 
			cmssoS2pNm_[instr] = boost::shared_ptr<FunctionP>(new FunctionP(sigma_,cmssoS2Sigma_[instr])); 
			cmssoR1pNm_[instr] = boost::shared_ptr<FunctionP>(new FunctionP(sigma_,cmssoR1Sigma_[instr])); 
			cmssoR2pNm_[instr] = boost::shared_ptr<FunctionP>(new FunctionP(sigma_,cmssoR2Sigma_[instr])); 
			// compute vSsS
			cmssoS1vSsS_[instr] = boost::shared_ptr<FunctionVsSs>(new FunctionVsSs(cmssoS1Sigma_[instr],v0_,eta_,theta_));
			cmssoS2vSsS_[instr] = boost::shared_ptr<FunctionVsSs>(new FunctionVsSs(cmssoS2Sigma_[instr],v0_,eta_,theta_));
			cmssoR1vSsS_[instr] = boost::shared_ptr<FunctionVsSs>(new FunctionVsSs(cmssoR1Sigma_[instr],v0_,eta_,theta_));
			cmssoR2vSsS_[instr] = boost::shared_ptr<FunctionVsSs>(new FunctionVsSs(cmssoR2Sigma_[instr],v0_,eta_,theta_));
		}

		// initialize effective skews with market skews (updated with every call of calibrateSkews)
			calibratedEffectiveSkews_ = vector<Real>(numberOfSwaptions_);
			for(int instr=0;instr<numberOfSwaptions_;instr++) {
				calibratedEffectiveSkews_[instr] = marketSkews_[instr];
			}
			calibratedEffectiveVols_ = vector<Real>(numberOfSwaptions_,0.0);
		// precompute skew weights
		precomputeSkewWeights();
	}

	Real EffectiveParametersSVLfm::atm(Size instrument) {
		return forwardSwapRates_[instrument];
	}
	
	Real EffectiveParametersSVLfm::effectiveVolatility(Size instr, Real effectiveBeta) {
		// compute effective volatility lambda
		double T = rateTimes_[fixingIndices_[instr]];
		double zeta = v0_*(sigmaNm_[instr]->squareIntegrate(0.0,T));
		//printf("sqrt(ZETA)=%f\n",sqrt(zeta/T));
		FunctionPhi phi(theta_,eta_,v0_,T,sigmaNm_[instr]);
		double alpha = -gPrimePrimeDivGPrime(zeta,effectiveBeta,forwardSwapRates_[instr]);
		double rhs = phi(alpha);
		Brent brent;
		FunctionPhi0 phi0(theta_,eta_,v0_,T,rhs);
		double lambda=1000.0;
		try { 
			lambda=brent.solve(phi0,LAMBDAACCURACY,0.2*0.2*alpha,0.0,alpha);
			lambda = sqrt(lambda/alpha);
		} catch(QuantLib::Error er) {
			std::cout << "QL threw an exception: " << er.what() << " -- returning eff vol 1000.0 " << std::endl;
		}
		return lambda;
	}

	Real EffectiveParametersSVLfm::effectiveVolatility(Size instr, Size rate, Real effectiveBeta) {
		Size fixingIndex = cmssoFixings_[instr];
		vector<boost::shared_ptr<FunctionSigma>>* sigma = instr==0 ? &cmssoS1Sigma_ : &cmssoS2Sigma_;
		double forward = instr==0 ? cmssoS1forwardRates_[instr] : cmssoS2forwardRates_[instr];
		// compute effective volatility lambda
		double T = rateTimes_[fixingIndex];
		double zeta = v0_*((*sigma)[instr]->squareIntegrate(0.0,T));
		//printf("sqrt(ZETA)=%f\n",sqrt(zeta/T));
		FunctionPhi phi(theta_,eta_,v0_,T,(*sigma)[instr]);
		double alpha = -gPrimePrimeDivGPrime(zeta,effectiveBeta,forward);
		double rhs = phi(alpha);
		Brent brent;
		FunctionPhi0 phi0(theta_,eta_,v0_,T,rhs);
		double lambda=1000.0;
		try { 
			lambda=brent.solve(phi0,LAMBDAACCURACY,0.2*0.2*alpha,0.0,alpha);
			lambda = sqrt(lambda/alpha);
		} catch(QuantLib::Error er) {
			std::cout << "QL threw an exception: " << er.what() << " -- returning eff vol 1000.0 " << std::endl;
		}
		return lambda;
	}

	void EffectiveParametersSVLfm::precomputeSkewWeights() {
		printf("Precompute skew weights...");
		// swaptions
		for(int instr=0;instr<numberOfSwaptions_;instr++) {
			double sum=0.0;
			double T = rateTimes_[fixingIndices_[instr]];
			for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
				double v = (*vSsS_[instr])((legendreRoots_[l]+1.0)*T/2.0);
				sum+=v*legendreWeights_[l];
				for(int i=0;i<swapLengths_[instr];i++) {
					double p = (*pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,fixingIndices_[instr]+i)/((double)swapLengths_[instr]);
					//printf("p(%f,%d)=%f weight=%f\n",(legendreRoots_[l]+1.0)*T/2.0,i,(*pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,i),legendreWeights_[l]);
					alpha_[instr][i][l] = v*p;
				}
			}
			alphaDenom_[instr]=sum*T/2.0;
		}
		// cmsso
		for(int instr=0;instr<numberOfCmsso_;instr++) {
			double sum;
			double T = rateTimes_[cmssoFixings_[instr]];
			sum=0.0;
			for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
				double v = (*cmssoS1vSsS_[instr])((legendreRoots_[l]+1.0)*T/2.0);
				sum+=v*legendreWeights_[l];
				for(int i=0;i<cmssoLengths1_[instr];i++) {
					double p = (*cmssoS1pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,cmssoFixings_[instr]+i)/((double)cmssoLengths1_[instr]);
					cmssoS1alpha_[instr][i][l] = v*p;
				}
			}
			cmssoS1alphaDenom_[instr]=sum*T/2.0;
			sum=0.0;
			for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
				double v = (*cmssoS2vSsS_[instr])((legendreRoots_[l]+1.0)*T/2.0);
				sum+=v*legendreWeights_[l];
				for(int i=0;i<cmssoLengths2_[instr];i++) {
					double p = (*cmssoS2pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,cmssoFixings_[instr]+i)/((double)cmssoLengths2_[instr]);
					cmssoS2alpha_[instr][i][l] = v*p;
				}
			}
			cmssoS2alphaDenom_[instr]=sum*T/2.0;
			sum=0.0;
			for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
				double v = (*cmssoR1vSsS_[instr])((legendreRoots_[l]+1.0)*T/2.0);
				sum+=v*legendreWeights_[l];
				for(int i=0;i<cmssoLengths1_[instr];i++) {
					double p = (*cmssoR1pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,cmssoFixings_[instr]+i)/((double)cmssoLengths1_[instr]);
					cmssoR1alpha_[instr][i][l] = v*p;
				}
			}
			cmssoR1alphaDenom_[instr]=sum*T/2.0;
			sum=0.0;
			for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
				double v = (*cmssoR2vSsS_[instr])((legendreRoots_[l]+1.0)*T/2.0);
				sum+=v*legendreWeights_[l];
				for(int i=0;i<cmssoLengths2_[instr];i++) {
					double p = (*cmssoR2pNm_[instr])((legendreRoots_[l]+1.0)*T/2.0,cmssoFixings_[instr]+i)/((double)cmssoLengths2_[instr]);
					cmssoR2alpha_[instr][i][l] = v*p;
				}
			}
			cmssoR2alphaDenom_[instr]=sum*T/2.0;
		}
		printf("done\n");
	}
	
	
	Real EffectiveParametersSVLfm::effectiveSkew(Size instrument) {
		double sum=0.0;
		double T = rateTimes_[fixingIndices_[instrument]];
		for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
			double sumL=0.0;
			for(int i=0;i<swapLengths_[instrument];i++) {
				sumL+=(*beta_)(legendreRoots_[l],fixingIndices_[instrument]+i)*alpha_[instrument][i][l];
			}
			sum+=sumL*legendreWeights_[l];
		}
		double res=sum*T/(2.0*alphaDenom_[instrument]);
		return res>1.0 ? 1.0 : res;
	}

	Real EffectiveParametersSVLfm::effectiveSkew(Size instrument, Size rate, bool sProcess) {
		double sum=0.0;
		double T = rateTimes_[cmssoFixings_[instrument]];
		Size length;
		std::vector<vector<vector<double>>>* alpha;
		std::vector<double>* alphaDenom;
		if(sProcess) {
			if(rate==0) {alpha = &cmssoS1alpha_; alphaDenom = &cmssoS1alphaDenom_;}
			else {alpha = &cmssoS2alpha_; alphaDenom = &cmssoS2alphaDenom_;}
		} else {
			if(rate==0) {alpha = &cmssoR1alpha_; alphaDenom = &cmssoR1alphaDenom_;}
			else {alpha = &cmssoR2alpha_; alphaDenom = &cmssoR2alphaDenom_;}
		}
		if(rate==0) length=cmssoLengths1_[instrument]; else length=cmssoLengths2_[instrument];
		for(int l=0;l<SKEWINTEGRALPOINTS;l++) {
			double sumL=0.0;
			for(int i=0;i<length;i++) {
				sumL+=(*beta_)(legendreRoots_[l],cmssoFixings_[instrument]+i)*(*alpha)[instrument][i][l]; 
			}
			sum+=sumL*legendreWeights_[l];
		}
		double res=sum*T/(2.0*(*alphaDenom)[instrument]);
		/*printf("computing effective skew instr %d, rate %d, sProcess=%d => result = %f\n",instrument,rate,sProcess,res);
		printf("alternative: %d %d\n",cmssoFixings_[instrument],cmssoFixings_[instrument]+length);
		FunctionBeta srBeta(beta_,pNm_[instrument],cmssoFixings_[instrument],cmssoFixings_[instrument]+length);
		double t=0.0;
		for(int i=0;i<50;i++) {
			printf("%f;%f\n",t,srBeta(t));
			t+=0.1;
		}*/
		return res>1.0 ? 1.0 : res;
	}

	Real EffectiveParametersSVLfm::calibrateVolatility() {
			VolatilityCostFunction costfct(this);
			VolatilityConstraint constraint;

			Array guess = vector2Array(sigma_->parameters());
			Problem prblm(costfct,constraint,guess);
			EndCriteria ec(100000, 500, 1E-5, 1E-5, 1E-5);
			//Simplex method(0.01);
			//LevenbergMarquardt method;
			//SimulatedAnnealing method(0.1,0,15,150,0.5,0.2);
			SimulatedAnnealing method(0.01,0,5,50,0.5,0.2);
			EndCriteria::Type ret=method.minimize(prblm,ec);

			Array x=prblm.currentValue();
			double y=costfct.value(x);//prblm.functionValue(); to make sure, sigma_ is set to optimal value

			if(ret==EndCriteria::None) QL_FAIL("Optimizer returns status none");
			if(ret==EndCriteria::MaxIterations) QL_FAIL("Optimizer returns status max iterations");
			if(ret==EndCriteria::Unknown) QL_FAIL("Optimizer returns status unknown");

			//set calibrated volatilities
			for(int instr=0;instr<numberOfSwaptions_;instr++) {
				calibratedEffectiveVols_[instr]=effectiveVolatility(instr,calibratedEffectiveSkews_[instr]);
			}
			
			return y;
	}

	std::vector<Real> EffectiveParametersSVLfm::recalibrateVolatility(std::vector<Real> parameters) {
		
		printf("recalibrating vola parameters to ");
		for(int i=0;i<parameters.size();i++) printf("%f;",parameters[i]);
		printf("*;\n");
		sigma_->recalibrate(parameters);
		double p=sigma_->penaltyFactor();
		std::vector<Real> result(numberOfSwaptions_+numberOfCmssoGroups_,1000.0);
		if(p>=100.0) return result; // means not admissable values
		
		double total=0.0;
		for(int instr=0;instr<numberOfSwaptions_;instr++) {
			double v = effectiveVolatility(instr,calibratedEffectiveSkews_[instr]);
			double f = (v-marketVolas_[instr])/marketVolas_[instr];
			/*printf("%d;%d;%d;%f;%f;%f;%f;%f;***;",
				instr,fixingIndices_[instr],swapLengths_[instr],
				calibratedEffectiveSkews_[instr],v,marketVolas_[instr],f,p);*/
			result[instr]=f*p;
			total+=f*p*f*p;
		}
		int group=-1;
		double groupPrice=0.0;
		double v=0.0;
		for(int instr=0;instr<numberOfCmsso_+1;instr++) {
			if(instr<numberOfCmsso_) {
				v = spreadOptionPriceT(instr);
				printf("cmsso #%d has price %f",instr,v);
			}
			if(instr==cmssoGroupStarts_[group < numberOfCmssoGroups_-1 ? group+1 : 0] || instr>=numberOfCmsso_) {
				if(instr>0) {
					double f=(groupPrice-cmssoGroupPremia_[group])/cmssoGroupPremia_[group];
					result[numberOfSwaptions_+group]=f*p;
					total+=f*p*f*p;
					printf(" group premia is %f, market %f", groupPrice,cmssoGroupPremia_[group]);
				}
				groupPrice=0.0;
				group++;
			}
			groupPrice+=v;
			printf("\n");
		}
		printf("total rmse=%f\n",sqrt(total/(numberOfSwaptions_+numberOfCmssoGroups_)));
		return result;

	}

	Real EffectiveParametersSVLfm::calibrateSkew() {
		
		precomputeSkewWeights();

		SkewCostFunction costfct(this);
		SkewConstraint constraint;

		Array guess = vector2Array(beta_->parameters());
		Problem prblm(costfct,constraint,guess);
		EndCriteria ec(100000, 500, 1E-5, 1E-5, 1E-5);
		//Simplex method(0.05);
		//LevenbergMarquardt method;
		SimulatedAnnealing method;
		EndCriteria::Type ret=method.minimize(prblm,ec);

		Array x=prblm.currentValue();
		double y=costfct.value(x);//prblm.functionValue(); to make sure, sigma_ is set to optimal value

		//set calibrated skews
		for(int instr=0;instr<numberOfSwaptions_;instr++) {
			calibratedEffectiveSkews_[instr]=effectiveSkew(instr);
		}

		if(ret==EndCriteria::None) QL_FAIL("Optimizer returns status none");
		if(ret==EndCriteria::MaxIterations) QL_FAIL("Optimizer returns status max iterations");
		if(ret==EndCriteria::Unknown) QL_FAIL("Optimizer returns status unknown");
		
		return y;
	}

	std::vector<Real> EffectiveParametersSVLfm::recalibrateSkew(std::vector<Real> parameters) {
		
		printf("recalibrating skew parameters to ");
		for(int i=0;i<parameters.size();i++) printf("%f;",parameters[i]);
		printf("\n");
		beta_->recalibrate(parameters);
		double p=beta_->penaltyFactor();
		std::vector<Real> result(numberOfSwaptions_,1000.0);
		if(p>=100.0) return result; // means not admissable values
		
		for(int instr=0;instr<numberOfSwaptions_;instr++) {
			double v = effectiveSkew(instr);
			double f = (v-marketSkews_[instr])/marketSkews_[instr];
			printf("   instr #%d: eff skew=%f, market=%f, error=%f, penalty=%f\n",instr,v,marketSkews_[instr],f,p);
			result[instr]=f*p;
		}
		return result;
	}

	Real EffectiveParametersSVLfm::impliedBlackVolatility(Size instrument, double strikeOffset) {
		Real result;
		double effSkew = effectiveSkew(instrument);
		double effVol = effectiveVolatility(instrument,effSkew);
		SimpleSVModel model(effVol,effSkew,theta_,eta_,omega_,v0_);
		LiborCurve curve(rateTimes_,initialRates_,sigma_,beta_,omega_,theta_,eta_,v0_);
		double atm=curve.swapRate(fixingIndices_[instrument],swapLengths_[instrument],payFreqs_[instrument]);
		//printf("     implied vol for #%d (eff vol=%f, effSkew=%f, atm=%f, offset=%f)...",instrument,effVol,effSkew,atm,strikeOffset);
		result=model.impliedVola(atm,atm+strikeOffset,rateTimes_[fixingIndices_[instrument]]);
		//printf("result=%f,%f\n",result[0],result[1]);
		return result;
	}

	double EffectiveParametersSVLfm::mcPrice(Size instrument, double strikeOffset) {
		double result;
		double effSkew = effectiveSkew(instrument);
		double effVol = effectiveVolatility(instrument,effSkew);
		SimpleSVModel model(effVol,effSkew,theta_,eta_,omega_,v0_);
		LiborCurve curve(rateTimes_,initialRates_,sigma_,beta_,omega_,theta_,eta_,v0_);
		double atm=curve.swapRate(fixingIndices_[instrument],swapLengths_[instrument],payFreqs_[instrument]);
		//printf("     mc for #%d (eff vol=%f, effSkew=%f, atm=%f, offset=%f)...",instrument,effVol,effSkew,atm,strikeOffset);
		result=model.mcPrice(atm,atm+strikeOffset,rateTimes_[fixingIndices_[instrument]]);
		//printf("result=%f\n",result);
		return result;
	}

	double EffectiveParametersSVLfm::adjustedSwapRate(Size instrument, Size rate) {
		double t = rateTimes_[cmssoFixings_[instrument]];
		boost::shared_ptr<FunctionSigma> S = rate==0 ? cmssoS1Sigma_[instrument] : cmssoS2Sigma_[instrument];
		boost::shared_ptr<FunctionSigma> R = rate==0 ? cmssoR1Sigma_[instrument] : cmssoR2Sigma_[instrument];
		double S0 = rate==0 ? cmssoS1forwardRates_[instrument] : cmssoS2forwardRates_[instrument];
		double R0 = rate==0 ? cmssoR1forwardRates_[instrument] : cmssoR2forwardRates_[instrument];
		double avSkewS = effectiveSkew(instrument,rate,true);
		double avSkewR = effectiveSkew(instrument,rate,false);
		CharFctHeston  cfct(*S,*R,avSkewS,avSkewR,theta_,eta_,t);
		complex<double> cValue = cfct(complex<double>(1.0,0.0),complex<double>(1.0,0.0));
		double adjustment = R0*S0/(avSkewR*avSkewS)*(cValue.real()-1.0);
		//printf(" instrument #%d: S0=%f, R0=%f, avSkewS=%f, avSkewR=%f, adjustment=%f, charFct=%f\n",instrument,S0,R0,avSkewS,avSkewR,adjustment,cValue.real());
		return S0+adjustment;
	}

	double EffectiveParametersSVLfm::spreadOptionPriceT(Size instrument) {
		
		double t = rateTimes_[cmssoFixings_[instrument]];
		//boost::shared_ptr<FunctionSigma> S1 = cmssoS1Sigma_[instrument];
		//boost::shared_ptr<FunctionSigma> S2 = cmssoS2Sigma_[instrument];
		double avSkewS1 = effectiveSkew(instrument,0,true);
		double avSkewS2 = effectiveSkew(instrument,1,true);
		double effVolS1 = effectiveVolatility(instrument,0,avSkewS1);
		double effVolS2 = effectiveVolatility(instrument,1,avSkewS2);
		double adjSR1 = adjustedSwapRate(instrument, 0);
		double adjSR2 = adjustedSwapRate(instrument, 1);
		double sR1=cmssoS1forwardRates_[instrument];
		double sR2=cmssoS2forwardRates_[instrument];
		//double c1 = adjSR1/avSkewS1;
		//double c2 = adjSR2/avSkewS2;
		double k = cmssoStrikes_[instrument]+ sR1*(1.0-avSkewS1)/avSkewS1 - sR2*(1.0-avSkewS2)/avSkewS2;

		double rho = cmssoCov_[instrument]->covariance(0.0,t) /
						sqrt(cmssoS1Sigma_[instrument]->squareIntegrate(0.0,t) * cmssoS2Sigma_[instrument]->squareIntegrate(0.0,t));
		
		//double mu1 = avSkewS1 / sR1 * (adjSR1-sR1) / t; 
		//double mu2 = avSkewS2 / sR1 * (adjSR2-sR2) / t; 
		double mu1 = avSkewS1*log(adjSR1/sR1) / t;
		double mu2 = avSkewS2*log(adjSR2/sR2) / t;

		int w = cmssoFlavors_[instrument];
		double wd = (double)w;

		//cout << "rho=" << rho << " sigma1=" << effVolS1 << " sigma2=" << effVolS2 << " skew1=" << avSkewS1 << " skew2=" << avSkewS2 << endl;
		//cout << "forward1=" << sR1 << " adjRate1=" << adjSR1 << " forward2=" << sR2 << " adjRate2=" << adjSR2 << " Strike K=" << cmssoStrikes_[instrument] << " Transformed Strike k=" << k << endl;
		//cout << "mu1A = " << mu1 << " mu2A = " << mu2 << endl;
		//cout << " c1=" << c1 << " c2=" << c2 << " log(k/c1)=" << log(k/c1) << " log(k/c2)=" << log(k/c2) << endl;
		//QL_REQUIRE(k>=0,"transformed strike k (" << k << ") of cmsso must non negative at the moment .... Original Strike is K=" << cmssoStrikes_[instrument]);

		double cmssoPriceReal = 0.0;


		BranchCutCorrLaplace laplace(theta_,eta_,t);
	
		double skVolS1=effVolS1*avSkewS1;
		double skVolS2=effVolS2*avSkewS2;

		GaussLobattoIntegral gl(1000,INTVACCURACY);
		LutzVIntegrand lvi(&laplace,mu1,mu2,rho,k,sR1,avSkewS1,sR2,avSkewS2,wd,skVolS1,skVolS2,&hermiteRoots_,&hermiteWeights_);
		//test calculation
		//lvi(t);
		cmssoPriceReal = gl(lvi,INTVLOWERBOUND/**t*/,INTVUPPERBOUND*t);

		cmssoPriceReal/=sqrt(M_PI)*M_PI; // ein Pi von der Dichte (14), ein Wurzel(Pi) vom inneren Integral (22)
		//printf("undiscounted result = %f\n",cmssoPriceReal);

		// test calc
		//CmssoFunction cmssofct(sR1,sR2,adjSR1,adjSR2,effVolS1,effVolS2,rho,k,t,1.0,1.0,w);
		//printf("alternative: %f\n",(*hermite_)(cmssofct));
		
		
		/*CharFctHeston cfct(*S1,*S2,avSkewS1,avSkewS2,theta_,eta_,t);

		double L = atan(INTEGRATIONBOUND); // for Bounded atan transformation
		//printf("L=%1.12f, 1/cos^2(L)=%1.12f\n",L,1.0/(cos(L)*cos(L)));

		if( fabs(k) < 0.0001 ) { // zero strike case
			complex<double> sum=0.0;
			FILE *out=fopen("zerocms.csv","w");
			for(int i=0;i<LAPLACEINTEGRALPOINTS;i++) {
				// Linear Transformation
				//complex<double> xi(2.0,INTEGRATIONBOUND*legendreRoots2_[i]);
				//sum+=cfct(xi,1.0-xi)*exp(-xi*log(c2/c1))/(xi*(xi-1.0))*legendreWeights2_[i];
				//complex<double> dgb=cfct(xi,1.0-xi)*exp(-xi*log(c2/c1))/(xi*(xi-1.0));
				// TAN Transformation
				complex<double> xi(2.0,tan(legendreRoots2_[i]*L));
				double trf1=cos(L*legendreRoots2_[i]);
				trf1*=trf1;
				sum+=cfct(xi,1.0-xi)*exp(-xi*log(c2/c1))/(xi*(xi-1.0))*legendreWeights2_[i]/trf1;
				complex<double> dgb=cfct(xi,1.0-xi)*exp(-xi*log(c2/c1))/(xi*(xi-1.0))*legendreWeights2_[i]/trf1;
				fprintf(out,"%.12f %.12f %.12f\n",legendreRoots2_[i],dgb.real(),dgb.imag());
			}
			fclose(out);
			sum*=c2/(2.0*M_PI);
			//sum*=INTEGRATIONBOUND; // Linear Transformation
			sum*=L; // tan Transformation
			cout << "non discounted cmsso prive" << sum << endl;
			cmssoPriceReal = sum.real();
			//double omega = log(c2/c1); cout << "oscillatory parameter is " << omega << endl;
			double t=-10.0;
			for(int i=0;i<40;i++) {
				complex<double> xi(2.0,t);
				complex<double> dgb=cfct(xi,1.0-xi)*exp(-xi*log(c2/c1))/(xi*(xi-1.0));
				complex<double> alt=cfct(xi,1.0-xi)/(xi*(xi-1.0));//*(cos(omega*t)-sin(omega*t)*complex<double>(0.0,1.0)*exp(-2.0*omega));
				printf("%f;%f;%f\n",t,alt.real(),alt.imag());
				t+=0.5;
			}
			//int intSize = 50;
			//gsl_integration_qawo_table* qawo_table_sine = gsl_integration_qawo_table_alloc(omega, INTEGRATIONBOUND*2.0, GSL_INTEG_SINE, intSize);
			//gsl_integration_qawo_table* qawo_table_cosine = gsl_integration_qawo_table_alloc(omega, INTEGRATIONBOUND*2.0, GSL_INTEG_COSINE, intSize);
			//gsl_integration_workspace* qawo_ws = gsl_integration_workspace_alloc(intSize);
			//gsl_function fInt;
			//fInt.function = &cmsFctZero;
			//fInt.params = &cfct;
			gsl_function fSin;
			fSin.function = &cmsFctZeroSin;
			fSin.params = &cfct;
			gsl_function fCos;
			fCos.function = &cmsFctZeroCos;
			fCos.params = &cfct;
			
			//double result=0.0;
			//double abserr=0.0;
			int rc=gsl_integration_qawo (&fSin, -INTEGRATIONBOUND, INTEGRALACCURACY, 0.0, intSize, qawo_ws, qawo_table_sine , &result, &abserr);
			cout << "SIN INTEGRATION return code " << rc << " result " << result << " abserr " << abserr << " size " << qawo_ws->size << endl;
			cmssoPriceReal=result;
			rc=gsl_integration_qawo (&fCos, -INTEGRATIONBOUND, INTEGRALACCURACY, 0.0, intSize, qawo_ws, qawo_table_cosine , &result, &abserr);
			cout << "COSINE INTEGRATION return code " << rc << " result " << result << " abserr " << abserr << " size " << qawo_ws->size << endl;
			cmssoPriceReal+=result;
			
			//cmsFctZeroOmega_=omega;
			//int rc=gsl_integration_qag(&fInt, -INTEGRATIONBOUND, INTEGRATIONBOUND, INTEGRALACCURACY, 0.0, intSize, 1, qawo_ws, &result, &abserr);
			//cout << "QAG INTEGRATION return code " << rc << " result " << result << " abserr " << abserr << " size " << qawo_ws->size << endl;			
			//int rc=gsl_integration_qagi(&fInt,INTEGRALACCURACY,0.0,intSize,qawo_ws,&result,&abserr);
			//cout << "QAGI INTEGRATION return code " << rc << " result " << result << " abserr " << abserr << " size " << qawo_ws->size << endl;			
			//gsl_integration_workspace_free(qawo_ws);
			//cmssoPriceReal=result*c2/(2.0*M_PI);
			//gsl_integration_qawo_table_free(qawo_table_sine);
			//gsl_integration_qawo_table_free(qawo_table_cosine);
			//cmssoPriceReal*=c2*exp(-2.0*omega)/(2.0*M_PI);
			cout << "non discounted cmsso price" << cmssoPriceReal << endl;
		}
		else { // non zero strike case
			CharFctHeston cfct(*S1,*S2,avSkewS1,avSkewS2,theta_,eta_,t);
			initGslQuawo( log(k/c1), log(k/c2),  &cfct );
			//FunctionL lfct(c1,c2,k);
			//char lgtxt[20];
			//complex<double> sum=0.0;
			for(int i=0;i<LAPLACEINTEGRALPOINTS;i++) {
				complex<double> sum2=0.0;
				sprintf(lgtxt,"cmsso_i_%d.txt",i);
				FILE *ff=fopen(lgtxt,"w");
				for(int j=0;j<LAPLACEINTEGRALPOINTS;j++) {
					// Linear Transformation
					complex<double> xi1(3.0,INTEGRATIONBOUND*legendreRoots2_[i]);
					complex<double> xi2(-1.0,INTEGRATIONBOUND*legendreRoots2_[j]);
					complex<double> lf=lfct(xi1,xi2);
					complex<double> cf=cfct(xi1,xi2);
					complex<double> dgb=lf*cf;
					sum2+=lfct(xi1,xi2)*cfct(xi1,xi2)*legendreWeights2_[j];
					fprintf(out,"%.12f %.12f %.12f %.12f\n",legendreRoots2_[i],legendreRoots2_[j],dgb.real(),dgb.imag());
					complex<double> pt1=1.0/(xi1*(xi1-1.0));
					complex<double> pt2=exp(log(k/c1)*(-(xi1-1.0)))*exp(log(k/c2)*(-xi2));
					complex<double> pt3=exp( gammaLn(-xi2)+gammaLn(xi1+xi2-1.0)-gammaLn(xi1-1.0) );
					fprintf(ff,"%.12f %.12f %.12f %.12f\n",legendreRoots2_[j],pt1.real(),pt2.real(),pt3.real());
				}
				fclose(ff);
				fprintf(out2,"%.12f %.12f %.12f\n",legendreRoots2_[i],sum2.real(),sum2.imag());
				sum+=sum2*legendreWeights2_[i];
				fprintf(out,"\n");
			}
			sum*=c1/(4.0*M_PI*M_PI);			
			//sum*=L*L; // Tan Transformation
			sum*=INTEGRATIONBOUND; // Linear Transformation
			
			cmssoPriceReal=cmsFctOuterInt();
			freeGslQuawo();
			cmssoPriceReal*=c1*k/(4.0*M_PI*M_PI*c2); // outer non periodic
			//cmssoPriceReal*=(c1*c1*c1)/(4.0*M_PI*M_PI*c2*k); // outer periodic
			FunctionSpreadInnerI ifct(cfct,lfct);
			ComplexSimpsonIntegral simpson(INTEGRALACCURACY,100);
			complex<double> cmssoPrice = simpson(ifct,-INTEGRATIONBOUND,INTEGRATIONBOUND);
			cout << "k=" << k << " c1=" << c1 << " c2=" << c2 << " price non discounted: " << cmssoPrice << endl;
			cout << "non discounted cmsso price" << cmssoPriceReal << endl;
		}
		*/

		LiborCurve curve(rateTimes_,initialRates_,sigma_,beta_,omega_,theta_,eta_,v0_);
		double df = curve.discountFactor(0,cmssoFixings_[instrument]+cmssoPaymentDelays_[instrument]);
		return df * cmssoPriceReal;

	}

	// helper function: return g''/g' (z), where g is defined as in Piterbarg, 6.17
	Real EffectiveParametersSVLfm::gPrimePrimeDivGPrime(double z, double b, double S0) {
		double phiBSX = 1.0/sqrt(2.0*M_PI)*exp(-0.25*b*b*z);
		double phiPrimeBSX = -(b*sqrt(z)/2)*phiBSX;
		double denom = S0/(2.0*sqrt(z))*phiBSX;
		double nom = S0/(4.0*z)*(-phiBSX/sqrt(z)+phiPrimeBSX*b/2.0);
		return nom/denom;
	}

	/*CmssoFunction::CmssoFunction(const double swapRate1,const double swapRate2,const double adjustedSwapRate1,const double adjustedSwapRate2,
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
		//double h_=k_+sR1_/avSkewS1_*exp((mu1_-0.5*skVolS1_*skVolS1_)*v+skVolS1_*sqrt(v)*u);
		//if(h_<0) h_=1E-6;
		double phi1_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_+(0.5-rho_*rho_)*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
							(vol2_*sqrt(t_*(1.0-rho_*rho_))));
		double phi2_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_-0.5*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
							(vol2_*sqrt(t_*(1.0-rho_*rho_))));
		double f=mult2_*(double)flavor_*swapRate2_*exp(mu2_*t_-0.5*rho_*rho_*vol2_*vol2_*t_+rho_*vol2_*sqrt(t_)*v_)*phi1_-(double)flavor_*h_*phi2_;
		double res=1.0/sqrt(M_PI)*exp(-x*x)*f;
		return res;
	}*/
	


}