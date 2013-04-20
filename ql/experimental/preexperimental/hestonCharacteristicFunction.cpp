#include <hestonCharacteristicFunction.hpp>
#include <stdio.h>


namespace QuantLib {


	/*template<class S, class R>*/ complex<double> CharFctHeston/*<S,R>*/::operator()(complex<double> xiS, complex<double> xiR) {
		
		std::vector<complex<double>> yStart(2,complex<double>(0.0,0.0));
		CharFctHestonODEHelper/*<S,R>*/ fct(sigmaS_,sigmaR_,avSkewS_,avSkewR_,xiS,xiR,theta_,eta_,T_);
		ComplexRungeKutta rk(yStart, fct, 0.0, T_, ODEEPS2);
		std::vector<complex<double>> sol=rk.solution();
		return exp(sol[0]+sol[1]);

	}

	// dummy to avoid LNK4221
	/*class DummyFct {
		public:
			double operator()(double t, Size k) { return 0.0; };
			Size numberOfFactors() { return 0; };
	};
	DummyFct f;
	CharFctHeston<DummyFct,DummyFct> dummyObject(f,f,0.0,0.0,0.0,0.0);
	*/


}