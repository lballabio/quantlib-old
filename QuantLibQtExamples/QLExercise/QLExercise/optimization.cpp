/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

#include "optimization.hpp"
#include "customutilities.hpp"
#include <iostream>

#include <ql/utilities/disposable.hpp>
#include <ql/math/array.hpp>
#include <ql/math/optimization/endcriteria.hpp>
#include <ql/math/optimization/problem.hpp>
#include <ql/math/optimization/constraint.hpp>

#include <ql/math/optimization/conjugategradient.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/math/array.hpp>
#include <ql/math/matrix.hpp>

#include <boost/math/distributions.hpp>

using namespace QuantLib;

class RosenBrockFunction : public CostFunction {
public :
	Real value(const Array& x) const {
		QL_REQUIRE (x.size()==2 ,"Rosenbrock function is 2-dim.");
		Real res = (1-x[0])*(1-x[0]);
		res += 100.0*(x[1]-x[0]*x[0])*(x[1]-x[0]*x[0]);
		return res;
	}
	Disposable<Array> values(const Array& x) const {
		QL_REQUIRE (x.size()==2, "Rosenbrock function is 2-dim.");
		// irrelevant what you write in res for most of the optimizers
		// most of them are using value anyways . try with res [0]=100.0
		Array res(1); res[0] = value(x);
		return res;
	}
};

void OptimizationTest::optimization() {
	BOOST_MESSAGE("Optimization");

	Size maxIterations = 1000;
	Size minStatIterations = 100;
	Real rootEpsilon = 1e-8;
	Real functionEpsilon = 1e-9;
	Real gradientNormEpsilon = 1e-5;
	EndCriteria myEndCrit(maxIterations,
		minStatIterations,
		rootEpsilon,
		functionEpsilon,
		gradientNormEpsilon);

	RosenBrockFunction myFunc;

	NoConstraint constraint;

	Problem myProb1(myFunc, constraint, Array(2 ,0.1));
	Problem myProb2(myFunc, constraint, Array(2 ,0.1));
	//Problem myProb3(myFunc, constraint, Array(2 ,0.1));

	Simplex solver1(0.1);
	ConjugateGradient solver2;
	//LevenbergMarquardt solver3;//(1e-6, 1e-6, 1e-6);

	EndCriteria::Type solvedCrit1 = solver1.minimize(myProb1, myEndCrit);
	EndCriteria::Type solvedCrit2 = solver2.minimize(myProb2, myEndCrit);
	//EndCriteria::Type solvedCrit3 = solver3.minimize(myProb3, myEndCrit);

	std::cout << "Criteria Simplex :" << solvedCrit1 << std::endl;
	std::cout << "Root Simplex :" << myProb1.currentValue() << std::endl;
	std::cout << "Min F Value Simplex :" << myProb1.functionValue() << std::endl;

	std::cout << "Criteria CG:" << solvedCrit2 << std::endl;
	std::cout << "Root CG:" << myProb2.currentValue() << std::endl;
	std::cout << "Min F Value CG :" << myProb2.functionValue() << std::endl;

	//std::cout << "Criteria Leveberg-Marquardt:" << solvedCrit3 << std::endl;
	//std::cout << "Root Leveberg-Marquardt:" << myProb3.currentValue() << std::endl;
	//std::cout << "Min F Value Leveberg-Marquardt :" << myProb3.functionValue() << std::endl;
}




Real blackScholesPrice(const Real& spot, 
	const Real& strike,
	const Rate& rd,
	const Rate& rf,
	const Volatility& vol,
	const Time& tau,
	const Integer& phi){
		boost::math::normal_distribution<> d(0.0,1.0);
		Real dp,dm, fwd, stdDev, res, domDf, forDf;

		domDf=std::exp(-rd*tau); forDf=std::exp(-rf*tau);
		fwd=spot*forDf/domDf;
		stdDev=vol*std::sqrt(tau);

		dp=(std::log(fwd/strike)+0.5*stdDev*stdDev)/stdDev;
		dm=(std::log(fwd/strike)-0.5*stdDev*stdDev)/stdDev;

		res=phi*domDf*(fwd*cdf(d,phi*dp)-strike*cdf(d,phi*dm));
		return res;
}

Real impliedVolProblem(const Real& spot,
	const Rate& strike,
	const Rate& rd,
	const Rate& rf,
	const Volatility& vol,
	const Time& tau,
	const Integer& phi,
	const Real& price){
		return blackScholesPrice(spot,strike, rd,rf,vol,tau, phi) - price;
}

class CallProblemFunction : public CostFunction {
private :
	Real C1_, C2_, C3_, C4_, K1_, K2_, K3_, K4_;
	Rate rd_, rf_;
	Integer phi_;
	Time tau_;
public :
	CallProblemFunction(const Rate& rd, const Rate& rf, const Time& tau, const Integer& phi,
		const Real& K1, const Real& K2, const Real& K3, const Real& K4,
		const Real& C1, const Real& C2, const Real& C3, const Real& C4)
		: rd_(rd), rf_(rf), phi_(phi), tau_(tau),
		C1_(C1), C2_(C2), C3_(C3), C4_(C4),
		K1_(K1), K2_(K2), K3_(K3), K4_(K4){}
	Real value(const Array& x) const {
		Array tmpRes = values(x);
		Real res = tmpRes[0]*tmpRes[0];
		res += tmpRes[1]*tmpRes[1];
		res += tmpRes[2]*tmpRes[2];
		res += tmpRes[3]*tmpRes[3];
		return res;
	}
	Disposable<Array> values(const Array& x) const {
		Array res(4);
		res[0] = blackScholesPrice(x[0], K1_, rd_, rf_, x[1], tau_, phi_) - C1_;
		res[1] = blackScholesPrice(x[0], K2_, rd_, rf_, x[1], tau_, phi_) - C2_;
		res[2] = blackScholesPrice(x[0], K3_, rd_, rf_, x[1], tau_, phi_) - C3_;
		res[3] = blackScholesPrice(x[0], K4_, rd_, rf_, x[1], tau_, phi_) - C4_;
		return res;
	}
};

void OptimizationTest::LM(){

	// parameter vector n = 2 (S0=x[0] and sigma=x[1])
	// dimension of Disposable <Array> values m = 4 (options Array res[4])
	// So the requirement n=<m is satisfied.

	// setup of market parameters
	Real spot=98.51;
	Volatility vol=0.134;
	Real K1=87.0, K2=96.0, K3=103.0, K4=110.0;	
	Rate rd=0.002, rf=0.01;
	Integer phi=1;
	Time tau=0.6;
	// calculate Black Scholes prices
	Real C1=blackScholesPrice(spot,K1,rd,rf,vol,tau,phi);
	Real C2=blackScholesPrice(spot,K2,rd,rf,vol,tau,phi);
	Real C3=blackScholesPrice(spot,K3,rd,rf,vol,tau,phi);
	Real C4=blackScholesPrice(spot,K4,rd,rf,vol,tau,phi);

	CallProblemFunction optFunc(rd, rf, tau, phi,K1, K2, K3, K4, C1, C2, C3, C4);

	Size maxIterations=1000;
	Size minStatIterations=100;
	Real rootEpsilon=1e-5;
	Real functionEpsilon=1e-5;
	Real gradientNormEpsilon=1e-5;

	EndCriteria myEndCrit(maxIterations,minStatIterations, rootEpsilon,
		functionEpsilon, gradientNormEpsilon);

	Array startVal(2);
	startVal[0]=80.0; // S0 guess
	startVal[1]=0.20; // sigma guess
	NoConstraint constraint;
	Problem myProb(optFunc, constraint, startVal);
	LevenbergMarquardt solver;
	EndCriteria::Type solvedCrit = solver.minimize(myProb,myEndCrit);

	std::cout << "Criteria :"<< solvedCrit << std::endl;
	std::cout << "Root(S0, sigma) :" << myProb.currentValue() << std::endl;
	std::cout << "Min Cost Function Value :"	<< myProb.functionValue() << std::endl;
}



class RiskParityCostFunction : public CostFunction {
public :
	RiskParityCostFunction(const Matrix& covar) : covar_(covar){}
	Real value(const Array& x) const { //tmpfun(i,j) = (weights(i)*mrgrisk(i)-weights(j)*mrgrisk(j))^2;
		Real res = 0.0;
		double tmp = 0.0;
		//res = DotProduct(x, covar_*x);
		Real portvar = 0.0;
		portvar = DotProduct(x, covar_*x);
		double sigma = sqrt(portvar);
		for(Size i=0; i<x.size(); ++i){
			tmp += ((covar_*x)[i]/sigma - sigma/x.size())*((covar_*x)[i]/sigma - sigma/x.size());
		}
		/*
		sigma=sqrtm(x'*expcov*x);
		ceq=expcov*x/sigma - sigma./n; %weight times marginal contribution should be equal to the avg risk

		double tmp = 0.0;
		Array mrgrsk = covar_*x;
		for(Size i=0; i<x.size(); ++i){
			for(Size j=0; j<x.size(); ++j){
				tmp = (x[i]*mrgrsk[i] - x[j]*mrgrsk[j]) * (x[i]*mrgrsk[i] - x[j]*mrgrsk[j]);
				res += tmp;
				std::cout << tmp << "\t" << res << std::endl;
			}
		}

		*/
		//std::cout << res << std::endl;
		res = tmp;
		return res;
	}
	Disposable<Array> values(const Array& x) const {
		Array res(4);
		res[0] = x[0];
		res[1] = x[1];
		res[2] = x[2];
		res[3] = x[3];
		return res;
	}
private :
	Matrix covar_;
};

void OptimizationTest::RiskParity(){
	BOOST_MESSAGE("Risk Parity Optimization");

	Size maxIterations = 1000;
	Size minStatIterations = 100;
	Real rootEpsilon = 1e-8;
	Real functionEpsilon = 1e-9;
	Real gradientNormEpsilon = 1e-5;
	EndCriteria myEndCrit(maxIterations,
		minStatIterations,
		rootEpsilon,
		functionEpsilon,
		gradientNormEpsilon);

	Matrix covar(4,4);
	covar[1][1]=0.000149235557601678;	covar[1][2]=5.35878611338219e-5;	covar[1][3]=3.73291159392500e-5;	covar[1][4]=-1.98686524851011e-6;
	covar[2][1]=5.35878611338219e-5;	covar[2][2]=6.25607511588529e-5;	covar[2][3]=1.52999036922032e-5;	covar[2][4]=-2.11523970067296e-6;
	covar[3][1]=3.73291159392500e-5;	covar[3][2]=1.52999036922032e-5;	covar[3][3]=0.000150148806515967;	covar[3][4]=1.72130975908247e-6;
	covar[4][1]=-1.98686524851011e-6;	covar[4][2]=-2.11523970067296e-6;	covar[4][3]=1.72130975908247e-6;	covar[4][4]=5.63959244160529e-7;

	RiskParityCostFunction myFunc(covar);

	//NoConstraint constraint;
	BoundaryConstraint constraint(0.0, 1.0);

	Array startVal(4);
	startVal[0]=0.25;
	startVal[1]=0.25;
	startVal[3]=0.25;
	startVal[4]=0.25;

	//Problem myProb1(myFunc, constraint, startVal);
	Problem myProb2(myFunc, constraint, startVal);

	//Simplex solver1(0.1);
	ConjugateGradient solver2;

	//EndCriteria::Type solvedCrit1 = solver1.minimize(myProb1, myEndCrit);
	EndCriteria::Type solvedCrit2 = solver2.minimize(myProb2, myEndCrit);

	//std::cout << "Criteria Simplex :" << solvedCrit1 << std::endl;
	//std::cout << "Root Simplex :" << myProb1.currentValue() << std::endl;
	//std::cout << "Min F Value Simplex :" << myProb1.functionValue() << std::endl;

	std::cout << "Criteria CG:" << solvedCrit2 << std::endl;
	std::cout << "Root CG:" << myProb2.currentValue() << std::endl;
	std::cout << "Min F Value CG :" << myProb2.functionValue() << std::endl;
}
