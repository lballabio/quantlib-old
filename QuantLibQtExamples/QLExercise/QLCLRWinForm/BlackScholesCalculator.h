#ifndef BS_CALC_H
#define BS_CALC_H

#include "stdafx.h"

// Author: Dimitri Reiswich

//#include <ql/pricingengines/blackcalculator.hpp>
#include <ql/quantlib.hpp>
using namespace QuantLib;


class BSCalc
{
public:
	BSCalc();
	~BSCalc();
	void calculate();
	const double getprice() const {return price_;}
	const double getdelta() const {return delta_;}
	const double getgamma() const {return gamma_;}
	const double getvega() const {return vega_;}
	const double gettheta() const {return theta_;}

private:
	double price_;
	double delta_;
	double gamma_;
	double vega_;
	double theta_;
};

BSCalc::BSCalc()
	: price_(0.0), delta_(0.0), gamma_(0.0), vega_(0.0), theta_(0.0)
{
}

BSCalc::~BSCalc(){}

void BSCalc::calculate()
{
	Real S0=100.0, K=105.0;
	Real rd=0.034, rf=0.021, tau=0.5, vol=0.177;
	Real domDisc=std::exp(-rd*tau), forDisc=std::exp(-rf*tau);
	Real stdDev=vol*std::sqrt(tau);

	boost::shared_ptr<PlainVanillaPayoff> vanillaPayoffPut(
		new PlainVanillaPayoff(Option::Put,K));

	boost::shared_ptr<AssetOrNothingPayoff> aonPayoffCall(
		new AssetOrNothingPayoff(Option::Call,K));

	BlackScholesCalculator vanillaPutPricer(vanillaPayoffPut,S0,forDisc,stdDev,domDisc);
	BlackScholesCalculator aonCallPricer(aonPayoffCall,S0,forDisc,stdDev,domDisc);

	price_ = vanillaPutPricer.value();
	delta_ = vanillaPutPricer.delta();
	gamma_ = vanillaPutPricer.gamma();
	vega_  = vanillaPutPricer.vega(tau);
	theta_ = vanillaPutPricer.theta(tau);

	/*
	std::cout << "--------------Vanilla Values-------------" << std::endl;
	std::cout << "Value:" << vanillaPutPricer.value() << std::endl;
	std::cout << "Delta:" << vanillaPutPricer.delta() << std::endl;
	std::cout << "Gamma:" << vanillaPutPricer.gamma() << std::endl;
	std::cout << "Vega:" << vanillaPutPricer.vega(tau) << std::endl;
	std::cout << "Theta:" << vanillaPutPricer.theta(tau) << std::endl;
	std::cout << "Delta Fwd:" << vanillaPutPricer.deltaForward() << std::endl;
	std::cout << "Gamma Fwd:" << vanillaPutPricer.gammaForward() << std::endl;
	std::cout << "-------------- AON Values-------------" << std::endl;
	std::cout << "Value:" << aonCallPricer.value() << std::endl;
	std::cout << "Delta:" << aonCallPricer.delta() << std::endl;
	std::cout << "Gamma:" << aonCallPricer.gamma() << std::endl;
	std::cout << "Vega:" << aonCallPricer.vega(tau) << std::endl;
	std::cout << "Theta:" << aonCallPricer.theta(tau) << std::endl;
	std::cout << "Delta Fwd:" << aonCallPricer.deltaForward() << std::endl;
	std::cout << "Gamma Fwd:" << aonCallPricer.gammaForward() << std::endl;
	*/
}

#endif // BS_CALC_H