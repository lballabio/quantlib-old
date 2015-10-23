#include <ql/quantlib.hpp>

#include <iostream>

using namespace QuantLib;


Disposable<std::vector<Real> > impliedStdDevs(const Real atm,
					  const std::vector<Real>& strikes, 
					  const std::vector<Real>& prices) {
    
    std::vector<Real> result;
    
    for(Size i=0;i<prices.size();i++) {
	result.push_back( blackFormulaImpliedStdDev(Option::Call,strikes[i],atm,prices[i],1.0,0.0,0.2,1E-8,1000) );
    }

    return result;

}

int main(int, char* []) {

    Real tol = 1E-8;

    // arbitrage free sample smile data

    Real atm = 0.05;
    Real t = 1.0;

    Real strikes0[] = { 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10 };

    std::vector<Real> strikes(strikes0,strikes0+10);
    std::vector<Real> money;
    std::vector<Real> calls0;

    for(Size i=0;i<strikes.size();i++) {
	money.push_back(strikes[i]/atm);
	calls0.push_back( blackFormula(Option::Call,strikes[i],atm,0.50*std::sqrt(t),1.0,0.0) );
    }

    std::vector<Real> stdDevs0 = impliedStdDevs(atm,strikes,calls0);
    boost::shared_ptr<SmileSection> sec1(new InterpolatedSmileSection<Linear>(t,strikes,stdDevs0,atm));

    std::cout << "testing arbitrage free smile reproduction" << std::endl;

    boost::shared_ptr<KahaleSmileSection> ksec11(new KahaleSmileSection(sec1,atm,false,false,false,money));

    if( std::fabs(ksec11->leftCoreStrike() - 0.01) > tol ) std::cout << "smile11 left af strike is " 
								  << ksec11->leftCoreStrike()
								  << "expected 0.01" << std::endl;

    if( std::fabs(ksec11->rightCoreStrike() - 0.10) > tol ) std::cout << "smile11 right af strike is " 
								  << ksec11->rightCoreStrike() 
								  << "expected 0.10" << std::endl;

    Real k = strikes[0];
    while(k <= strikes.back()+tol) {
	Real pric0 = sec1->optionPrice(k);
	Real pric1 = ksec11->optionPrice(k);
	if( std::fabs(pric0-pric1) > tol ) std::cout << "smile11 is not reprocduced at strike " << k
						     << "input smile call price is  " << pric0
						     << "kahale smile call price is " << pric1 << std::endl;
	k += 0.0001;
    }

    std::cout << "testing interpolation" << std::endl;

    boost::shared_ptr<KahaleSmileSection> ksec12(new KahaleSmileSection(sec1,atm,true,false,false,money));
    
    if( std::fabs(ksec12->leftCoreStrike() - 0.01) > tol ) std::cout << "smile12 left af strike is " 
								  << ksec12->leftCoreStrike() 
								  << " expected 0.01" << std::endl;

    if( std::fabs(ksec12->rightCoreStrike() - 0.10) > tol ) std::cout << "smile12 right af strike is " 
								  << ksec12->rightCoreStrike()
								  << " expected 0.10" << std::endl;

    for(Size i=0;i<strikes.size();i++) {
	Real pric0 = sec1->optionPrice(strikes[i]);
	Real pric1 = ksec12->optionPrice(strikes[i]);
	if( std::fabs(pric0-pric1) > tol ) std::cout << "smile12 is not reproduced on grid at strike " << strikes[i]
					             << "input smile call price is " << pric0
						     << "kahale smile call price is " << pric1 << std::endl;
    }

    std::cout << "testing global no arbitrageability" << std::endl;
    
    k = 0.0010;
    Real dig00 = 1.0, dig10 = 1.0;
    while(k <= 2.0*strikes.back()+tol) {
	Real dig0 = ksec11->digitalOptionPrice(k);
	Real dig1 = ksec12->digitalOptionPrice(k);
	if( ! ( dig0 <= dig00+tol && dig0 >= 0.0 ) ) std::cout << "arbitrage in digitals11 (" 
									  << dig00 << "," << dig0 << ") at strike " 
									  << k << std::endl; 
	if( ! ( dig1 <= dig10+tol && dig1 >= 0.0 ) ) std::cout << std::setprecision(20) << "arbitrage in digitals12 (" 
									  << dig10 << "," << dig1 << ") at strike " 
									  << k << std::endl;
	dig00 = dig0; dig10 = dig1;
	k += 0.0001;
    }

    std::cout << "testing exponential extrapolation" << std::endl;

    boost::shared_ptr<KahaleSmileSection> ksec13(new KahaleSmileSection(sec1,atm,false,true,false,money));
    
    k=strikes.back();
    Real dig0 = ksec13->digitalOptionPrice(k-0.0010);
    while(k <= 10.0*strikes.back()+tol) {
	Real dig = ksec13->digitalOptionPrice(k);
	if( ! ( dig <= dig0+tol && dig >= 0.0 ) ) std::cout << "arbitrage in digitals13 (" 
									  << dig0 << "," << dig << ") at strike " 
									  << k << std::endl; 

	k += 0.0001;
    }

    std::cout << "testing arbitrageable smile (leftmost point)" << std::endl;

    std::vector<Real> calls1(calls0);
    calls1[0] = (atm-strikes[0])+0.0010; // introduce arbitrage in leftmost strike
    std::vector<Real> stdDevs1 = impliedStdDevs(atm,strikes,calls1);
    boost::shared_ptr<SmileSection> sec2(new InterpolatedSmileSection<Linear>(t,strikes,stdDevs1,atm));

    boost::shared_ptr<KahaleSmileSection> ksec21(new KahaleSmileSection(sec2,atm,false,false,false,money));
    boost::shared_ptr<KahaleSmileSection> ksec22(new KahaleSmileSection(sec2,atm,true,false,true,money));
    
    if( std::fabs(ksec21->leftCoreStrike() - 0.02) > tol ) std::cout << "smile21 left af strike is " 
								  << ksec21->leftCoreStrike()
								  << " expected 0.02" << std::endl;
    if( std::fabs(ksec22->leftCoreStrike() - 0.02) > tol ) std::cout << "smile22 left af strike is " 
								  << ksec22->leftCoreStrike() 
								  << " expected 0.02" << std::endl;

    if( std::fabs(ksec21->rightCoreStrike() - 0.10) > tol ) std::cout << "smile21 right af strike is " 
								  << ksec21->rightCoreStrike() 
								  << " expected 0.10" << std::endl;
    if( std::fabs(ksec22->rightCoreStrike() - 0.10) > tol ) std::cout << "smile22 right af strike is " 
								  << ksec22->rightCoreStrike() 
								  << " expected 0.10" << std::endl;

    k = 0.0010;
    dig00 = dig10 = 1.0;
    while(k <= 2.0*strikes.back()+tol) {
	Real dig0 = ksec21->digitalOptionPrice(k);
	Real dig1 = ksec22->digitalOptionPrice(k);
	if( ! ( dig0 <= dig00+tol && dig0 >= 0.0 ) ) std::cout << "arbitrage in digitals21 (" 
									  << dig00 << "," << dig0 << ") at strike " 
									  << k << std::endl; 
	if( ! ( dig1 <= dig10+tol && dig1 >= 0.0 ) ) std::cout << "arbitrage in digitals22 (" 
									  << dig10 << "," << dig1 << ") at strike " 
									  << k << std::endl; 
	dig00 = dig0; dig10= dig1;
	k += 0.0001;
    }
    
    std::cout << "testing arbitrageable smile (second but rightmost point)" << std::endl;

    std::vector<Real> calls2(calls0);
    calls2[8] = 0.9*calls2[9]+0.1*calls2[8];
    std::vector<Real> stdDevs2 = impliedStdDevs(atm,strikes,calls2);
    boost::shared_ptr<SmileSection> sec3(new InterpolatedSmileSection<Linear>(t,strikes,stdDevs2,atm));

    boost::shared_ptr<KahaleSmileSection> ksec31(new KahaleSmileSection(sec3,atm,false,false,false,money));
    boost::shared_ptr<KahaleSmileSection> ksec32(new KahaleSmileSection(sec3,atm,true,false,true,money));
    
    if( std::fabs(ksec31->leftCoreStrike() - 0.01) > tol ) std::cout << "smile31 left af strike is " 
								  << ksec31->leftCoreStrike() 
								  << " expected 0.01" << std::endl;
    if( std::fabs(ksec32->leftCoreStrike() - 0.01) > tol ) std::cout << "smile32 left af strike is " 
								  << ksec32->leftCoreStrike() 
								  << " expected 0.01" << std::endl;

    if( std::fabs(ksec31->rightCoreStrike() - 0.08) > tol ) std::cout << "smile31 right af strike is " 
								  << ksec31->rightCoreStrike()
								  << " expected 0.08" << std::endl;
    if( std::fabs(ksec32->rightCoreStrike() - 0.10) > tol ) std::cout << "smile32 right af strike is " 
								  << ksec32->rightCoreStrike()
								  << " expected 0.10" << std::endl;

    k = 0.0010;
    dig00 = dig10 = 1.0;
    while(k <= 2.0*strikes.back()+tol) {
	Real dig0 = ksec31->digitalOptionPrice(k);
	Real dig1 = ksec32->digitalOptionPrice(k);
	if( ! ( dig0 <= dig00+tol && dig0 >= 0.0 ) ) std::cout << "arbitrage in digitals31 (" 
									  << dig00 << "," << dig0 << ") at strike " 
									  << k << std::endl; 
	if( ! ( dig1 <= dig10+tol && dig1 >= 0.0 ) ) std::cout << "arbitrage in digitals32 (" 
									  << dig10 << "," << dig1 << ") at strike " 
									  << k << std::endl; 
	dig00 = dig0; dig10 =dig1;
	k += 0.0001;
    }

    return(0);

}

