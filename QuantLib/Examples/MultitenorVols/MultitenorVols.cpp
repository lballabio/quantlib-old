#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

int main(int, char* []) {

    Settings::instance().evaluationDate() = Date(15,June,2013);

    std::cout << "Example 1: Flat Smile 6m=>3m" << std::endl;

    Real f=0.0200, g=0.0220;
    Real k=0.0150, l=0.0220;
    Real s=0.20, tau=5.0;
    
    Real alpha=g-f;
    
    Real callg = blackFormula(Option::Call, l, g, s*std::sqrt(tau), 1.0, 0.0);
    Real callf = blackFormula(Option::Call, k, f, s*std::sqrt(tau), 1.0, alpha);

    Real impliedVolF = blackFormulaImpliedStdDev(Option::Call, k, f, callf, 1.0, 0.0) / std::sqrt(tau);

    std::cout << "atm call on g, premium " << callg  << std::endl;
    std::cout << std::setprecision(12) << "atm call on f, premium " << callf  << " implied vol " << impliedVolF << std::endl;
    
    std::ofstream out1("ex1.txt",std::ios::out);

    Real k0=0.0010;
    while (k0<=0.1000001) {
        Real call = blackFormula(Option::Call, k0, f, s*std::sqrt(tau), 1.0, alpha);
        Real implVol = blackFormulaImpliedStdDev(Option::Call, k0, f, call, 1.0, 0.0) / std::sqrt(tau);
        out1 << std::setprecision(2) << k0 << " " << std::setprecision(4) << s << " " << implVol << std::endl;
        k0 += 0.0010;
    }

    out1.close();

    std::cout << "Example 2: Flat Smile 3m=>6m" << std::endl;

    std::ofstream out2("ex2.txt",std::ios::out);

    k0=0.0030;
    while (k0<=0.1000001) {
        Real call = blackFormula(Option::Call, k0-alpha, g-alpha, s*std::sqrt(tau), 1.0, 0.0);
	// negative displacement not allowed
        Real implVol = blackFormulaImpliedStdDev(Option::Call, k0, g, call, 1.0, 0.0) / std::sqrt(tau);
        out2 << std::setprecision(2) << k0 << " " << std::setprecision(4) << s << " " << implVol << std::endl;
        k0 += 0.0010;
    }

    out2.close();

    std::cout << "Example 3: SABR Smile 6m=>3m" << std::endl;

    std::ofstream out3("ex3.txt",std::ios::out);

    k0=0.0050;
    while (k0<=0.1000001) {
	s = sabrVolatility(k0+alpha,g,tau,0.07,0.8,0.6,-0.3);
        Real call = blackFormula(Option::Call, k0, f, s*std::sqrt(tau), 1.0, alpha);
        Real implVol = blackFormulaImpliedStdDev(Option::Call, k0, f, call, 1.0, 0.0) / std::sqrt(tau);
        out3 << std::setprecision(2) << k0 << " " << std::setprecision(4) << s << " " << implVol << std::endl;
        k0 += 0.0010;
    }

    out3.close();

    std::cout << "Example 4: SABR Smile 3m=>6m" << std::endl;

    std::ofstream out4("ex4.txt",std::ios::out);

    k0=0.0070;
    while (k0<=0.1000001) {
	s = sabrVolatility(k0-alpha,f,tau,0.07,0.8,0.6,-0.3);
        Real call = blackFormula(Option::Call, k0-alpha, g-alpha, s*std::sqrt(tau), 1.0, 0.0);
	//negative displacement not allowed
        Real implVol = blackFormulaImpliedStdDev(Option::Call, k0, g, call, 1.0, 0.0) / std::sqrt(tau);
        out4 << std::setprecision(2) << k0 << " " << std::setprecision(4) << s << " " << implVol << std::endl;
        k0 += 0.0010;
    }

    out4.close();

    return(0);

}

