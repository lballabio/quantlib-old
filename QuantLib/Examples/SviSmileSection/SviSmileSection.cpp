#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

int main(int, char* []) {

    Settings::instance().evaluationDate() = Date(15,June,2013);

    // example from wilmott forum

    // Real atm = 84.16652402;
    // Real t = 1.1260273973;


    // std::vector<Real> k;

    // k.push_back(68.08812552);
    // k.push_back(75.2825432);
    // k.push_back(81.91471416);
    // k.push_back(90.16442872);
    // k.push_back(98.33704705);
    // k.push_back(105.3328053);
    // k.push_back(112.7347653);

    // std::vector<Real> s;

    // s.push_back(0.4405771181);
    // s.push_back(0.4238596309);
    // s.push_back(0.4094996309);
    // s.push_back(0.3855106625);
    // s.push_back(0.3663049223);
    // s.push_back(0.3598082706);
    // s.push_back(0.3480563746);

    // std::vector<Real> m;
    
    // for(Size i=0;i<k.size();i++) {
	// m.push_back(k[i]/atm);
	// std::cout << k[i] << " " << s[i]/std::sqrt(t) << std::endl;
    // }

    // std::cout << std::endl;

    // boost::shared_ptr<SmileSection> src(new InterpolatedSmileSection<Linear>(t,k,s,atm));

    // boost::shared_ptr<SviSmileSection> svi(new SviSmileSection(src,atm,m));

    // Real k0 = 10.0;
    // while(k0<=230.001) {
	// std::cout << k0 << " " << svi->volatility(k0) << " " << svi->density(k0) << std::endl;
	// k0 += 1.0;
    // }

    // sabr smile fit

    Real atm = 0.03;
    Real alpha = 0.15, beta = 0.90, nu = 0.30, rho = 0.30;
    Real t = 10.0;

    std::vector<Real> k, m, s;
    m.push_back(0.10);
    m.push_back(0.25);
    m.push_back(0.50);
    m.push_back(0.75);
    m.push_back(1.00);
    m.push_back(1.10);
    m.push_back(1.25);
    m.push_back(1.50);
    m.push_back(2.00);

    std::vector<Real> sabr;
    sabr.push_back(alpha);
    sabr.push_back(beta);
    sabr.push_back(nu);
    sabr.push_back(rho);

    boost::shared_ptr<SabrSmileSection> src(new SabrSmileSection(t,atm,sabr));

    std::vector<Real> strikes, inputVols;
    for(Size i=0;i<m.size();++i) {
        strikes.push_back(m[i]*atm);
        inputVols.push_back(src->volatility(strikes.back()));
        std::cout << strikes.back() << " " << inputVols.back() << std::endl;
    }

    SviInterpolation svi(strikes.begin(), strikes.end(), inputVols.begin(),
                         t,atm,Null<Real>(),Null<Real>(),Null<Real>(),Null<Real>(),Null<Real>(),
                         false,false,false,false,false);
    svi.enableExtrapolation(true);
    svi.update();    

    Real k0 = 0.0001;
    while(k0<=0.10) {
        std::cout << k0 << " " << svi(k0) << std::endl;
        k0 += 0.0001;
    }


    return(0);

}

