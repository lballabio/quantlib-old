#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

// compare different SABR formulas

int main(int, char * []) {

    std::ofstream out;
    out.open("smiles.dat");

    // this is an example from the paper, forward is a guess (since not mentioned there)

    Real forward = 0.0325;
    Real expiryTime = 10.0;

    std::vector<Real> sabrParams, zabrParams;

    sabrParams.push_back(0.0873); // alpha
    sabrParams.push_back(0.7);    // beta
    sabrParams.push_back(0.47);   // nu
    sabrParams.push_back(-0.48);  // rho

    zabrParams.push_back(sabrParams[0]);
    zabrParams.push_back(sabrParams[1]);
    zabrParams.push_back(sabrParams[2]);
    zabrParams.push_back(sabrParams[3]);
    zabrParams.push_back(1.0); // gamma

    boost::shared_ptr<SabrSmileSection> sabr(
        new SabrSmileSection(expiryTime, forward, sabrParams));

    boost::shared_ptr<ZabrSmileSection> zabrln(
        new ZabrSmileSection(expiryTime, forward, zabrParams,
                             ZabrSmileSection::ShortMaturityLognormal));

    boost::shared_ptr<ZabrSmileSection> zabrnm(
        new ZabrSmileSection(expiryTime, forward, zabrParams,
                             ZabrSmileSection::ShortMaturityNormal));

    boost::shared_ptr<ZabrSmileSection> zabrlv(new ZabrSmileSection(
        expiryTime, forward, zabrParams, ZabrSmileSection::LocalVolatility));

    boost::shared_ptr<ZabrModel> zabrmodel = zabrln->model();

    Real strike = -0.20;// 0.0010; // we start at 10bp ...

    while (strike <= 0.20) { // ... and go to 100% ...

        // debug: localvol function for sabr hardcoded
        Real ytmp = strike >= 0.0 ?
            1.0/(sabrParams[0]*(1.0-sabrParams[1])) * (pow(forward,1.0-sabrParams[1])-pow(strike,1.0-sabrParams[1])) :
            1.0/(sabrParams[0]*(1.0-sabrParams[1])) * (
                                                       pow(-strike,1.0-sabrParams[1]) + pow(forward,1.0-sabrParams[1]));
        Real Jtmp = sqrt(1.0+sabrParams[2]*sabrParams[2]*ytmp*ytmp-2.0*sabrParams[3]*sabrParams[2]*ytmp);
        Real localVol = Jtmp * pow(std::fabs(strike),sabrParams[1]) * sabrParams[0];
        // end debug

        out << strike << " "                           // 1  SABR hagan
            << "0.0 " //sabr->volatility(strike) << " "         // 2
            << "0.0 " //sabr->optionPrice(strike) << " "        // 3
            << "0.0 " //sabr->digitalOptionPrice(strike) << " " // 4
            << "0.0 " //sabr->density(strike) << " "            // 5
            << "0.0 " //zabrln->volatility(strike) << " "       // 6  ZABR short maturity lognormal
            << "0.0 " //zabrln->optionPrice(strike) << " "        // 7
            << "0.0 " //zabrln->digitalOptionPrice(strike) << " " // 8
            << "0.0 " //zabrln->density(strike) << " "            // 9
            << "0.0 " //zabrnm->volatility(strike) << " "         // 10  ZABR short maturity normal
            << "0.0 " //zabrnm->optionPrice(strike) << " "        // 11
            << "0.0 " //zabrnm->digitalOptionPrice(strike) << " " // 12
            << "0.0 " //zabrnm->density(strike) << " "            // 13
            << zabrmodel->localVolatility(strike) << " " // 14  *** Equivalent deterministic volatility
            << localVol << " "                           // 15  *** same as 14, but hardcoded for SABR (debug)
            << "0.0 " //zabrlv->volatility(strike) << " "         // 16  ZABR local vol 
            << zabrlv->optionPrice(strike) << " "        // 17
            << zabrlv->digitalOptionPrice(strike) << " " // 18
            << zabrlv->density(strike) << " "            // 19
            << std::endl;

        strike += 0.0010; // ... in steps of 10bp
    }

    out.close();
}
