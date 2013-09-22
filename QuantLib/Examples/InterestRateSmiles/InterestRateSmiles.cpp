#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

// compare different SABR formulas

void zabrPaper() {

    std::ofstream out;
    out.open("smiles.dat");

    // this is an example from the paper, forward is a guess (since not
    // mentioned there)

    Real forward = 0.0325;
    Real expiryTime = 10.0;

    std::vector<Real> sabrParams, zabrParams;

    sabrParams.push_back(0.0873); // alpha
    sabrParams.push_back(0.7);    // beta
    sabrParams.push_back(0.47);   // nu
    sabrParams.push_back(-0.48);  // rho

    // Black Scholes with 20%
    // sabrParams.push_back(0.20); // alpha
    // Sabrparams.push_back(1.0);    // beta
    // sabrParams.push_back(0.0001);   // nu
    // sabrParams.push_back(0.0);  // rho

    // BS beta = 0.7
    // sabrParams.push_back(0.08); // alpha
    // sabrParams.push_back(0.7);    // beta
    // sabrParams.push_back(0.0001);   // nu
    // sabrParams.push_back(0.0);  // rho

    // BScholes nu = 0.50
    // sabrParams.push_back(0.20); // alpha
    // sabrParams.push_back(1.0);    // beta
    // sabrParams.push_back(0.50);   // nu
    // sabrParams.push_back(0.0);  // rho

    zabrParams.push_back(sabrParams[0]);
    zabrParams.push_back(sabrParams[1]);
    zabrParams.push_back(sabrParams[2]);
    zabrParams.push_back(sabrParams[3]);
    zabrParams.push_back(1.0); // gamma

    std::vector<Real> zabrParamsg0(zabrParams);
    zabrParamsg0[4] = 0.0;
    std::vector<Real> zabrParamsg05(zabrParams);
    zabrParamsg05[4] = 0.5;
    std::vector<Real> zabrParamsg15(zabrParams);
    zabrParamsg15[4] = 1.5;
    std::vector<Real> zabrParamsg17(zabrParams);
    zabrParamsg17[4] = 1.7;

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

    boost::shared_ptr<ZabrSmileSection> zabrnmg0(
        new ZabrSmileSection(expiryTime, forward, zabrParamsg0,
                             ZabrSmileSection::ShortMaturityNormal));
    boost::shared_ptr<ZabrSmileSection> zabrnmg05(
        new ZabrSmileSection(expiryTime, forward, zabrParamsg05,
                             ZabrSmileSection::ShortMaturityNormal));
    boost::shared_ptr<ZabrSmileSection> zabrnmg15(
        new ZabrSmileSection(expiryTime, forward, zabrParamsg15,
                             ZabrSmileSection::ShortMaturityNormal));
    boost::shared_ptr<ZabrSmileSection> zabrnmg17(
        new ZabrSmileSection(expiryTime, forward, zabrParamsg17,
                             ZabrSmileSection::ShortMaturityNormal));

    // boost::shared_ptr<ZabrSmileSection> zabrfd(new ZabrSmileSection(
    //     expiryTime, forward, zabrParams, ZabrSmileSection::FullFd));

    boost::shared_ptr<ZabrModel> zabrmodel = zabrln->model();

    Real strike = 0.0001; // we start at 1bp ...

    while (strike <= 0.50) { // ... and go to 50% ...

        // debug: localvol function for sabr hardcoded
        Real ytmp = strike >= 0.0
                        ? 1.0 / (sabrParams[0] * (1.0 - sabrParams[1])) *
                              (pow(forward, 1.0 - sabrParams[1]) -
                               pow(strike, 1.0 - sabrParams[1]))
                        : 1.0 / (sabrParams[0] * (1.0 - sabrParams[1])) *
                              (pow(-strike, 1.0 - sabrParams[1]) +
                               pow(forward, 1.0 - sabrParams[1]));
        Real Jtmp = sqrt(1.0 + sabrParams[2] * sabrParams[2] * ytmp * ytmp -
                         2.0 * sabrParams[3] * sabrParams[2] * ytmp);
        Real localVol =
            Jtmp * pow(std::fabs(strike), sabrParams[1]) * sabrParams[0];
        // end debug

        out << strike << " "                           // 1  SABR hagan
            << sabr->volatility(strike) << " "         // 2
            << sabr->optionPrice(strike) << " "        // 3
            << sabr->digitalOptionPrice(strike) << " " // 4
            << sabr->density(strike) << " "            // 5
            << zabrln->volatility(strike)
            << " " // 6  ZABR short maturity lognormal
            << zabrln->optionPrice(strike) << " "        // 7
            << zabrln->digitalOptionPrice(strike) << " " // 8
            << zabrln->density(strike) << " "            // 9
            << zabrnm->volatility(strike)
            << " " // 10  ZABR short maturity normal
            << zabrnm->optionPrice(strike) << " "        // 11
            << zabrnm->digitalOptionPrice(strike) << " " // 12
            << zabrnm->density(strike) << " "            // 13
            << zabrmodel->localVolatility(strike)
            << " " // 14  *** Equivalent deterministic volatility
            << localVol
            << " " // 15  *** same as 14, but hardcoded for SABR (debug)
            << zabrlv->volatility(strike) << " "  // 16  ZABR local vol
            << zabrlv->optionPrice(strike) << " " // 17
            << zabrlv->digitalOptionPrice(strike, Option::Call, 1.0, 1E-4)
            << " "                                                // 18
            << zabrlv->density(strike, Option::Call, 1E-4) << " " // 19
            // << zabrfd->volatility(strike) << " "         // 20  ZABR fd
            // << zabrfd->optionPrice(strike) << " "        // 21
            // << zabrfd->digitalOptionPrice(strike) << " " // 22
            // << zabrfd->density(strike) << " "            // 23
            << zabrnmg0->volatility(strike)
            << " " // 20  ZABR short maturity normal gamma=0.0
            << zabrnmg05->volatility(strike)
            << " " // 21  ZABR short maturity normal gamma=0.5
            << zabrnmg15->volatility(strike)
            << " " // 22  ZABR short maturity normal gamma=1.5
            << zabrnmg17->volatility(strike)
            << " " // 23  ZABR short maturity normal gamma=1.7
            << std::endl;

        strike += 0.0001; // ... in steps of 5bp
    }

    out.close();
}

void splineSmiles() {

    std::ofstream out;
    out.open("smiles2.dat");

    // same example as in zabrPaper

    Real forward = 0.0325;
    Real expiryTime = 10.0;

    std::vector<Real> sabrParams;

    sabrParams.push_back(0.0873); // alphacd
    sabrParams.push_back(0.7);    // beta
    sabrParams.push_back(0.47);   // nu
    sabrParams.push_back(-0.48);  // rho

    std::vector<Real> money;
    for(Size i=0;i<4;i++) {
        money.push_back(i/4.0);
    }
    for(Size i=1;i<=60;i++) {
        money.push_back(i*1.0);
    }

    boost::shared_ptr<SabrSmileSection> sabr(
        new SabrSmileSection(expiryTime, forward, sabrParams));

    boost::shared_ptr<SmileSection> flatSmileSection(new FlatSmileSection(expiryTime,0.10,Actual365Fixed(),forward));

    boost::shared_ptr<SplineDensitySmileSection> spline(
        new SplineDensitySmileSection(sabr, forward, 0.0, 1.00, false, money));

    std::cout << "arbitrage free region is " << spline->leftCoreStrike()
              << " ... " << spline->rightCoreStrike() << std::endl;

    Real strike = 0.00001;

    while (strike <= 3.0) {

        Real vol = 0.10;
        Real mu = std::log(forward) + vol*vol*expiryTime/2.0;
        Real referencePrice = blackFormula(Option::Call,strike,forward,vol*sqrt(expiryTime));
        Real referenceDigital = blackFormulaCashItmProbability(Option::Call,strike,forward,vol*sqrt(expiryTime));
        NormalDistribution norm(mu,vol*sqrt(expiryTime));
        Real referenceDensity = norm(std::log(strike))/strike;

        out << strike << " "                                    // 1
            << sabr->volatility(strike) << " "                  // 2
            << sabr->optionPrice(strike) << " "                 // 3
            << sabr->digitalOptionPrice(strike) << " "          // 4
            << sabr->density(strike) << " "                     // 5
            << spline->volatility(strike) << " "                // 6
            << spline->optionPrice(strike) << " "               // 7
            << spline->digitalOptionPrice(strike) << " "        // 8
            << spline->density(strike) << " "                   // 9
            << referencePrice << " " // 5
            << referenceDigital << " " //6
            << referenceDensity << " "  //7 
            << referenceDigital - spline->digitalOptionPrice(strike) << " " // 8
            << std::endl;
        strike += 0.0010;
    }

    out.close();
}

int main(int, char * []) { splineSmiles(); }
