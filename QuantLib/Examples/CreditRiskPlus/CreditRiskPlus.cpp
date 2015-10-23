#include <ql/quantlib.hpp>

#include <iostream>

using namespace QuantLib;

int main(int, char* []) {

    std::vector<Real> sector1Exposure(1000,1.0);
    std::vector<Real> sector1Pd(1000,0.04);
    std::vector<Size> sector1Sector(1000,0);

    std::vector<Real> sector2Exposure(1000,2.0);
    std::vector<Real> sector2Pd(1000,0.02);
    std::vector<Size> sector2Sector(1000,1);

    std::vector<Real> exposure;
    exposure.insert(exposure.end(),sector1Exposure.begin(),sector1Exposure.end());
    exposure.insert(exposure.end(),sector2Exposure.begin(),sector2Exposure.end());

    std::vector<Real> pd;
    pd.insert(pd.end(),sector1Pd.begin(),sector1Pd.end());
    pd.insert(pd.end(),sector2Pd.begin(),sector2Pd.end());

    std::vector<Size> sector;
    sector.insert(sector.end(),sector1Sector.begin(),sector1Sector.end());
    sector.insert(sector.end(),sector2Sector.begin(),sector2Sector.end());

    std::vector<Real> relativeDefaultVariance;
    relativeDefaultVariance.push_back(0.75*0.75);
    relativeDefaultVariance.push_back(0.75*0.75);

    Matrix rho(2,2);
    rho[0][0] = rho[1][1] = 1.0;
    rho[0][1] = rho[1][0] = 0.50;

    Real unit = 1.0;

    CreditRiskPlus cr(exposure,pd,sector,relativeDefaultVariance,rho,unit);

    std::cout << "result;sector1;sector2" << cr.sectorExposures().size() << std::endl;
    std::cout << "exposure;" << cr.sectorExposures()[0] << ";"<< cr.sectorExposures()[1] << std::endl;
    std::cout << "EL;" << cr.sectorExpectedLoss()[0] << ";" << cr.sectorExpectedLoss()[1] << std::endl;
    std::cout << "UL;" << cr.sectorUnexpectedLoss()[0] << ";" << cr.sectorUnexpectedLoss()[1] << std::endl;

    std::cout << "overall exposure " << cr.exposure() << std::endl;
    std::cout << "        EL       " << cr.expectedLoss() << std::endl;
    std::cout << "        UL       " << cr.unexpectedLoss() << std::endl;
    std::cout << "        relDefVr " << cr.relativeDefaultVariance() << std::endl;
    std::cout << "        99\%     " << cr.lossQuantile(0.99) << std::endl;

    /* loss */

    std::cout << "losses:" << std::endl;

    for(Size i=0;i<cr.loss().size();i++)
        std::cout << i << ";" << cr.loss()[i] << std::endl;

    /* marginal losses */

    std::cout << "marginal losses:" << std::endl;

    for(Size i=0;i<exposure.size();i++) 
        std::cout << i << ";" << cr.marginalLoss()[i] << std::endl;

    std::cout << "done." << std::endl;


}

