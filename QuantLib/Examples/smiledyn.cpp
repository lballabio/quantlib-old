#include <ql/quantlib.hpp>

#include <iostream>

using namespace QuantLib;

int main() {

    Real forward = 0.03;

    Real expiryTime = 5.0;

    Real alpha = atof(getenv("ALPHA"));
    Real beta = atof(getenv("BETA"));
    Real nu = atof(getenv("NU"));
    Real rho = atof(getenv("RHO"));

    // generate call prices

    std::vector<Real> strikes;
    std::vector<Real> vols;
    std::vector<Real> prices;

    Real h = 0.0001;

    Real tmp = h;
    while (tmp < 0.12) {
        Real vol =
            sabrVolatility(tmp, forward, expiryTime, alpha, beta, nu, rho);
        Real price = blackFormula(Option::Call, tmp, forward,
                                  vol * std::sqrt(expiryTime), 1.0, 0.0);
        strikes.push_back(tmp);
        vols.push_back(vol);
        prices.push_back(price);
        tmp += h;
    }

    // dynamics

    Real shiftedForward = 0.0350;
    Real shiftedForwardb = 0.0400;

    // 1 sticky strike
    // 2 sticky absolute moneyness
    // 3 sticky bp vol (approx)
    // 4 model
    std::vector<Real> prices1, prices2, prices3, prices4;
    std::vector<Real> prices1b, prices2b, prices3b, prices4b;

    for (Size i = 0; i < strikes.size(); ++i) {
        Real price1 = blackFormula(Option::Call, strikes[i], shiftedForward,
                                   vols[i] * std::sqrt(expiryTime), 1.0, 0.0);
        Real price2 =
            blackFormula(Option::Call, strikes[i], shiftedForward,
                         vols[std::max<Size>(i - 50,0)] * std::sqrt(expiryTime), 1.0, 0.0);
        Real bpVol = vols[std::max<Size>(i-50,0)] * forward;
        Real price3 = blackFormula(
            Option::Call, strikes[i], shiftedForward,
            bpVol / shiftedForward * std::sqrt(expiryTime), 1.0, 0.0);
        Real vol4 = sabrVolatility(strikes[i], shiftedForward, expiryTime,
                                   alpha, beta, nu, rho);
        Real price4 = blackFormula(Option::Call, strikes[i], shiftedForward,
                                   vol4 * std::sqrt(expiryTime), 1.0, 0.0);
        prices1.push_back(price1);
        prices2.push_back(price2);
        prices3.push_back(price3);
        prices4.push_back(price4);

        Real price1b = blackFormula(Option::Call, strikes[i], shiftedForwardb,
                                   vols[i] * std::sqrt(expiryTime), 1.0, 0.0);
        Real price2b =
            blackFormula(Option::Call, strikes[i], shiftedForwardb,
                         vols[std::max<Size>(i - 100,0)] * std::sqrt(expiryTime), 1.0, 0.0);
        Real bpVolb = vols[std::max<Size>(i-100,0)] * forward;
        Real price3b = blackFormula(
            Option::Call, strikes[i], shiftedForwardb,
            bpVolb / shiftedForwardb * std::sqrt(expiryTime), 1.0, 0.0);
        Real vol4b = sabrVolatility(strikes[i], shiftedForwardb, expiryTime,
                                   alpha, beta, nu, rho);
        Real price4b = blackFormula(Option::Call, strikes[i], shiftedForwardb,
                                   vol4b * std::sqrt(expiryTime), 1.0, 0.0);
        prices1b.push_back(price1b);
        prices2b.push_back(price2b);
        prices3b.push_back(price3b);
        prices4b.push_back(price4b);
    }

    // output

    for (Size i = 1; i < strikes.size() -1; ++i) {
        Real dens0 =
            (prices[i + 1] - 2.0 * prices[i] + prices[i - 1]) / (h * h);
        Real dens1 =
            (prices1[i + 1] - 2.0 * prices1[i] + prices1[i - 1]) / (h * h);
        Real dens2 =
            (prices2[i + 1] - 2.0 * prices2[i] + prices2[i - 1]) / (h * h);
        Real dens3 =
            (prices3[i + 1] - 2.0 * prices3[i] + prices3[i - 1]) / (h * h);
        Real dens4 =
            (prices4[i + 1] - 2.0 * prices4[i] + prices4[i - 1]) / (h * h);
        Real dens1b =
            (prices1b[i + 1] - 2.0 * prices1b[i] + prices1b[i - 1]) / (h * h);
        Real dens2b =
            (prices2b[i + 1] - 2.0 * prices2b[i] + prices2b[i - 1]) / (h * h);
        Real dens3b =
            (prices3b[i + 1] - 2.0 * prices3b[i] + prices3b[i - 1]) / (h * h);
        Real dens4b =
            (prices4b[i + 1] - 2.0 * prices4b[i] + prices4b[i - 1]) / (h * h);
        std::cout << strikes[i] << " " << vols[i] << " " << dens0 << " "
                  << dens1 << " " << dens2 << " " << dens3 << " " << dens4 << " "
                  << dens1b << " " << dens2b << " " << dens3b << " " << dens4b
                  << std::endl;
    }

    return 0;
}
