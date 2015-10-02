#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/experimental/models/qg1dlinearmodel.hpp>

#include <ql/indexes/swap/euriborswap.hpp>

#include <iostream>

using namespace QuantLib;

int main() {

    Date refDate(17, Sep, 2015);
    Settings::instance().evaluationDate() = refDate;

    Handle<YieldTermStructure> yts(
        boost::make_shared<FlatForward>(0, TARGET(), 0.03, Actual365Fixed()));

    // ===========================
    // model parameters
    // ===========================

    std::vector<Date> stepDates;

    for (Size i = 1; i < 10; ++i) {
        stepDates.push_back(TARGET().advance(refDate, i * Years));
    }

    std::vector<Real> lambda(10, 0.0050);
    std::vector<Real> alpha(10, 1.0);
    std::vector<Real> beta(10, 0.0);
    std::vector<Real> kappa(10, 0.02);

    Qg1dLinearModel model(yts, stepDates, lambda, alpha, beta, kappa);

    // ===========================
    // test lambda, alpha, beta
    // ===========================
    Real t = 0.0;
    while (t < 10.0) {
        // std::cout << t << " " << model.lambda(t) << " " << model.alpha(t) <<
        // " " << model.beta(t) << std::endl;
        t += 0.01;
    }

    // ==============================
    // test kappa, h, G (for mr 0.02)
    // ==============================

    std::cout.precision(16);
    t = 0.0;
    while (t < 10.0) {
        std::cout << t << " " << model.kappa(t) << " " << model.h(t) << " "
                  << std::exp(-0.02 * t) << " " << model.G(0.0, t) << " "
                  << -(exp(-0.02 * t) - 1.0) / 0.02 << std::endl;
        t += 0.01;
    }

    // ===========================
    // test yApprox
    // ===========================

    Real y = model.yApprox(10.0);
    // std::cout << "yApprox(10) = " << y << std::endl;

    // ===========================
    // test swap rate
    // ===========================

    Date startDate = TARGET().advance(refDate, 10 * Years);
    boost::shared_ptr<SwapIndex> index =
        boost::make_shared<EuriborSwapIsdaFixA>(10 * Years);

    Real x = -0.02;
    while (x <= 0.02) {
        // std::cout << x << " "
        //           << model.swapRate(startDate, startDate, index, 10 * Years,
        //           x,
        //                             y)
        //           << " "
        //           << -std::log(
        //                  model.zerobond(TARGET().advance(startDate, 10 *
        //                  Years),
        //                                 startDate, x, y)) /
        //                  10.0
        //           << std::endl;
        x += 0.0001;
    }

    // ================================================
    // test numerical inversion of swap rate (fixed y)
    // ================================================

    std::vector<Real> times, taus;
    Real T0;
    model.timesAndTaus(startDate, index, 10 * Years, T0, times, taus);
    // std::cout << "times and taus result: T0=" << T0 << std::endl;
    for (Size i = 0; i < times.size(); ++i) {
        // std::cout << times[i] << " " << taus[i] << std::endl;
    }

    // std::cout << "x( 0.0104 ) = "
    //           << model.sInvX(T0, T0, times, taus,
    //                          Handle<YieldTermStructure>(), 0.0104)
    //           << std::endl;
    // std::cout << "x( 0.0304 ) = "
    //           << model.sInvX(T0, T0, times, taus,
    //                          Handle<YieldTermStructure>(), 0.0304)
    //           << std::endl;
    // std::cout << "x( 0.0504 ) = "
    //           << model.sInvX(T0, T0, times, taus,
    //                          Handle<YieldTermStructure>(), 0.0504)
    //           << std::endl;

    // ===========================
    // test swap rate derivative
    // ===========================

    // std::cout << "dS/dx ( -0.02 ) = "
    //           << model.dSwapRateDx(startDate, startDate, index, 10 * Years,
    //                                -0.02, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              -0.02 + 0.0001, y) -
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //               -0.02,
    //                              y)) /
    //                  0.0001
    //           << std::endl;
    // std::cout << "dS/dx ( 0.00 ) = "
    //           << model.dSwapRateDx(startDate, startDate, index, 10 * Years,
    //                                0.00, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.0 + 0.0001, y) -
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //               0.0,
    //                              y)) /
    //                  0.0001
    //           << std::endl;
    // std::cout << "dS/dx ( +0.02 ) = "
    //           << model.dSwapRateDx(startDate, startDate, index, 10 * Years,
    //                                0.02, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.02 + 0.0001, y) -
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //               0.02,
    //                              y)) /
    //                  0.0001
    //           << std::endl;

    // =================================
    // test swap rate second derivative
    // =================================

    // std::cout << "d^2S/dx^2 ( -0.02 ) = "
    //           << model.d2SwapRateDx2(startDate, startDate, index, 10 * Years,
    //                                  -0.02, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              -0.02 + 0.0001, y) -
    //               2.0 *
    //                   model.swapRate(startDate, startDate, index, 10 * Years,
    //                                  -0.02, y) +
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //                              -0.02 - 0.0001, y)) /
    //                  1E-8
    //           << std::endl;
    // std::cout << "d^2S/dx^2 ( 0.00 ) = "
    //           << model.d2SwapRateDx2(startDate, startDate, index, 10 * Years,
    //                                  0.00, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.00 + 0.0001, y) -
    //               2.0 *
    //                   model.swapRate(startDate, startDate, index, 10 * Years,
    //                                  0.00, y) +
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.00 - 0.0001, y)) /
    //                  1E-8
    //           << std::endl;
    // std::cout << "d^2S/dx^2 ( +0.02 ) = "
    //           << model.d2SwapRateDx2(startDate, startDate, index, 10 * Years,
    //                                  0.02, y)
    //           << " fd = "
    //           << (model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.02 + 0.0001, y) -
    //               2.0 *
    //                   model.swapRate(startDate, startDate, index, 10 * Years,
    //                                  0.02, y) +
    //               model.swapRate(startDate, startDate, index, 10 * Years,
    //                              0.02 - 0.0001, y)) /
    //                  1E-8
    //           << std::endl;

    // =======================================================
    // test (numerical, approximated) local vol for swap rate
    // =======================================================

    t = 0.0;
    Real s = -0.10;
    for (Size i = 0; i < 10; ++i) {
        t = static_cast<Real>(i) * T0 / 10.0;
        for (Size j = 0; j < 10; ++j) {
            s = static_cast<Real>(j) / 10.0 * 0.20 - 0.10;
            // std::cout << t << " " << s << " "
            //           << model.phi(t, s, T0, times, taus,
            //                        Handle<YieldTermStructure>(), true)
            //           << " "
            //           << model.phi(t, s, T0, times, taus,
            //                        Handle<YieldTermStructure>(), false)
            //           << std::endl;
        }
        // std::cout << std::endl;
    }

    // ====================================================
    // test vectorized approximated local vol for swap rate
    // ====================================================

    t = 0.0;
    std::vector<Real> svec(100);
    for(Size i=0;i<100;++i)
        svec[i] = -0.10 + static_cast<Real>(i)*0.20/100;
    for (Size i = 0; i < 100; ++i) {
        t = static_cast<Real>(i) * T0 / 100.0;
        // std::vector<Real> localVol = model.phi(t,svec,T0,times,taus);
        // for (Size j = 0; j < 100; ++j) {
        //     std::cout << t << " " << svec[j] << " " << localVol[j] << std::endl;
        // }
        // std::cout << std::endl;
    }

    // =======================================================
    // test xi vs numerical inversion
    // =======================================================

    s = -0.10;
    while (s <= 0.10) {
        // std::cout << s << " "
        //           << model.sInvX(T0, T0, times, taus,
        //                          Handle<YieldTermStructure>(), s)
        //           << " "
        //           << model.xi(T0, T0, times, taus, Handle<YieldTermStructure>(),
        //                       s)
        //           << std::endl;
        s += 0.01;
    }

    return 0;
}
