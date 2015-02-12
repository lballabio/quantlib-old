#include <ql/quantlib.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/timer.hpp>

using namespace QuantLib;
using namespace boost::assign;

// helper class for timing

class Timer {
    boost::timer timer_;
    double elapsed_;

  public:
    void start() {
        timer_ = boost::timer();
    }
    void stop() {
        elapsed_ = timer_.elapsed();
    }
    double elapsed() const { return elapsed_ * 1000.0; }
};

int main() {

    try {

    // some typedefs to keep notation simple

    typedef RateHelper_t<CppAD::AD<double> >::Type RateHelperAD;
    typedef DepositRateHelper_t<CppAD::AD<double> > DepositRateHelperAD;
    typedef FraRateHelper_t<CppAD::AD<double> > FraRateHelperAD;
    typedef SwapRateHelper_t<CppAD::AD<double> > SwapRateHelperAD;
    typedef SimpleQuote_t<CppAD::AD<double> > SimpleQuoteAD;
    typedef Quote_t<CppAD::AD<double> > QuoteAD;
    typedef SwapIndex_t<CppAD::AD<double> > SwapIndexAD;
    typedef Euribor_t<CppAD::AD<double> > EuriborAD;
    typedef YieldTermStructure_t<CppAD::AD<double> > YieldTermStructureAD;
    typedef VanillaSwap_t<CppAD::AD<double> > VanillaSwapAD;
    typedef DiscountingSwapEngine_t<CppAD::AD<double> > DiscountingSwapEngineAD;

    // settings

    bool outputStatsOnly = false;
    bool outputDeltas = true;

    Real h = 1.0E-4; // step size for finite differences

    std::vector<Size> portfolioSize;
    std::vector<Size> maximumMaturity;

    portfolioSize += 1, 10, 100, 1000, 5000, 10000;
    maximumMaturity += 10, 20, 30, 50, 70;

    // the reference date

    Date referenceDate(2, January, 2015);
    Settings::instance().evaluationDate() = referenceDate;

    // start the test runs

    Timer timer;

    if (outputStatsOnly)
        std::cout << "portfolioSize;numberofPillars;effectiveNumberNPVCalcs"
                  << std::endl;

    for (Size ii = 0; ii < maximumMaturity.size(); ++ii) {
        for (Size jj = 0; jj < portfolioSize.size(); ++jj) {

            // market quotes

            std::vector<double> x;

            // deposit quotes on, tn, sn, sw, 1m, ... , 6m
            for (Size i = 0; i < 10; ++i)
                x += 0.0010 + i * 0.0002;
            // fra quotes 1-7, ... , 5-11
            for (Size i = 0; i < 5; ++i)
                x += 0.0030 + i * 0.0005;
            // swap quotes 1y, ... , maximum maturity
            for (Size i = 0; i < maximumMaturity[ii]; ++i)
                x += 0.0060 + i * 0.0001;

            // market quotes for AD

            std::vector<CppAD::AD<double> > xAD;

            for (Size i = 0; i < x.size(); ++i)
                xAD += CppAD::AD<double>(x[i]);

            // declare the independent variables

            CppAD::Independent(xAD);

            // build quotes

            std::vector<boost::shared_ptr<SimpleQuote> > quotes;
            std::vector<boost::shared_ptr<SimpleQuoteAD> > quotesAD;
            for (Size i = 0; i < x.size(); ++i) {
                quotes += boost::make_shared<SimpleQuote>(x[i]);
                quotesAD += boost::make_shared<SimpleQuoteAD>(xAD[i]);
            }

            std::vector<boost::shared_ptr<SimpleQuote> > quotes_h;
            for (Size i = 0; i < x.size(); ++i)
                quotes_h += boost::make_shared<SimpleQuote>(x[i] + h);

            std::vector<RelinkableHandle<Quote> > quoteHandles;
            std::vector<RelinkableHandle<QuoteAD> > quoteHandlesAD;
            for (Size i = 0; i < x.size(); ++i) {
                quoteHandles += RelinkableHandle<Quote>(quotes[i]);
                quoteHandlesAD += RelinkableHandle<QuoteAD>(quotesAD[i]);
            }

            // rate helpers

            std::vector<boost::shared_ptr<RateHelper> > instruments;
            std::vector<boost::shared_ptr<RateHelperAD> > instrumentsAD;

            // depos
            for (Size i = 0; i < 10; ++i) {
                Period matTmp;
                Size fixingDays = 2;
                switch (i) {
                case 0:
                    matTmp = 1 * Days;
                    fixingDays = 0;
                    break;
                case 1:
                    matTmp = 1 * Days;
                    fixingDays = 1;
                    break;
                case 2:
                    matTmp = 1 * Days;
                    break;
                case 3:
                    matTmp = 1 * Weeks;
                    break;
                default:
                    matTmp = (i - 3) * Months;
                    break;
                }
                boost::shared_ptr<RateHelper> depoTmp = boost::make_shared<DepositRateHelper>(
                    quoteHandles[i], matTmp, fixingDays, TARGET(),
                    ModifiedFollowing, false, Actual360());
                boost::shared_ptr<RateHelperAD> depoTmpAD = boost::make_shared<DepositRateHelperAD>(
                    quoteHandlesAD[i], matTmp, fixingDays, TARGET(),
                    ModifiedFollowing, false, Actual360());
                instruments += depoTmp;
                instrumentsAD += depoTmpAD;
            }

            // fras

            for (Size i = 0; i < 5; ++i) {
                boost::shared_ptr<RateHelper> fraTmp = boost::make_shared<FraRateHelper>(
                    quoteHandles[10 + i], (i + 1), (i + 7), 2, TARGET(),
                    ModifiedFollowing, false, Actual360());
                boost::shared_ptr<RateHelperAD> fraTmpAD = boost::make_shared<FraRateHelperAD>(
                    quoteHandlesAD[10 + i], (i + 1), (i + 7), 2, TARGET(),
                    ModifiedFollowing, false, Actual360());
                instruments += fraTmp;
                instrumentsAD += fraTmpAD;
            }

            // swaps

            auto euribor6m = boost::make_shared<Euribor>(6 * Months);
            auto euribor6mAD = boost::make_shared<EuriborAD>(6 * Months);

            for (Size i = 0; i < maximumMaturity[ii]; ++i) {
                boost::shared_ptr<RateHelper> swapTmp = boost::make_shared<SwapRateHelper>(
                    quoteHandles[15 + i], (i + 1) * Years, TARGET(), Annual,
                    ModifiedFollowing, Thirty360(), euribor6m);
                boost::shared_ptr<RateHelperAD> swapTmpAD = boost::make_shared<SwapRateHelperAD>(
                    quoteHandlesAD[15 + i], (i + 1) * Years, TARGET(), Annual,
                    ModifiedFollowing, Thirty360(), euribor6mAD);
                instruments += swapTmp;
                instrumentsAD += swapTmpAD;
            }

            // build a piecewise yield curve

            typedef PiecewiseYieldCurve<ZeroYield, Linear, IterativeBootstrap,
                                        double> CurveType;
            typedef PiecewiseYieldCurve<ZeroYield, Linear, IterativeBootstrap,
                                        CppAD::AD<double> > CurveTypeAD;

            boost::shared_ptr<CurveType> curve = boost::make_shared<CurveType>(
                referenceDate, instruments, Actual365Fixed());
            boost::shared_ptr<CurveTypeAD> curveAD = boost::make_shared<CurveTypeAD>(
                referenceDate, instrumentsAD, Actual365Fixed());

            Handle<YieldTermStructure> curveHandle(curve);
            Handle<YieldTermStructureAD> curveHandleAD(curveAD);

            boost::shared_ptr<Euribor> euribor6mYts =
                boost::make_shared<Euribor>(6 * Months, curveHandle);
            boost::shared_ptr<EuriborAD> euribor6mYtsAD =
                boost::make_shared<EuriborAD>(6 * Months, curveHandleAD);

            // set up a vanilla swap portfolio

            euribor6mYts->addFixing(Date(2, October, 2014), 0.0040);
            euribor6mYts->addFixing(Date(3, October, 2014), 0.0040);
            euribor6mYts->addFixing(Date(6, October, 2014), 0.0040);
            euribor6mYtsAD->addFixing(Date(2, October, 2014), 0.0040);
            euribor6mYtsAD->addFixing(Date(3, October, 2014), 0.0040);
            euribor6mYtsAD->addFixing(Date(6, October, 2014), 0.0040);

            boost::shared_ptr<DiscountingSwapEngine> discEngine =
                boost::make_shared<DiscountingSwapEngine>(curveHandle);
            boost::shared_ptr<DiscountingSwapEngineAD> discEngineAD =
                boost::make_shared<DiscountingSwapEngineAD>(curveHandleAD);

            std::vector<boost::shared_ptr<VanillaSwap> > portfolio;
            std::vector<boost::shared_ptr<VanillaSwapAD> > portfolioAD;
            MersenneTwisterUniformRng mt(42);

            for (Size j = 0; j < portfolioSize[jj]; ++j) {
                Real fixedRate = mt.nextReal() * 0.10;
                Date effective(6, October, 2014);
                Date termination = TARGET().advance(
                    effective,
                    static_cast<Size>(mt.nextReal() * maximumMaturity[ii] + 1) *
                        Years);
                Schedule fixedSchedule(effective, termination, 1 * Years,
                                       TARGET(), ModifiedFollowing, Following,
                                       DateGeneration::Backward, false);
                Schedule floatSchedule(effective, termination, 6 * Months,
                                       TARGET(), ModifiedFollowing, Following,
                                       DateGeneration::Backward, false);
                boost::shared_ptr<VanillaSwap> swap = boost::make_shared<VanillaSwap>(
                    VanillaSwap::Payer, 100000000.0 / portfolioSize[jj],
                    fixedSchedule, fixedRate, Thirty360(), floatSchedule,
                    euribor6mYts, 0.0, Actual360());
                boost::shared_ptr<VanillaSwapAD> swapAD = boost::make_shared<VanillaSwapAD>(
                    VanillaSwapAD::Payer, 100000000.0 / portfolioSize[jj],
                    fixedSchedule, fixedRate, Thirty360(), floatSchedule,
                    euribor6mYtsAD, 0.0, Actual360());
                swap->setPricingEngine(discEngine);
                swapAD->setPricingEngine(discEngineAD);
                portfolio.push_back(swap);
                portfolioAD.push_back(swapAD);
            }

            // price the portfolio

            timer.start();
            double y = 0.0;
            for (Size j = 0; j < portfolio.size(); ++j) {
                y += portfolio[j]->NPV();
            }
            timer.stop();
            Real timePricing = timer.elapsed();

            timer.start();
            std::vector<CppAD::AD<double> > yAD(1, 0.0);
            for (Size j = 0; j < portfolio.size(); ++j) {
                yAD[0] = yAD[0] + portfolioAD[j]->NPV();
            }
            timer.stop();
            Real timePricingAD = timer.elapsed();

            // delta vector computation

            timer.start();
            std::vector<double> deltas;
            Real ytemp = y;
            for (Size i = 0; i < x.size(); ++i) {
                quoteHandles[i].linkTo(quotes_h[i]);
                Real yh = 0.0;
                for (Size j = 0; j < portfolio.size(); ++j) {
                    yh += portfolio[j]->NPV();
                }
                deltas.push_back((yh - ytemp) / h);
                ytemp = yh;
            }
            timer.stop();
            Real timeDeltas = timer.elapsed();

            timer.start();
            CppAD::ADFun<Real> f(xAD, yAD);
            std::vector<Real> deltasAD(xAD.size()), w(1, 1.0);
            deltasAD = f.Reverse(1, w);
            timer.stop();
            Real timeDeltasAD = timer.elapsed();

            // output results

            if (outputStatsOnly) {
                std::cout << portfolioSize[jj] << ";" << x.size() << ";"
                          << (timePricingAD + timeDeltasAD) /
                                 (timePricing + timeDeltas) * x.size()
                          << std::endl;
            } else {
                std::cout
                    << "====================================================="
                       "=============================" << std::endl;
                std::cout << "maximum maturity        " << std::setw(5)
                          << maximumMaturity[ii] << " years" << std::endl;
                std::cout << "portfolio size     " << std::setw(10)
                          << portfolioSize[jj] << " swaps" << std::endl;
                std::cout << "delta vector size  " << std::setw(10) << x.size()
                          << " pillars" << std::endl;
                std::cout << std::endl << "timings (ms)            double     "
                                          "AD<double>         factor  eff#NPVs"
                          << std::endl;
                std::cout << "   pricing          " << std::setw(10)
                          << timePricing << std::setw(15) << timePricingAD
                          << std::endl;
                std::cout << "   deltas           " << std::setw(10)
                          << timeDeltas << std::setw(15) << timeDeltasAD
                          << std::endl;
                std::cout << "   total            " << std::setw(10)
                          << timePricing + timeDeltas << std::setw(15)
                          << timePricingAD + timeDeltasAD << std::setw(15)
                          << (timePricing + timeDeltas) /
                                 (timePricingAD + timeDeltasAD) << std::setw(10)
                          << (timePricingAD + timeDeltasAD) /
                                 (timePricing + timeDeltas) * x.size()
                          << std::endl;
                std::cout
                    << std::endl
                    << "results:                double     AD<double>     "
                       "difference" << std::endl;
                std::cout << "   NPV         " << std::fixed
                          << std::setprecision(2) << std::setw(15) << y
                          << std::setw(15) << yAD[0] << std::setw(15)
                          << (y - yAD[0]) << std::endl;
                if (outputDeltas)
                    for (Size i = 0; i < x.size(); ++i) {
                        std::cout << "   Delta #" << std::setw(5) << (i + 1)
                                  << std::setw(15) << deltas[i] / 10000.0
                                  << std::setw(15) << deltasAD[i] / 10000.0
                                  << std::setw(15)
                                  << (deltas[i] - deltasAD[i]) / 10000.0
                                  << std::endl;
                    }
            }

        } // loop portfolio size
    }     // lopp maximum maturity

    } catch(QuantLib::Error e) {
        std::cout << e.what() << std::endl;
    } catch(std::exception e) {
        std::cout << e.what() << std::endl;
    }

    return 0;
}
