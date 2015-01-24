#include <ql/quantlib.hpp>
#include <boost/assign/std/vector.hpp>

using namespace QuantLib;
using namespace boost::assign;

// comment or uncomment this macro
//#define YES_I_WANT_TO_USE_AD

// cppad utilities
#ifdef YES_I_WANT_TO_USE_AD
#include <ql/qlcppad.hpp>
#endif

int main() {

// define the double type to be used

#ifdef YES_I_WANT_TO_USE_AD
    std::cout << "Example with AD enabled" << std::endl;
    typedef CppAD::AD<double> dbltype;
#else
    std::cout << "Example with AD disabled, use finite differences"
              << std::endl;
    typedef double dbltype;
#endif

    // some typedefs to keep notation simple

    typedef RateHelper_t<dbltype>::Type RateHelperAD;

    typedef DepositRateHelper_t<dbltype> DepositRateHelperAD;
    typedef FraRateHelper_t<dbltype> FraRateHelperAD;
    typedef SwapRateHelper_t<dbltype> SwapRateHelperAD;

    typedef SimpleQuote_t<dbltype> SimpleQuoteAD;
    typedef Quote_t<dbltype> QuoteAD;

    typedef SwapIndex_t<dbltype> SwapIndexAD;
    typedef Euribor_t<dbltype> EuriborAD;

    typedef YieldTermStructure_t<dbltype> YieldTermStructureAD;

    typedef VanillaSwap_t<dbltype> VanillaSwapAD;
    typedef DiscountingSwapEngine_t<dbltype> DiscountingSwapEngineAD;

    // the reference date

    Date referenceDate(2, January, 2015);
    Settings::instance().evaluationDate() = referenceDate;

    // declare the independent variables (sample market quotes)
    std::vector<dbltype> x;
    // on, tn, sn, sw, 1m, 2m, 3m, 4m, 5m, 6m
    x += 0.0010, 0.0012, 0.0015, 0.0020, 0.0030, 0.0035, 0.0040, 0.0045, 0.0050,
        0.0055;
    // 1m-7m, 2m-8m, 3m-9m, 4m-10m, 5m-11m
    x += 0.0060, 0.0062, 0.0064, 0.0066, 0.0068;
    // 1y, 2y, 3y, 4y, 5y, 7y, 10y, 20y, 30y
    x += 0.0100, 0.0110, 0.0120, 0.0130, 0.0140, 0.0145, 0.0150, 0.0160, 0.0185, 0.0200, 
        0.0205,0.0210,0.0215,0.0220,0.0225,0.0230, 0.0235, 0.0240, 0.0245,
        0.0250, 
        0.0255,0.0260,0.0265,0.0270,0.0275,0.0280,0.0285,0.0290,0.0295,
        0.0300;

#ifdef YES_I_WANT_TO_USE_AD
    CppAD::Independent(x);
#endif

    std::vector<boost::shared_ptr<SimpleQuoteAD> > quotes;
    for (Size i = 0; i < x.size(); ++i)
        quotes += boost::make_shared<SimpleQuoteAD>(x[i]);

#ifndef YES_I_WANT_TO_USE_AD
    Real h = 1e-6;
    std::vector<boost::shared_ptr<SimpleQuoteAD> > quotes_h;
    for (Size i = 0; i < x.size(); ++i)
        quotes_h += boost::make_shared<SimpleQuoteAD>(x[i] + h);
#endif

    // build a piecewise curve

    std::vector<RelinkableHandle<QuoteAD> > qHandles;    for (Size i = 0; i < x.size(); ++i)
        qHandles += RelinkableHandle<QuoteAD>(quotes[i]);

    auto dpon = boost::make_shared<DepositRateHelperAD>(
        qHandles[0], 1 * Days, 0, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dptn = boost::make_shared<DepositRateHelperAD>(
        qHandles[1], 1 * Days, 1, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dpsn = boost::make_shared<DepositRateHelperAD>(
        qHandles[2], 1 * Days, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dpsw = boost::make_shared<DepositRateHelperAD>(
        qHandles[3], 1 * Weeks, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp1m = boost::make_shared<DepositRateHelperAD>(
        qHandles[4], 1 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp2m = boost::make_shared<DepositRateHelperAD>(
        qHandles[5], 2 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp3m = boost::make_shared<DepositRateHelperAD>(
        qHandles[6], 3 * Months, 0, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp4m = boost::make_shared<DepositRateHelperAD>(
        qHandles[7], 4 * Months, 0, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp5m = boost::make_shared<DepositRateHelperAD>(
        qHandles[8], 5 * Months, 0, TARGET(), ModifiedFollowing, false,
        Actual360());
    auto dp6m = boost::make_shared<DepositRateHelperAD>(
        qHandles[9], 6 * Months, 0, TARGET(), ModifiedFollowing, false,
        Actual360());

    auto fra17 = boost::make_shared<FraRateHelperAD>(
        qHandles[10], 1, 7, 2, TARGET(), ModifiedFollowing, false, Actual360());
    auto fra28 = boost::make_shared<FraRateHelperAD>(
        qHandles[11], 2, 8, 2, TARGET(), ModifiedFollowing, false, Actual360());
    auto fra39 = boost::make_shared<FraRateHelperAD>(
        qHandles[12], 3, 9, 2, TARGET(), ModifiedFollowing, false, Actual360());
    auto fra410 = boost::make_shared<FraRateHelperAD>(
        qHandles[13], 4, 10, 2, TARGET(), ModifiedFollowing, false, Actual360());
    auto fra511 = boost::make_shared<FraRateHelperAD>(
        qHandles[14], 5, 11, 2, TARGET(), ModifiedFollowing, false, Actual360());

    auto euribor6m = boost::make_shared<EuriborAD>(6 * Months);

    auto swap1y = boost::make_shared<SwapRateHelperAD>(
        qHandles[15], 1 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap2y = boost::make_shared<SwapRateHelperAD>(
        qHandles[16], 2 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap3y = boost::make_shared<SwapRateHelperAD>(
        qHandles[17], 3 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap4y = boost::make_shared<SwapRateHelperAD>(
        qHandles[18], 4 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap5y = boost::make_shared<SwapRateHelperAD>(
        qHandles[19], 5 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap6y = boost::make_shared<SwapRateHelperAD>(
        qHandles[20], 6 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
   auto swap7y = boost::make_shared<SwapRateHelperAD>(
        qHandles[21], 7 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
   auto swap8y = boost::make_shared<SwapRateHelperAD>(
        qHandles[22], 8 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
   auto swap9y = boost::make_shared<SwapRateHelperAD>(
        qHandles[23], 9 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap10y = boost::make_shared<SwapRateHelperAD>(
        qHandles[24], 10 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap11y = boost::make_shared<SwapRateHelperAD>(
        qHandles[25], 11 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap12y = boost::make_shared<SwapRateHelperAD>(
        qHandles[26], 12 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap13y = boost::make_shared<SwapRateHelperAD>(
        qHandles[27], 13 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap14y = boost::make_shared<SwapRateHelperAD>(
        qHandles[28], 14 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap15y = boost::make_shared<SwapRateHelperAD>(
        qHandles[29], 15 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap16y = boost::make_shared<SwapRateHelperAD>(
        qHandles[30], 16 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap17y = boost::make_shared<SwapRateHelperAD>(
        qHandles[31], 17 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap18y = boost::make_shared<SwapRateHelperAD>(
        qHandles[32], 18 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap19y = boost::make_shared<SwapRateHelperAD>(
        qHandles[33], 19 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap20y = boost::make_shared<SwapRateHelperAD>(
        qHandles[34], 20 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap21y = boost::make_shared<SwapRateHelperAD>(
        qHandles[35], 21 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap22y = boost::make_shared<SwapRateHelperAD>(
        qHandles[36], 22 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap23y = boost::make_shared<SwapRateHelperAD>(
        qHandles[37], 23 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap24y = boost::make_shared<SwapRateHelperAD>(
        qHandles[38], 24 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap25y = boost::make_shared<SwapRateHelperAD>(
        qHandles[39], 25 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap26y = boost::make_shared<SwapRateHelperAD>(
        qHandles[40], 26 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap27y = boost::make_shared<SwapRateHelperAD>(
        qHandles[41], 27 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap28y = boost::make_shared<SwapRateHelperAD>(
        qHandles[42], 28 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap29y = boost::make_shared<SwapRateHelperAD>(
        qHandles[43], 29 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    auto swap30y = boost::make_shared<SwapRateHelperAD>(
        qHandles[44], 30 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);

    std::vector<boost::shared_ptr<RateHelperAD> > instruments;
    instruments += dpon, dptn, dpsn, dpsw, dp1m, dp2m, dp3m, dp4m, dp5m, dp6m,
        fra17, fra28, fra39, fra410, fra511, swap1y, swap2y, swap3y, swap4y,
        swap5y, swap6y, swap7y, swap8y, swap9y, swap10y, 
        swap11y, swap12y, swap13y, swap14y, swap15y, swap16y, swap17y, swap18y,
        swap19y, swap20y, swap21y, swap22y, swap23y, swap24y, swap25y, swap26y,
        swap27y, swap28y, swap29y, swap30y;

    typedef PiecewiseYieldCurve<ZeroYield, Linear, IterativeBootstrap, dbltype>
        curve_type;

    auto curve = boost::make_shared<curve_type>(referenceDate, instruments,
                                                Actual365Fixed());

    Handle<YieldTermStructureAD> curveHandle(curve);

    auto euribor6m_yts = boost::make_shared<EuriborAD>(6 * Months, curveHandle);

    // set up a vanilla swap

    Real fixedRate = 0.04;
    Date effective(6, October, 2014);
    Date termination = TARGET().advance(effective, 28 * Years);

    Schedule fixedSchedule(effective, termination, 1 * Years, TARGET(),
                           ModifiedFollowing, Following,
                           DateGeneration::Backward, false);
    Schedule floatSchedule(effective, termination, 6 * Months, TARGET(),
                           ModifiedFollowing, Following,
                           DateGeneration::Backward, false);

    VanillaSwapAD swap(VanillaSwapAD::Payer, 100000000.0, fixedSchedule,
                       fixedRate, Thirty360(), floatSchedule, euribor6m_yts,
                       0.0, Actual360());

    // price the swap

    euribor6m_yts->addFixing(Date(2,October,2014),0.0040);

    auto discEngine = boost::make_shared<DiscountingSwapEngineAD>(curveHandle);

    swap.setPricingEngine(discEngine);

    std::vector<dbltype> y(1);

    y[0] = swap.NPV();

    std::cout << std::setprecision(16);
    std::cout << "swap npv = " << y[0] << std::endl;

#ifdef YES_I_WANT_TO_USE_AD
    // define the operation sequence
    CppAD::ADFun<Real> f(x, y);
    std::vector<Real> dw(x.size()), w(1, 1.0);
    // gradient computation
    dw = f.Reverse(1, w);
    std::cout << "deltas (AD):" << std::endl;
    for (Size i = 0; i < x.size(); ++i) {
        std::cout << i << ";" << dw[i]/10000.0 << std::endl;
    }
#else
    Real ytemp = y[0];
    std::cout << "deltas (FD):" << std::endl;
    for (Size i = 0; i < x.size(); ++i) {
        qHandles[i].linkTo(quotes_h[i]);
        Real yh = swap.NPV();
        //qHandles[i].linkTo(quotes[i]);
        std::cout << i << ";" << (yh - ytemp) / 10000.0 / h << std::endl;
        ytemp = yh;
    }
#endif

    return 0;
}
