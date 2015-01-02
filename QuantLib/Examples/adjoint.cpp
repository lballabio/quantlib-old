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
    typedef SimpleQuote_t<dbltype> SimpleQuoteAD;
    typedef Quote_t<dbltype> QuoteAD;

    // the reference date

    Date referenceDate(2, January, 2015);
    Settings::instance().evaluationDate() = referenceDate;

    // declare the independent variables (sample deposit quotes)
    std::vector<dbltype> x(3);
    x[0] = 0.0035;
    x[1] = 0.0090;
    x[2] = 0.0121;

#ifdef YES_I_WANT_TO_USE_AD
    CppAD::Independent(x);
#endif

    auto rate1m = boost::make_shared<SimpleQuoteAD>(x[0]);
    auto rate2m = boost::make_shared<SimpleQuoteAD>(x[1]);
    auto rate3m = boost::make_shared<SimpleQuoteAD>(x[2]);

#ifndef YES_I_WANT_TO_USE_AD
    Real h = 1e-4;
    auto rate1mp = boost::make_shared<SimpleQuoteAD>(x[0] + h);
    auto rate2mp = boost::make_shared<SimpleQuoteAD>(x[1] + h);
    auto rate3mp = boost::make_shared<SimpleQuoteAD>(x[2] + h);
#endif

    // build a piecewise curve

    RelinkableHandle<QuoteAD> quote1m(rate1m), quote2m(rate2m), quote3m(rate3m);

    auto dp1m = boost::make_shared<DepositRateHelperAD>(
        quote1m, 1 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());

    auto dp2m = boost::make_shared<DepositRateHelperAD>(
        quote2m, 2 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());

    auto dp3m = boost::make_shared<DepositRateHelperAD>(
        quote3m, 3 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());

    std::vector<boost::shared_ptr<RateHelperAD> > instruments;
    instruments += dp1m, dp2m, dp3m;

    PiecewiseYieldCurve<ZeroYield, Linear, IterativeBootstrap, dbltype> curve(
        referenceDate, instruments, Actual365Fixed());

    std::vector<dbltype> y(1);

    //Real t3 = curve.timeFromReference(dp3m->latestDate());
    Real t3 = 2.5 / 12.0;

    y[0] = curve.zeroRate(t3, Continuous).rate();

    std::cout << std::setprecision(16);
    std::cout << "zero rate = " << y[0] << std::endl;

#ifdef YES_I_WANT_TO_USE_AD
    // define the operation sequence
    CppAD::ADFun<Real> f(x, y);
    std::vector<Real> dw(3), w(1, 1.0);
    // gradient computation
    dw = f.Reverse(1, w);
    std::cout << "gradient = (" << dw[0] << "," << dw[1] << "," << dw[2] << ")"
              << std::endl;
#else
    // finite difference values
    quote1m.linkTo(rate1mp);
    Real y0_1 = curve.zeroRate(t3, Continuous).rate();
    quote1m.linkTo(rate1m);
    quote2m.linkTo(rate2mp);
    Real y0_2 = curve.zeroRate(t3, Continuous).rate();
    quote2m.linkTo(rate2m);
    quote3m.linkTo(rate3mp);
    Real y0_3 = curve.zeroRate(t3, Continuous).rate();
    quote3m.linkTo(rate3m);
    std::cout << "gradient = (" << (y0_1 - y[0]) / h << "," << (y0_2 - y[0]) / h
              << "," << (y0_3 - y[0]) / h << ")" << std::endl;
#endif

    return 0;
}
