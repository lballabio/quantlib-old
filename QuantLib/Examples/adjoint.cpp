#include <ql/quantlib.hpp>
#include <boost/assign/std/vector.hpp>

#include <ql/qlcppad.hpp>

using namespace QuantLib;
using namespace boost::assign;

int main() {

    typedef double dbltype;
    //   typedef CppAD::AD<double> dbltype;

    typedef RateHelper_t<dbltype>::Type RateHelperAD;
    typedef DepositRateHelper_t<dbltype> DepositRateHelperAD;
    typedef Quote_t<dbltype> QuoteAD;
    typedef SimpleQuote_t<dbltype> SimpleQuoteAD;

    Date referenceDate(2, January, 2015);

    Settings::instance().evaluationDate() = referenceDate;

    // declare the independent variables

    std::vector<dbltype> x(3);
    x[0] = 0.0035;
    x[1] = 0.0090;
    x[2] = 0.0121;
//    CppAD::Independent(x);

    Real h = 1e-4;
    auto rate1m = boost::make_shared<SimpleQuoteAD>(x[0]);
    auto rate2m = boost::make_shared<SimpleQuoteAD>(x[1]);
    auto rate3m = boost::make_shared<SimpleQuoteAD>(x[2]);
    auto rate1mp = boost::make_shared<SimpleQuoteAD>(x[0] + h);
    auto rate2mp = boost::make_shared<SimpleQuoteAD>(x[1] + h);
    auto rate3mp = boost::make_shared<SimpleQuoteAD>(x[2] + h);
    RelinkableHandle<QuoteAD> quote1m(rate1m), quote2m(rate2m), quote3m(rate3m);

    // tape the operation sequence
    auto dp1m = boost::make_shared<DepositRateHelperAD>(
        quote1m, 1 * Months, 2, TARGET(), ModifiedFollowing, false, Actual360());

    auto dp2m = boost::make_shared<DepositRateHelperAD>(
        quote2m, 2 * Months, 2, TARGET(), ModifiedFollowing, false, Actual360());

    auto dp3m = boost::make_shared<DepositRateHelperAD>(
        quote3m, 3 * Months, 2, TARGET(), ModifiedFollowing, false, Actual360());

    std::vector<boost::shared_ptr<RateHelperAD> > instruments;
    instruments += dp1m, dp2m, dp3m;

    PiecewiseYieldCurve<ZeroYield, Linear, IterativeBootstrap, dbltype> curve(
        referenceDate, instruments, Actual365Fixed());

    // dependent variables and taped function

    std::vector<dbltype> y(1);
    //Real t3 = curve.timeFromReference(dp3m->latestDate());
    Real t3 = 1.5 / 12.0;
    y[0] = curve.zeroRate(t3, Continuous).rate();
    //CppAD::ADFun<Real> f(x,y);

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

    // gradient
    std::vector<Real> dw(3), w(1, 1.0);
//    dw = f.Reverse(1,w);

    std::cout << std::setprecision(16);
    std::cout << "zero rate = " << y[0] << std::endl;
    std::cout << "gradient= (" << dw[0] << "," << dw[1] << "," << dw[2] << ")"
              << std::endl;
     std::cout << "gradient'=(" << (y0_1 - y[0]) / h << "," << (y0_2 - y[0]) / h
               << "," << (y0_3 - y[0]) / h << ")" << std::endl;

    return 0;
}
