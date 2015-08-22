#include <ql/quantlib.hpp>
#include <boost/make_shared.hpp>

using namespace QuantLib;

int main() {

    Date refDate(24, August, 2015);

    Settings::instance().evaluationDate() = refDate;

    Handle<YieldTermStructure> yts(
        boost::make_shared<FlatForward>(refDate, 0.02, Actual365Fixed()));

    boost::shared_ptr<IborIndex> euribor6m =
        boost::make_shared<Euribor>(6 * Months, yts);

    Handle<OptionletVolatilityStructure> capletVol(
        boost::make_shared<ConstantOptionletVolatility>(
            refDate, TARGET(), Following, 0.40, Actual365Fixed()));

    Real rho = atof(getenv("RHO"));
    boost::shared_ptr<SimpleQuote> correlationQuote =
        boost::make_shared<SimpleQuote>(rho);
    Handle<Quote> correlation(correlationQuote);

    Date startDate =
        TARGET().advance(TARGET().advance(refDate, 2 * Days), 10 * Years);
    Date endDate = TARGET().advance(startDate, 6 * Months);
    Natural fixingDays = 2;

    boost::shared_ptr<IborCoupon> coupon = boost::make_shared<IborCoupon>(
        endDate, 1.0, startDate, endDate, fixingDays, euribor6m, 1.0, 0.0,
        Date(), Date(), DayCounter(), true);

    boost::shared_ptr<IborCouponPricer> black76Pricer =
        boost::make_shared<BlackIborCouponPricer>(
            capletVol, BlackIborCouponPricer::Black76);
    boost::shared_ptr<IborCouponPricer> bivariateLnPricer =
        boost::make_shared<BlackIborCouponPricer>(
            capletVol, BlackIborCouponPricer::BivariateLognormal, correlation);

    coupon->setPricer(black76Pricer);
    std::clog << "coupon fixing  = " << coupon->fixingDate() << std::endl;
    std::clog << "coupon start   = " << coupon->accrualStartDate() << std::endl;
    std::clog << "coupon end     = " << coupon->accrualEndDate() << std::endl;
    std::clog << "coupon payment = " << coupon->date() << std::endl;

    std::clog << "Black76: fixing = " << coupon->indexFixing()
              << " adjustment = " << coupon->convexityAdjustment() << std::endl;

    Date paymentDate = startDate;

    while (paymentDate <= TARGET().advance(endDate, 6 * Months)) {
        boost::shared_ptr<IborCoupon> coupon = boost::make_shared<IborCoupon>(
            paymentDate, 1.0, startDate, endDate, fixingDays, euribor6m, 1.0,
            0.0, Date(), Date(), DayCounter(), false);
        coupon->setPricer(bivariateLnPricer);
        Real tau = Actual365Fixed().yearFraction(paymentDate, endDate);
        if (tau >= 0.0) {
            correlationQuote->setValue(rho + (1.0 - rho) * exp(-10.0 * tau));
        } else {
            correlationQuote->setValue(rho);
        }
        std::cout << "\"" << paymentDate << "\" "
                  << yts->timeFromReference(paymentDate) << " "
                  << coupon->indexFixing() << " "
                  << coupon->convexityAdjustment() << std::endl;
        paymentDate++;
    }
}
