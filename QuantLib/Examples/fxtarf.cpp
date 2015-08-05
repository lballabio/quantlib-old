#include <ql/quantlib.hpp>

using namespace QuantLib;

int main() {

    // set the evaluation date

    Settings::instance().evaluationDate() = Date(28, Apr, 2015);

    std::cout << "Evaluation Date " << Settings::instance().evaluationDate()
              << "\n";

    // set the EUR USD exchange rate

    ExchangeRate eurusd(EURCurrency(), USDCurrency(), 1.10);
    ExchangeRateManager::instance().add(eurusd);

    // build an EUR and USD curve

    Handle<YieldTermStructure> eurYts(
        boost::make_shared<FlatForward>(0, TARGET(), 0.01, Actual365Fixed()));
    Handle<YieldTermStructure> usdYts(
        boost::make_shared<FlatForward>(0, TARGET(), 0.02, Actual365Fixed()));

    // build an ECB EUR-USD fx fixing index

    JointCalendar TgtNy((TARGET()), (UnitedStates()));

    boost::shared_ptr<FxIndex> ecbIndex = boost::make_shared<FxIndex>(
        "ECB", 2, EURCurrency(), USDCurrency(), TgtNy, eurYts, usdYts);

    // add historic fixings

    ecbIndex->addFixing(Date(13, Nov, 2014), 1.04);
    ecbIndex->addFixing(Date(11, Dec, 2014), 1.07);
    ecbIndex->addFixing(Date(13, Jan, 2015), 1.05);
    ecbIndex->addFixing(Date(12, Feb, 2015), 1.06);
    ecbIndex->addFixing(Date(12, Mar, 2015), 1.02);
    ecbIndex->addFixing(Date(13, Apr, 2015), 1.08);

    // retrieve historic, today's (not set, so estimated) and future fixings
    // (always estimated)

    std::cout << "past fixing 13-04-2015 = "
              << ecbIndex->fixing(Date(13, Apr, 2015)) << "\n";
    std::cout << "fut  fixing 28-04-2015 = "
              << ecbIndex->fixing(Date(28, Apr, 2015)) << "\n";
    std::cout << "fut  fixing 28-04-2016 = "
              << ecbIndex->fixing(Date(28, Apr, 2016)) << "\n";

    // set up a tarf deal

    Schedule sched(Date(15, Oct, 2014), Date(15, Oct, 2015), 1 * Months, TgtNy,
                   Following, Following, DateGeneration::Forward, false);

    // check fixing dates
    // for (Size i = 1; i < sched.size(); ++i) {
    //     std::cout << "value date " << sched.date(i) << " has fixing date "
    //               << TgtNy.advance(sched.date(i), -2 * Days) << std::endl;
    // }

    boost::shared_ptr<SimpleQuote> acc000 =
        boost::make_shared<SimpleQuote>(0.0);
    boost::shared_ptr<SimpleQuote> acc010 =
        boost::make_shared<SimpleQuote>(0.10);
    boost::shared_ptr<SimpleQuote> acc020 =
        boost::make_shared<SimpleQuote>(0.20);
    boost::shared_ptr<SimpleQuote> acc025 =
        boost::make_shared<SimpleQuote>(0.25);
    boost::shared_ptr<SimpleQuote> acc030 =
        boost::make_shared<SimpleQuote>(0.25);

    RelinkableHandle<Quote> precalculatedAcc; // no pre-calculated accumulator
    // precalculatedAcc.linkTo(acc010); // set pre-calculated accumulator

    FxTarf tarf(sched, ecbIndex, 100000000.0, 1.05, Option::Call, 2.0, 0.25,
                precalculatedAcc);

    std::cout << "accumulated amount = " << tarf.accumulatedAmount()
              << " (settled: " << tarf.accumulatedAmountSettled() << ")\n";

    // Settings::instance().evaluationDate() = Date(12,Dec,2014);
    // std::cout << "accumulated amount = " << tarf.accumulatedAmount()
    //           << " (settled: " << tarf.accumulatedAmountSettled() << ")\n";

    boost::shared_ptr<SimpleQuote> fx1 =
        boost::make_shared<SimpleQuote>(eurusd.rate());
    boost::shared_ptr<SimpleQuote> fx2 =
        boost::make_shared<SimpleQuote>(1.50);
    RelinkableHandle<Quote> fxQuote(fx1);

    Handle<BlackVolTermStructure> volTs(
        boost::make_shared<BlackConstantVol>(0, TARGET(), 0.20,
                                             Actual365Fixed()));
    boost::shared_ptr<GarmanKohlagenProcess> gkProcess =
        boost::make_shared<GarmanKohlagenProcess>(fxQuote, eurYts, usdYts, volTs);

    // full pricing engine and creation of proxy

    boost::shared_ptr<PricingEngine> mcEngine = boost::make_shared<McFxTarfEngine>(gkProcess);
    tarf.setPricingEngine(mcEngine);
    std::cout << "MCEngine NPV = " << tarf.NPV() << std::endl;

    boost::shared_ptr<ProxyInstrument::ProxyDescription> proxy = tarf.proxy();

    // pfe proxy engine
    boost::shared_ptr<PricingEngine> pfePricingEngine = boost::make_shared<ProxyFxTarfEngine>(proxy, fxQuote);
    tarf.setPricingEngine(pfePricingEngine);
    std::cout << "Proxy Engine NPV = " << tarf.NPV() << std::endl;

    // shift spot
    fxQuote.linkTo(fx2);
    std::cout << "Proxy Engine NPV = " << tarf.NPV() << std::endl;

    // shift eval date and set acc level
    Settings::instance().evaluationDate() = Date(28,June,2015);
    precalculatedAcc.linkTo(acc010);
    fxQuote.linkTo(fx1);
    std::cout << "Proxy Engine NPV = " << tarf.NPV() << std::endl;

    // shift eval date and fx quote
    Settings::instance().evaluationDate() = Date(28,July,2015);
    //precalculatedAcc.linkTo(acc030); // expired !
    precalculatedAcc.linkTo(acc020);
    fxQuote.linkTo(fx2);
    std::cout << "Proxy Engine NPV = " << tarf.NPV() << std::endl;
    

    return 0;
}
