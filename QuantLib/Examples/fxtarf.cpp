#include <ql/quantlib.hpp>

#include <boost/assign/std/vector.hpp>

using namespace boost::assign;

using namespace QuantLib;

int main() {

    // set the evaluation date

    Date refDate(13, Oct, 2014);
    Settings::instance().evaluationDate() = refDate;

    std::cerr << "Evaluation Date " << Settings::instance().evaluationDate()
              << "\n";

    // set the EUR USD exchange rate

    Real exRate = atof(getenv("SPOT"));
    ExchangeRate eurusd(EURCurrency(), USDCurrency(), exRate);
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

    // ecbIndex->addFixing(Date(13, Nov, 2014), 1.04);
    // ecbIndex->addFixing(Date(11, Dec, 2014), 1.07);
    // ecbIndex->addFixing(Date(13, Jan, 2015), 1.05);
    // ecbIndex->addFixing(Date(12, Feb, 2015), 1.06);
    // ecbIndex->addFixing(Date(12, Mar, 2015), 1.02);
    // ecbIndex->addFixing(Date(13, Apr, 2015), 1.08);

    // set up a tarf deal

    Schedule sched(Date(15, Oct, 2014), Date(15, Oct, 2015), 1 * Months, TgtNy,
                   Following, Following, DateGeneration::Forward, false);

    std::cerr << "tarf schedule:" << std::endl;
    for (Size i = 1; i < sched.size(); ++i) {
        Date fixing = ecbIndex->fixingDate(sched[i]);
        std::cerr << fixing << ";" << sched[i] << ";"
                  << fixing.serialNumber() - refDate.serialNumber()
                  << std::endl;
    }

    double target = atof(getenv("TARGET"));
    int samples = atoi(getenv("SAMPLES"));
    int samples2 = atoi(getenv("SAMPLES2"));
    int steps = atoi(getenv("STEPS"));
    int seed = atoi(getenv("SEED"));
    char *type = getenv("TYPE");
    double shortLeverage = atof(getenv("LEV"));
    FxTarf::CouponType cpType;
    switch (type[0]) {
    case 'f':
        cpType = FxTarf::full;
        break;
    case 'c':
        cpType = FxTarf::capped;
        break;
    case 'n':
        cpType = FxTarf::none;
        break;
    default:
        QL_FAIL("unknown cl option " << type[0]);
        break;
    }

    double initialAm = atof(getenv("ACC"));
    boost::shared_ptr<SimpleQuote> initialAmQ =
        boost::make_shared<SimpleQuote>(initialAm);
    Handle<Quote> initialAcc(initialAmQ);

    FxTarf tarf(sched, ecbIndex, 100000000.0,
                boost::make_shared<PlainVanillaPayoff>(Option::Put, 1.21),
                boost::make_shared<PlainVanillaPayoff>(Option::Call, 1.21),
                target, cpType, shortLeverage, 1.0, initialAcc);

    boost::shared_ptr<SimpleQuote> fx1 =
        boost::make_shared<SimpleQuote>(eurusd.rate());
    RelinkableHandle<Quote> fxQuote(fx1);

    Handle<BlackVolTermStructure> volTs(boost::make_shared<BlackConstantVol>(
        0, TARGET(), 0.20, Actual365Fixed()));
    boost::shared_ptr<GarmanKohlagenProcess> gkProcess =
        boost::make_shared<GarmanKohlagenProcess>(fxQuote, eurYts, usdYts,
                                                  volTs);
    // boost::shared_ptr<ExtendedBlackScholesMertonProcess> gkProcess =
    //     boost::make_shared<ExtendedBlackScholesMertonProcess>(fxQuote,
    //     eurYts, usdYts,
    //                                               volTs);

    // test path generation
    // std::vector<Real> required;
    // required += 0.0, 0.05, 0.1, 0.15, 0.25, 0.47, 0.68, 0.91;
    // TimeGrid gridTmp(required.begin(),required.end(),1);
    // Real result = 0.0;
    // size_t N = 1000000;
    // PseudoRandom::rsg_type rsg =
    // PseudoRandom::make_sequence_generator(gridTmp.size(),42);
    // for(Size i=0;i<N;++i) {
    //     Real x = fxQuote->value();
    //     PseudoRandom::rsg_type::sample_type seq=rsg.nextSequence();
    //     for(Size j=1;j<gridTmp.size();++j) {
    //         x=gkProcess->evolve(gridTmp[j-1],x,gridTmp.dt(j-1),seq.value[j-1]);
    //     }
    //     result += x;
    // }
    // std::cout << "test mean = " << std::setprecision(12) << (result/N) <<
    // std::endl;
    // return 0;
    // end test path generation

    // full pricing engine and creation of proxy

    boost::shared_ptr<PricingEngine> mcEngine = MakeMcFxTarfEngine<>(gkProcess)
                                                    .withStepsPerYear(steps)
                                                    .withSamples(samples)
                                                    .withSeed(seed)
                                                    .withProxy();
    // same, but without proxy pricing
    boost::shared_ptr<PricingEngine> mcEngine2 = MakeMcFxTarfEngine<>(gkProcess)
                                                     .withStepsPerYear(steps)
                                                     .withSamples(samples2)
                                                     .withSeed(seed);
    tarf.setPricingEngine(mcEngine);
    std::cerr << "spot=" << exRate << " steps=" << steps
              << " samples=" << samples << " samples2=" << samples2
              << " type=" << type[0] << " target=" << target
              << " shortLev=" << shortLeverage << " initialAm=" << initialAm
              << std::endl;
    std::cerr << "MCEngine NPV = " << tarf.NPV() << " error est "
              << tarf.errorEstimate() << std::endl;

    // return 0; //test

    // transport the proxy to the proxy engine

    int interpolate = atoi(getenv("INT"));

    boost::shared_ptr<PricingEngine> proxyEngine =
        boost::make_shared<ProxyFxTarfEngine>(
            tarf.proxy(), fxQuote, gkProcess->riskFreeRate(), interpolate);

    tarf.setPricingEngine(proxyEngine);

    std::cerr << "ProxyEngine NPV (orig eval date and spot)= " << tarf.NPV()
              << std::endl;

    // estimate NPVs and compare to full MC pricing

    int shiftDays = atoi(getenv("DAYSHIFT"));
    Settings::instance().evaluationDate() = refDate + shiftDays;

    std::cerr << "output simulated npvs with refDate "
              << Settings::instance().evaluationDate()
              << " (DayShift=" << shiftDays << ")" << std::endl;

    double newAccAmount = atof(getenv("ACCPROXY"));
    initialAmQ->setValue(newAccAmount); // accumulated amount
    int stepOver = 0;
    for (Real x = 0.50; x <= 2.00; x += 0.01) {
        fx1->setValue(x);
        std::cout << x << " " << tarf.NPV() << " "
                  << tarf.result<Real>("coreRegionMin") << " "
                  << tarf.result<Real>("coreRegionMax") << " ";
        if (!stepOver--) {
            stepOver = 9;
            tarf.setPricingEngine(mcEngine2);
            std::cout << tarf.NPV() << " " << tarf.errorEstimate();
            tarf.setPricingEngine(proxyEngine);
        } else
            std::cout << "0.0 0.0";
        std::cout << std::endl;
    }

    return 0;
}
