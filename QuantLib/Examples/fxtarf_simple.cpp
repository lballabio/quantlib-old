#include <ql/experimental/fx/all.hpp>
#include <ql/currencies/all.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/termstructures/volatility/equityfx/blackconstantvol.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/time/date.hpp>
#include <ql/time/calendars/all.hpp>

#include <iostream>

using namespace QuantLib;

int main() {

    Date refDate(28, Aug, 2015);
    Settings::instance().evaluationDate() = refDate;

    ExchangeRate eurusd(EURCurrency(), USDCurrency(), 1.12225);

    Handle<YieldTermStructure> eurYts(boost::make_shared<FlatForward>(
        0, TARGET(), -0.000011, Actual365Fixed()));
    Handle<YieldTermStructure> usdYts(boost::make_shared<FlatForward>(
        0, TARGET(), 0.004202, Actual365Fixed()));

    JointCalendar TgtNy((TARGET()), (UnitedStates()));

    boost::shared_ptr<FxIndex> ecbIndex = boost::make_shared<FxIndex>(
        "ECB", 2, EURCurrency(), USDCurrency(), TgtNy, eurYts, usdYts);

    std::vector<Date> tarfScheduleDates;
    tarfScheduleDates.push_back(Date(7, Aug, 2015));
    tarfScheduleDates.push_back(Date(14, Jun, 2016));

    Schedule tarfSchedule(tarfScheduleDates);

    Handle<Quote> accumulated(boost::make_shared<SimpleQuote>(0.0));
    Handle<Quote> last(boost::make_shared<SimpleQuote>(0.0));

    FxTarf tarf(tarfSchedule, ecbIndex, 1000000.0,
                boost::make_shared<PlainVanillaPayoff>(Option::Put, 0.98),
                boost::make_shared<PlainVanillaPayoff>(Option::Call, 0.98),
                0.05, FxTarf::capped, 1.0, 1.0, accumulated, last);

    boost::shared_ptr<SimpleQuote> fx1 =
        boost::make_shared<SimpleQuote>(eurusd.rate());
    RelinkableHandle<Quote> fxQuote(fx1);

    Handle<BlackVolTermStructure> volTs(boost::make_shared<BlackConstantVol>(
        0, TARGET(), 0.094, Actual365Fixed()));
    boost::shared_ptr<GarmanKohlagenProcess> gkProcess =
        boost::make_shared<GarmanKohlagenProcess>(fxQuote, eurYts, usdYts,
                                                  volTs);

    boost::shared_ptr<PricingEngine> mcEngine = MakeMcFxTarfEngine<>(gkProcess)
                                                    .withStepsPerYear(12)
                                                    .withSamples(10000)
                                                    .withSeed(42)
                                                    .withProxy();

    tarf.setPricingEngine(mcEngine);

    std::clog << "Pricing as of original evaluation date (client's view): "
              << tarf.NPV() << std::endl;

    boost::shared_ptr<PricingEngine> proxyEngine =
        boost::make_shared<ProxyFxTarfEngine>(tarf.proxy(), fxQuote,
                                              gkProcess->riskFreeRate());

    tarf.setPricingEngine(proxyEngine);

    std::clog << "Pricing with proxy engine (original evaluation date): "
              << tarf.NPV() << std::endl;

    // Scenario calculation

    Settings::instance().evaluationDate() = Date(9, Jun, 2016);

    std::clog << "Pricing a few days before maturity, different spots:"
              << std::endl;
    std::clog << "spot proxy_npv core_region_min core_region_max" << std::endl;
    for (Real x = 0.50; x <= 1.50; x += 0.05) {
        fx1->setValue(x);
        std::cout << x << " " << tarf.NPV() << " "
                  << tarf.result<Real>("coreRegionMin") << " "
                  << tarf.result<Real>("coreRegionMax") << std::endl;
    }

    return 0;
}
