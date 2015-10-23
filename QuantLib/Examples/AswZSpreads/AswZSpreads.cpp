#include <ql/quantlib.hpp>

#include <boost/make_shared.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;


void spreads() {

    // set up market data

    Date refDate = Date(14, October, 2013);
    Date settlDate = TARGET().advance(refDate, 2, Days);
    Settings::instance().evaluationDate() = refDate;
    
    boost::shared_ptr<SimpleQuote> rateLevel0(new SimpleQuote(0.025));
    boost::shared_ptr<SimpleQuote> rateLevel(new SimpleQuote(0.03));
    Handle<Quote> forward0(rateLevel0);
    Handle<Quote> forward(rateLevel);
    Handle<YieldTermStructure> yts0(
        boost::make_shared<FlatForward>(refDate, forward0, Actual365Fixed()));
    Handle<YieldTermStructure> yts(
        boost::make_shared<FlatForward>(refDate, forward, Actual365Fixed()));

    boost::shared_ptr<IborIndex> euribor6m(new Euribor(6 * Months, yts));

    // swap and bond

    boost::shared_ptr<VanillaSwap> swap =
        MakeVanillaSwap(20 * Years, euribor6m, 0.04).receiveFixed(false);
    Leg fix = swap->leg(0);
    fix.push_back(
        boost::shared_ptr<CashFlow>(new Redemption(1.0, fix.back()->date())));

    for (Size i = 0; i < fix.size(); i++) {
        std::cout << fix[i]->date() << "  \t" << fix[i]->amount() << std::endl;
    }

    boost::shared_ptr<PricingEngine> discountingEngine(
        new DiscountingSwapEngine(yts0, boost::none, settlDate, settlDate));
    swap->setPricingEngine(discountingEngine);

    std::cout << "swap npv = " << swap->NPV() << std::endl;
    std::cout << "swap bps = " << swap->floatingLegBPS() << std::endl;

    // tabulate zSpread against asset swap spread

    std::ofstream out1;
    out1.open("spreads1c.dat");

    Real zSpread = 0.0;
    while (zSpread <= 0.10) {

        Real bondNpv = CashFlows::npv(
            fix, *yts, zSpread, Actual365Fixed(), Continuous,
            NoFrequency, false, settlDate, settlDate);

        Real swapNpv = swap->NPV();
        Real swapBps = swap->floatingLegBPS();

        Real aswSpread = (1.0 - (bondNpv + swapNpv)) / swapBps / 10000.0;

        out1 << zSpread * 10000.0 << " " << aswSpread * 10000.0 << std::endl;

        zSpread += 0.0001;
    }

    out1.close();

    // tabulate da/dz against zSpread level

    out1.open("spreads2c.dat");

    zSpread = 0.0;
    while (zSpread <= 0.10) {

        Real bondNpv = CashFlows::npv(
            fix, *yts, zSpread, Actual365Fixed(), Continuous,
            NoFrequency, false, settlDate, settlDate);

        Real bondNpvP =
            CashFlows::npv(fix, *yts, zSpread + 0.0001, Actual365Fixed(),
                           Continuous, NoFrequency,
                           false, settlDate, settlDate);

        // Real bondNpv = CashFlows::npv(
        //     fix, *yts, zSpread, Actual360(), Compounding::Compounded,
        //     Frequency::Annual, false, settlDate, settlDate);

        // Real bondNpvP =
        //     CashFlows::npv(fix, *yts, zSpread + 0.0001, Actual360(),
        //                    Compounding::Compounded, Frequency::Annual,
        //                    false, settlDate, settlDate);

        Real swapNpv = swap->NPV();
        Real swapBps = swap->floatingLegBPS();

        Real aswSpread = (1.0 - (bondNpv + swapNpv)) / swapBps / 10000.0;
        Real aswSpreadP = (1.0 - (bondNpvP + swapNpv)) / swapBps / 10000.0;

        out1 << zSpread * 10000.0 << " " << (aswSpreadP - aswSpread) * 10000.0
             << std::endl;

        zSpread += 0.0001;
    }

    out1.close();

    // tabulate da/dy against zSpread level

    out1.open("spreads3c.dat");

    zSpread = 0.0;
    while (zSpread <= 0.10) {

        Real swapNpv = swap->NPV();
        Real swapBps = swap->floatingLegBPS();
        Real bondNpv = CashFlows::npv(
            fix, *yts, zSpread, Actual365Fixed(), Continuous,
            NoFrequency, false, settlDate, settlDate);

        rateLevel0->setValue(rateLevel0->value() + 0.0001);
        rateLevel->setValue(rateLevel->value() + 0.0001);

        Real bondNpvP = CashFlows::npv(
            fix, *yts, zSpread, Actual365Fixed(), Continuous,
             NoFrequency, false, settlDate, settlDate);

        Real swapNpvP = swap->NPV();
        Real swapBpsP = swap->floatingLegBPS();

        rateLevel0->setValue(rateLevel0->value() - 0.0001);
        rateLevel->setValue(rateLevel->value() - 0.0001);

        Real aswSpread = (1.0 - (bondNpv + swapNpv)) / swapBps / 10000.0;
        Real aswSpreadP = (1.0 - (bondNpvP + swapNpvP)) / swapBpsP / 10000.0;

        out1 << zSpread * 10000.0 << " " << (aswSpreadP - aswSpread) * 10000.0
             << std::endl;

        zSpread += 0.0001;
    }

    out1.close();
}

int main(int, char * []) { spreads(); }
