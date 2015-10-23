#include <ql/quantlib.hpp>

#include <boost/make_shared.hpp>

using namespace QuantLib;

int example01() {

    try {

        Date refDate(13, November, 2013);
        Date settlDate = TARGET().advance(refDate, 2 * Days);
        Settings::instance().evaluationDate() = refDate;

        Handle<Quote> rateLevel(new SimpleQuote(0.03));
        Handle<YieldTermStructure> yts(
            new FlatForward(refDate, rateLevel, Actual365Fixed()));

        boost::shared_ptr<IborIndex> iborIndex(new Euribor(6 * Months, yts));
        boost::shared_ptr<SwapIndex> swapIndex(
            new EuriborSwapIsdaFixA(10 * Years, yts));

        iborIndex->addFixing(refDate, 0.0200);
        swapIndex->addFixing(refDate, 0.0315);

        // Handle<Quote> volatilityLevel(new SimpleQuote(0.30));
        // Handle<SwaptionVolatilityStructure> swaptionVol(
        //     new ConstantSwaptionVolatility(refDate, TARGET(), Following,
        //                                    volatilityLevel,
        // Actual365Fixed()));

        Handle<SwaptionVolatilityStructure> swaptionVol(
            new SingleSabrSwaptionVolatility(refDate, TARGET(), Following, 0.05,
                                             0.80, -0.30, 0.20,
                                             Actual365Fixed(), swapIndex));

        // Real strike = 0.0001;
        // while(strike < 1.0) {
        //     std::cout << strike << " " <<
        // swaptionVol->volatility(10.0,10.0,strike) << std::endl;
        //     strike += 0.0010;
        // }
        // return 0;

        Date termDate = TARGET().advance(settlDate, 10 * Years);

        Schedule sched1(settlDate, termDate, 1 * Years, TARGET(),
                        ModifiedFollowing, ModifiedFollowing,
                        DateGeneration::Forward, false);
        Schedule sched2(settlDate, termDate, 6 * Months, TARGET(),
                        ModifiedFollowing, ModifiedFollowing,
                        DateGeneration::Forward, false);

        Real nominal = 100000.0;

        boost::shared_ptr<FloatFloatSwap> cmsswap(
            new FloatFloatSwap( // CMS Swap
                VanillaSwap::Payer, nominal, nominal, sched1, swapIndex,
                Thirty360(), sched2, iborIndex, Actual360(), false, false, 1.0,
                0.00, Null<Real>(), Null<Real>(), 1.0, 0.00267294));

        // boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap(  //
        // Reversed Floored CMS Swap
        //     VanillaSwap::Payer, nominal, nominal, sched1, swapIndex,
        //     Thirty360(), sched2, iborIndex, Actual360(),
        //     false,false,-1.0,0.03,Null<Real>(),0.0,1.0,0.0));

        // boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap( // fix
        // float swap as FloatFloatSwap instrument
        //     VanillaSwap::Payer, nominal, nominal, sched1, swapIndex,
        //     Thirty360(), sched2, iborIndex, Actual360(),
        //     false,false,0.0,0.05,Null<Real>(),Null<Real>(),1.0,0.0));

        std::vector<Date> exerciseDates;
        std::vector<Date> sigmaSteps;
        std::vector<Real> sigma;

        sigma.push_back(0.01);
        for (Size i = 1; i < sched1.size() - 1; i++) {
            exerciseDates.push_back(swapIndex->fixingDate(sched1[i]));
            sigmaSteps.push_back(exerciseDates.back());
            sigma.push_back(0.01);
        }

        // double v[] =
        // {0.01,0.0115446,0.0111677,0.0113092,0.0112404,0.0111856,0.0111218,0.0110547,0.0112791,0.00945792};
        // sigma = std::vector<Real>(v,v+10);

        // boost::shared_ptr<FloatFloatSwap> underlying(new FloatFloatSwap(
        //     VanillaSwap::Receiver, nominal, nominal, sched1, swapIndex,
        //     Thirty360(), sched2, iborIndex, Actual360(),
        //     false,false,1.0,0.0,Null<Real>(),Null<Real>(),1.0,0.00267294));

        boost::shared_ptr<Exercise> exercise(
            new BermudanExercise(exerciseDates));
        boost::shared_ptr<FloatFloatSwaption> callRight(
            new FloatFloatSwaption(cmsswap, exercise));

        std::vector<Date> cmsFixingDates(exerciseDates);
        std::vector<Period> cmsTenors(exerciseDates.size(), 10 * Years);

        Handle<Quote> reversionLevel(new SimpleQuote(0.02));

        boost::shared_ptr<NumericHaganPricer> haganPricer(
            new NumericHaganPricer(swaptionVol,
                                   GFunctionFactory::NonParallelShifts,
                                   reversionLevel));
        setCouponPricer(cmsswap->leg(0), haganPricer);

        // hull white model (change the model->calibrate call below)
        std::vector<Date> sigmaSteps2(sigmaSteps.begin(), sigmaSteps.end() - 1);
        std::vector<Real> sigma2(sigma.begin(), sigma.end() - 1);
        // boost::shared_ptr<Gsr> model(new
        // Gsr(yts,sigmaSteps2,sigma2,reversionLevel->value()));
        boost::shared_ptr<Gaussian1dModel> model2(
            new Gsr(yts, sigmaSteps2, sigma2, reversionLevel->value()));

        Handle<SwaptionVolatilityStructure> hwVol(
            new Gaussian1dSwaptionVolatility(model2, swapIndex));
        Real moneyIn[] = { 0.20, 0.50, 0.75, 1.0, 1.5, 2.0, 5.0, 10.0 };
        std::vector<Real> money(moneyIn, moneyIn + 8);

        // markov model (change the model->calibrate call below)
        // boost::math::ntl::RR::SetPrecision(113);
        boost::shared_ptr<MarkovFunctional> model(new MarkovFunctional(
            yts, reversionLevel->value(), sigmaSteps, sigma, swaptionVol,
            cmsFixingDates, cmsTenors,
            swapIndex // set vol structure for mf here
            /*,MarkovFunctional::ModelSettings().withAdjustments(
                MarkovFunctional::ModelSettings::SabrSmile |
                MarkovFunctional::ModelSettings::
                    SmileExponentialExtrapolation)
                    .withSmileMoneynessCheckpoints(money)*/));

        boost::shared_ptr<Gaussian1dFloatFloatSwaptionEngine> floatEngine(
            new Gaussian1dFloatFloatSwaptionEngine(model));

        callRight->setPricingEngine(floatEngine);

        std::cout << "determine cal basket" << std::endl;

        boost::shared_ptr<Gaussian1dSwaptionEngine> stdEngine(
            new Gaussian1dSwaptionEngine(model));

        boost::shared_ptr<SwapIndex> swapBase(
            new EuriborSwapIsdaFixA(30 * Years, yts));

        LevenbergMarquardt opt;
        EndCriteria ec(2000, 500, 1E-8, 1E-8, 1E-8);

        Size iteration = 0;
        while (iteration < 1) { // set number of iterations here ...
            std::vector<boost::shared_ptr<CalibrationHelper> > basket =
                callRight->calibrationBasket(
                    swapBase, *swaptionVol, // set vol structure for basket here
                    // BasketGeneratingEngine::Naive
                    BasketGeneratingEngine::MaturityStrikeByDeltaGamma);

            for (Size i = 0; i < basket.size(); i++)
                basket[i]->setPricingEngine(stdEngine);
            model->calibrate(basket, opt, ec); // for markov
            // model->calibrate(basket, opt, ec, Constraint(),
            // std::vector<Real>(), model->FixedReversions()); // for gsr

            std::cout << "option date & maturity date & nominal & strike & "
                         "model vol \\\\" << std::endl;
            for (Size i = 0; i < basket.size(); i++) {
                boost::shared_ptr<SwaptionHelper> h =
                    boost::dynamic_pointer_cast<SwaptionHelper>(basket[i]);
                std::cout << exerciseDates[i] << " & "
                          << h->underlyingSwap()->fixedSchedule().dates().back()
                          << " & " << h->underlyingSwap()->nominal() << " & "
                          << h->underlyingSwap()->fixedRate() << " & "
                          << model->volatility()[i] << " \\\\" << std::endl;
            }
            std::cout << model->volatility().back() << std::endl;
            iteration++;
        }

        Real analyticSwapNpv = CashFlows::npv(cmsswap->leg(1), **yts, false) -
                               CashFlows::npv(cmsswap->leg(0), **yts, false);
        Real callRightNpv = callRight->NPV();
        Real firstCouponNpv = -cmsswap->leg(0)[0]->amount() *
                                  yts->discount(cmsswap->leg(0)[0]->date()) +
                              cmsswap->leg(1)[0]->amount() *
                                  yts->discount(cmsswap->leg(1)[0]->date());
        Real underlyingNpv =
            callRight->result<Real>("underlyingValue") + firstCouponNpv;

        std::cout << "Swap Npv (Hagan)     & " << analyticSwapNpv << "\\\\"
                  << std::endl;
        std::cout << "Call Right Npv (MF)  & " << callRightNpv << "\\\\"
                  << std::endl;
        std::cout << "Underlying Npv (MF)  & " << underlyingNpv << "\\\\"
                  << std::endl;
        std::cout << "fair margin swap & "
                  << -analyticSwapNpv / CashFlows::bps(cmsswap->leg(1), **yts,
                                                       false) << std::endl;

        // std::cout << "Model trace : " << std::endl << model->modelOutputs()
        // << std::endl;

        return 0;
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

int example02() {

    try {

        Date refDate(13, November, 2013);
        Date settlDate = TARGET().advance(refDate, 2 * Days);
        Settings::instance().evaluationDate() = refDate;

        Handle<Quote> rateLevel(new SimpleQuote(0.03));
        Handle<YieldTermStructure> yts(
            new FlatForward(refDate, rateLevel, Actual365Fixed()));

        boost::shared_ptr<IborIndex> iborIndex(new Euribor(6 * Months, yts));
        boost::shared_ptr<SwapIndex> swapIndex(
            new EuriborSwapIsdaFixA(10 * Years, yts));

        iborIndex->addFixing(refDate, 0.0200);
        swapIndex->addFixing(refDate, 0.0315);

        // Handle<Quote> volatilityLevel(new SimpleQuote(0.20)); // vol here !
        // Handle<SwaptionVolatilityStructure> swaptionVol(
        //     new ConstantSwaptionVolatility(refDate, TARGET(), Following,
        //                                    volatilityLevel, Actual365Fixed()));

        Handle<SwaptionVolatilityStructure> swaptionVol(
            new SingleSabrSwaptionVolatility(refDate, TARGET(), Following,
        0.10,
                                             0.80, -0.30, 0.40,
                                             Actual365Fixed(), swapIndex));

        // Real strike = 0.0001;
        // while(strike < 0.50) {
        //     std::cout << strike << " " <<
        // swaptionVol->volatility(10.0,10.0,strike) << std::endl;
        //     strike += 0.0050;
        // }
        // return 0;

        Date termDate = TARGET().advance(settlDate, 10 * Years);

        Schedule sched1(settlDate, termDate, 1 * Years, TARGET(),
                        ModifiedFollowing, ModifiedFollowing,
                        DateGeneration::Forward, false);
        Schedule sched2(settlDate, termDate, 6 * Months, TARGET(),
                        ModifiedFollowing, ModifiedFollowing,
                        DateGeneration::Forward, false);

        Real nominal = 100000.0;

        boost::shared_ptr<FloatFloatSwap> cmsswap(
            new FloatFloatSwap( // CMS Swap
                VanillaSwap::Payer, nominal, nominal, sched1, swapIndex,
                Thirty360(), sched2, iborIndex, Actual360(), false, false, 1.0,
                0.00, Null<Real>(), Null<Real>(), 1.0, 0.0));

        Handle<Quote> reversionLevel(new SimpleQuote(0.01)); // reversion here !

        boost::shared_ptr<NumericHaganPricer> haganPricerN(
            new NumericHaganPricer(swaptionVol,
                                   GFunctionFactory::Standard,
                                   reversionLevel));
        boost::shared_ptr<AnalyticHaganPricer> haganPricerA(
            new AnalyticHaganPricer(swaptionVol,
                                    GFunctionFactory::Standard,
                                    reversionLevel));

        // auto integrator =
        // boost::make_shared<GaussLobattoIntegral>(1000,1E-4);

        boost::shared_ptr<LinearTsrPricer> tsrPricer(new LinearTsrPricer(
            swaptionVol, reversionLevel, Handle<YieldTermStructure>(),
            LinearTsrPricer::Settings().withRateBound(0.0, 1.0)
            //.withVegaRatio(0.01)
            ));

        boost::shared_ptr<CmsReplicationPricer> replPricer(
            new CmsReplicationPricer(swaptionVol, reversionLevel));

        Real strike = 0.0001;
        while (strike <= 0.1000) {

            auto tmpCap = boost::shared_ptr<CappedFlooredCoupon>(new CappedFlooredCmsCoupon(
                Date(13, November, 2023), 100000.0, Date(13, November, 2022),
                Date(13, November, 2023), 2, swapIndex, 1.0, 0.0, strike,
                Null<Rate>(), Date(), Date(), DayCounter(), false));
            auto cap = boost::make_shared<StrippedCappedFlooredCoupon>(tmpCap);
            auto tmpFloor = boost::shared_ptr<CappedFlooredCoupon>(
                new CappedFlooredCmsCoupon(
                    Date(13, November, 2023), 100000.0,
                    Date(13, November, 2022), Date(13, November, 2023), 2,
                    swapIndex, 1.0, 0.0, Null<Real>(), strike, Date(), Date(),
                    DayCounter(), false));
            auto floor = boost::make_shared<StrippedCappedFlooredCoupon>(tmpFloor);
            auto swaplet = boost::shared_ptr<CappedFlooredCmsCoupon>(
                new CappedFlooredCmsCoupon(
                    Date(13, November, 2023), 100000.0,
                    Date(13, November, 2022), Date(13, November, 2023), 2,
                    swapIndex, 1.0, 0.0, Null<Rate>(), Null<Rate>(), Date(),
                    Date(), DayCounter(), false));

            cap->setPricer(tsrPricer);
            floor->setPricer(tsrPricer);
            swaplet->setPricer(tsrPricer);
            Real cap1 = cap->adjustedFixing();
            Real floor1 = floor->adjustedFixing();
            Real swaplet1 = swaplet->adjustedFixing();

            // cap->setPricer(haganPricerN);
            // floor->setPricer(haganPricerN);
            // swaplet->setPricer(haganPricerN);
            // Real cap2 = cap->adjustedFixing();
            // Real floor2 = floor->adjustedFixing();
            // Real swaplet2 = swaplet->adjustedFixing();
            // cap->setPricer(replPricer);
            // floor->setPricer(replPricer);
            // swaplet->setPricer(replPricer);
            // Real cap2 = 0.0;//cap->adjustedFixing();
            // Real floor2 = 0.0;//floor->adjustedFixing();
            // Real swaplet2 = 0.0;//swaplet->adjustedFixing();

            // cap->setPricer(tsrPricer);
            // floor->setPricer(tsrPricer);
            // swaplet->setPricer(tsrPricer);
            // Real cap3 = cap->adjustedFixing();
            // Real floor3 = floor->adjustedFixing();
            // Real swaplet3 = swaplet->adjustedFixing();

            // std::cout << strike << " " << cap1 << " " 
            //           << " " << floor1 << " "
            //           << (cap1 - floor1 - (swaplet1 - strike)) << std::endl;

            strike += 0.0001;
        }

        return 0;
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

int example03() {

    try {

        Date refDate(13, November, 2013);
        Date settlDate = TARGET().advance(refDate, 2 * Days);
        Settings::instance().evaluationDate() = refDate;

        Handle<Quote> rateLevel1(new SimpleQuote(0.0350));
        Handle<Quote> rateLevel2(new SimpleQuote(0.0300));
        Handle<YieldTermStructure> yts1(
            new FlatForward(refDate, rateLevel1, Actual365Fixed()));
        Handle<YieldTermStructure> yts2(
            new FlatForward(refDate, rateLevel2, Actual365Fixed()));

        boost::shared_ptr<IborIndex> iborIndex(new Euribor(6 * Months, yts1));
        boost::shared_ptr<SwapIndex> swapIndex1(
            new EuriborSwapIsdaFixA(10 * Years, yts1));
        boost::shared_ptr<SwapIndex> swapIndex2(
            new EuriborSwapIsdaFixA(2 * Years, yts2));

        boost::shared_ptr<SwapSpreadIndex> swapSpreadIndex(
            new SwapSpreadIndex("cms10_2", swapIndex1, swapIndex2));

        Handle<Quote> volatilityLevel(new SimpleQuote(0.40)); // vol here !
        Handle<SwaptionVolatilityStructure> swaptionVol(
            new ConstantSwaptionVolatility(refDate, TARGET(), Following,
                                           volatilityLevel, Actual365Fixed()));

        // Handle<SwaptionVolatilityStructure> swaptionVol(
        //     new SingleSabrSwaptionVolatility(refDate, TARGET(), Following,
        // 0.15,
        //                                      0.80, -0.30, 0.20,
        //                                      Actual365Fixed(), swapIndex));

        Handle<Quote> reversionLevel(new SimpleQuote(0.00)); // reversion here !

        boost::shared_ptr<LinearTsrPricer> tsrPricer(new LinearTsrPricer(
            swaptionVol, reversionLevel, Handle<YieldTermStructure>(),
            LinearTsrPricer::Settings().withRateBound(0.0, 1.0)
            //.withVegaRatio(0.01)
            ));

        Handle<Quote> correlation(new SimpleQuote(0.20)); // correlation here

        boost::shared_ptr<CappedFlooredCoupon> tmpSpreadCoupon(
            new CappedFlooredCmsSpreadCoupon(
                Date(13, November, 2034), 1.0, Date(13, November, 2033),
                Date(13, November, 2034), 2, swapSpreadIndex, 1.0, 0.0,
                Null<Real>(), 0.0050, Date(), Date(), DayCounter(), false));

        boost::shared_ptr<StrippedCappedFlooredCoupon> spreadCoupon =
            boost::make_shared<StrippedCappedFlooredCoupon>(tmpSpreadCoupon);

        // boost::shared_ptr<CmsSpreadCoupon> spreadCoupon(new CmsSpreadCoupon(
        //     Date(13, November, 2024), 1.0, Date(13, November, 2023),
        //     Date(13, November, 2024), 2, swapSpreadIndex, 1.0, 0.0,
        //     Date(), Date(), DayCounter(),false));

        std::cout << "integration_points;rate" << std::setprecision(16)
                  << std::endl;
        for (Size i = 4; i < 64; i++) {

            boost::shared_ptr<LognormalCmsSpreadPricer> spreadPricer(
                new LognormalCmsSpreadPricer(tsrPricer, correlation,
                                             Handle<YieldTermStructure>(), i));

            spreadCoupon->setPricer(spreadPricer);

            std::cout << i << ";" << spreadCoupon->rate() << std::endl;
        }

        return 0;
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

int example04() {

    try {

        Date refDate(13, November, 2013);
        Date settlDate = TARGET().advance(refDate, 2 * Days);
        Settings::instance().evaluationDate() = refDate;

        Handle<Quote> rateLevel1(new SimpleQuote(0.0350));
        Handle<YieldTermStructure> yts1(
            new FlatForward(refDate, rateLevel1, Actual365Fixed()));
        Handle<Quote> rateLevel2(new SimpleQuote(0.0300));
        Handle<YieldTermStructure> yts2(
            new FlatForward(refDate, rateLevel2, Actual365Fixed()));

        boost::shared_ptr<IborIndex> iborIndex(new Euribor(6 * Months, yts1));
        boost::shared_ptr<SwapIndex> swapIndex1(
            new EuriborSwapIsdaFixA(10 * Years, yts1));
        boost::shared_ptr<SwapIndex> swapIndex2(
            new EuriborSwapIsdaFixA(2 * Years, yts2));

        boost::shared_ptr<SwapSpreadIndex> swapSpreadIndex(
            new SwapSpreadIndex("cms10_2", swapIndex1, swapIndex2));

        Handle<Quote> volatilityLevel(new SimpleQuote(0.40)); // vol here !
        Handle<SwaptionVolatilityStructure> swaptionVol(
            new ConstantSwaptionVolatility(refDate, TARGET(), Following,
                                           volatilityLevel, Actual365Fixed()));

        Handle<Quote> reversionLevel(new SimpleQuote(0.00)); // reversion here !

        Schedule sched(settlDate, settlDate + 10 * Years, 1 * Years, TARGET(),
                       ModifiedFollowing, ModifiedFollowing,
                       DateGeneration::Backward, false);

        boost::shared_ptr<FloatFloatSwap> underlying(new FloatFloatSwap(VanillaSwap::Payer,
                                                                        100.0,100.0,
                                                                        sched,iborIndex,Actual360(),
                                                                        sched,swapSpreadIndex,Thirty360(),
                                                                        false,
                                                                        false));

        std::vector<Date> callDates = sched.dates();

        std::cout << "call dates:" << std::endl;
        for(Size i=0;i<callDates.size();i++)
            std::cout << callDates[i] << std::endl;


        boost::shared_ptr<Exercise> exercise = boost::make_shared<BermudanExercise>(callDates);

        boost::shared_ptr<FloatFloatSwaption> swaption = boost::make_shared<FloatFloatSwaption>(underlying,exercise);

        std::vector<Date> volStepDates;
        std::vector<Real> vols(1,0.01);

        boost::shared_ptr<Gaussian1dModel> model = boost::make_shared<Gsr>(yts1,volStepDates,
                                                                           vols,0.01);

        boost::shared_ptr<Gaussian1dFloatFloatSwaptionEngine> engine =
            boost::make_shared<Gaussian1dFloatFloatSwaptionEngine>(
                model, 64, 7.0, true, false, Handle<Quote>(),
                Handle<YieldTermStructure>(), false,
                Gaussian1dFloatFloatSwaptionEngine::Naive);

        swaption->setPricingEngine(engine);

        std::cout << "swaption npv = " << swaption->NPV() << std::endl;
        
        std::vector<Real> probs = swaption->result<std::vector<Real> >("probabilities");        
        for(Size i=0;i<probs.size();i++) {
            std::cout << i << " => " << probs[i] << std::endl;
        }

    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }

    return 0;

}


int main(int, char * []) {

    return example02();
    
}
