#include <ql/quantlib.hpp>
using namespace QuantLib;

int main(int, char * []) {

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

        Handle<Quote> volatilityLevel(new SimpleQuote(0.30));
        Handle<SwaptionVolatilityStructure> swaptionVol(
            new ConstantSwaptionVolatility(refDate, TARGET(), Following,
                                           volatilityLevel, Actual365Fixed()));

        // Handle<SwaptionVolatilityStructure> swaptionVol(
        //     new SingleSabrSwaptionVolatility(refDate, TARGET(), Following, 0.15,
        //                                      0.80, -0.30, 0.20,
        //                                      Actual365Fixed(), swapIndex));

        // Real strike = 0.0001;
        // while(strike < 1.0) {
        //     std::cout << strike << " " << swaptionVol->volatility(10.0,10.0,strike) << std::endl;
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

        boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap(
            VanillaSwap::Payer, nominal, nominal, sched1, swapIndex,
            Thirty360(), sched2, iborIndex, Actual360(),
            false,false,1.0,0.0,Null<Real>(),Null<Real>(),1.0,0.00267294));

        // boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap(
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

        // double v[] = {0.01,0.0115446,0.0111677,0.0113092,0.0112404,0.0111856,0.0111218,0.0110547,0.0112791,0.00945792};
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
        boost::shared_ptr<Gsr> model(new Gsr(yts,sigmaSteps2,sigma2,reversionLevel->value()));
        boost::shared_ptr<Gaussian1dModel> model2(new Gsr(yts,sigmaSteps2,sigma2,reversionLevel->value()));

        Handle<SwaptionVolatilityStructure> hwVol(new Gaussian1dSwaptionVolatility(model2,swapIndex));
        Real moneyIn[] = { 0.20, 0.50, 0.75, 1.0, 1.5, 2.0, 5.0, 10.0 };
        std::vector<Real> money(moneyIn,moneyIn+8);

        // markov model (change the model->calibrate call below)
        // boost::math::ntl::RR::SetPrecision(113);
        // boost::shared_ptr<MarkovFunctional> model(new MarkovFunctional(
        //     yts, reversionLevel->value(), sigmaSteps, sigma,
        //     swaptionVol, cmsFixingDates, cmsTenors, swapIndex // set vol structure for mf here
        //     /*,MarkovFunctional::ModelSettings().withAdjustments(
        //         MarkovFunctional::ModelSettings::SabrSmile |
        //         MarkovFunctional::ModelSettings::
        //             SmileExponentialExtrapolation)
        //             .withSmileMoneynessCheckpoints(money)*/));

        boost::shared_ptr<Gaussian1dFloatFloatSwaptionEngine> floatEngine(
            new Gaussian1dFloatFloatSwaptionEngine(model));

        callRight->setPricingEngine(floatEngine);

        boost::shared_ptr<SwapIndex> swapBase(
            new EuriborSwapIsdaFixA(30 * Years, yts));
        std::vector<boost::shared_ptr<CalibrationHelper> > basket =
            callRight->calibrationBasket(swapBase, *swaptionVol,   // set vol structure for basket here
                                         //BasketGeneratingEngine::Naive
                                         BasketGeneratingEngine::MaturityStrikeByDeltaGamma
                                         );

        std::cout << "# & option date & maturity date & nominal & strike & market vol & market price \\\\" << std::endl;
        for(Size i=0;i<basket.size();i++) {
            boost::shared_ptr<SwaptionHelper> h = boost::dynamic_pointer_cast<SwaptionHelper>(basket[i]);
            std::cout << i << " & " << exerciseDates[i] << " & " << h->underlyingSwap()->fixedSchedule().dates().back() << " & " <<
                h->underlyingSwap()->nominal() << " & " << h->underlyingSwap()->fixedRate() << " & " << 
                h->volatility()->value() << " & " << h->marketValue() << std::endl;
        }

        boost::shared_ptr<Gaussian1dSwaptionEngine> stdEngine(
            new Gaussian1dSwaptionEngine(model));
        for (Size i = 0; i < basket.size(); i++)
            basket[i]->setPricingEngine(stdEngine);

        LevenbergMarquardt opt;
        EndCriteria ec(2000, 500, 1E-8, 1E-8, 1E-8);
        //model->calibrate(basket, opt, ec); // for markov
        model->calibrate(basket, opt, ec, Constraint(), std::vector<Real>(), model->FixedReversions()); // for gsr

        // iterate calibration basket generation
        Size iteration = 0;
        do {
            std::vector<boost::shared_ptr<CalibrationHelper> > basket2 =
                callRight->calibrationBasket(swapBase, *swaptionVol,   // set vol structure for basket here
                                             //BasketGeneratingEngine::Naive
                                             BasketGeneratingEngine::MaturityStrikeByDeltaGamma
                                             );

            std::cout << "# & option date & maturity date & nominal & strike & market vol & market price \\\\" << std::endl;
            for(Size i=0;i<basket2.size();i++) {
                boost::shared_ptr<SwaptionHelper> h = boost::dynamic_pointer_cast<SwaptionHelper>(basket2[i]);
                std::cout << i << " & " << exerciseDates[i] << " & " << h->underlyingSwap()->fixedSchedule().dates().back() << " & " <<
                    h->underlyingSwap()->nominal() << " & " << h->underlyingSwap()->fixedRate() << " & " << 
                    h->volatility()->value() << " & " << h->marketValue() << std::endl;
                h->setPricingEngine(stdEngine);
            }
            model->calibrate(basket2, opt, ec, Constraint(), std::vector<Real>(), model->FixedReversions()); // for gsr
        } while(++iteration < 10);

        std::cout << "# & model vol & swaption market & swaption model "
                  << std::endl;
        for (Size i = 0; i < basket.size(); i++) {
            std::cout << i << " & " << model->volatility()[i] << " & "
                      << basket[i]->marketValue() << " & "
                      << basket[i]->modelValue() << " \\\\" << std::endl;
        }
        std::cout << model->volatility().back() << std::endl;

        Real analyticSwapNpv = CashFlows::npv(cmsswap->leg(1), **yts, false) -
                               CashFlows::npv(cmsswap->leg(0), **yts, false);
        Real callRightNpv = callRight->NPV();
        Real firstCouponNpv = -cmsswap->leg(0)[0]->amount() *
                                  yts->discount(cmsswap->leg(0)[0]->date()) +
                              cmsswap->leg(1)[0]->amount() *
                                  yts->discount(cmsswap->leg(1)[0]->date());
        Real underlyingNpv =
            callRight->result<Real>("underlyingValue") + firstCouponNpv;

        std::cout << "Swap Npv (Hagan)     & " << analyticSwapNpv << "\\\\" << std::endl;
        std::cout << "Call Right Npv (MF)  & " << callRightNpv << "\\\\" << std::endl;
        std::cout << "Underlying Npv (MF)  & " << underlyingNpv << "\\\\" << std::endl;
        std::cout << "fair margin swap & " << -analyticSwapNpv/CashFlows::bps(cmsswap->leg(1),**yts,false) << std::endl;

        //std::cout << "Model trace : " << std::endl << model->modelOutputs() << std::endl;
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
