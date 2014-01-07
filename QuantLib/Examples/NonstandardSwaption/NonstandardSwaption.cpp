#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

void outputBasket(std::vector<boost::shared_ptr<CalibrationHelper> > basket,
                  boost::shared_ptr<TermStructure> termStructure) {

	std::cout << "Calibration Basket:" << std::endl;
    std::cout << "expiry;maturityDate;expiryTime;maturityTime;nominal;rate" << std::endl;
    for(Size j=0;j<basket.size();j++) {
        boost::shared_ptr<SwaptionHelper> helper = boost::dynamic_pointer_cast<SwaptionHelper>(basket[j]);
        Date endDate = helper->underlyingSwap()->fixedSchedule().dates().back();
        Real nominal = helper->underlyingSwap()->nominal();
        Real vol = helper->volatility()->value();
        Real rate = helper->underlyingSwap()->fixedRate();
        Date expiry = helper->swaption()->exercise()->date(0);
        Real expiryTime = termStructure->timeFromReference(expiry);
        Real endTime = termStructure->timeFromReference(endDate);
        // std::cout << expiry << ";" << endDate << ";" << expiryTime << ";" << endTime << ";" << 
        //     nominal << ";" << rate << ";" << vol << std::endl;
        std::cout << expiry << " & " << endDate << " & " << expiryTime << " & " << endTime << " & " << 
            nominal << " & " << rate << " \\\\" << std::endl;
    }

}

void outputModel(std::vector<Date>& expiries, boost::shared_ptr<Gsr> model) {

    std::cout << "Model parameters: ";
    std::cout << "expiry;volatility" << std::endl;
    for(Size i=0;i<expiries.size();i++) { // first parameters are the vols, after that the mean reversions follow
        //std::cout << expiries[i] << ";" << model->params()[i] << std::endl;
        std::cout << expiries[i] << " & " << model->volatility()[i] << " \\\\" << std::endl;
    }
    std::cout << std::endl;

}


void example01() {

    Date refDate(17,June,2013); 
    Settings::instance().evaluationDate() = refDate;
	Date effective = TARGET().advance(refDate,2*Days);
	Date maturity = TARGET().advance(effective,10*Years);

    // market data: flat yts 3%, flat vol 20%
    
    Real rateLevel = 0.03;
    Real volLevel = 0.20;

    boost::shared_ptr<Quote> ytsQuote0(new SimpleQuote(rateLevel));
    boost::shared_ptr<Quote> ytsQuote1(new SimpleQuote(rateLevel+0.0010)); // 10bp shift up for dv01, dv02 calculation
    boost::shared_ptr<Quote> ytsQuote2(new SimpleQuote(rateLevel-0.0010)); // 10bp shift down for dv01, dv02 calculation
    
    RelinkableHandle<Quote> ytsQuote(ytsQuote0);

    Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),ytsQuote,
                                                                                              Actual365Fixed())));

    boost::shared_ptr<Quote> volQuote0(new SimpleQuote(volLevel));
    boost::shared_ptr<Quote> volQuote1(new SimpleQuote(volLevel+0.01));

    RelinkableHandle<Quote> volQuote(volQuote0);

	boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(new ConstantSwaptionVolatility(0,TARGET(),
                                                          ModifiedFollowing,volQuote,Actual365Fixed()));
   
	boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months,yts));
	boost::shared_ptr<SwapIndex> standardSwapBase(new EuriborSwapIsdaFixA(10*Years,yts));

    // non standard swaption instrument (10y, amortizing nominal and step up coupon, yearly exercise dates)

    std::vector<Real> fixedNominal(10), floatingNominal(20), fixedRate(10);
	for(Size i=0;i<10;i++) {
		fixedNominal[i] = 100.0;//-i*10.0;//-i*5000000;//(i>0 ? fixedNominal[i-1] : 100000000.0 )*1.075;
		floatingNominal[2*i] = floatingNominal[2*i+1] = fixedNominal[i];
		fixedRate[i] = 0.035;//+0.0050*i;
	}

	Schedule fixedSchedule(effective,maturity,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);
	Schedule floatingSchedule(effective,maturity,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);

	boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(VanillaSwap::Payer,fixedNominal,floatingNominal,
                                                                      fixedSchedule,fixedRate,Thirty360(),floatingSchedule,
                                                                      iborIndex,1.0,0.0,Actual360()));
    
    std::vector<Date> exerciseDates;
	for(Size i=1;i<10;i++) exerciseDates.push_back(TARGET().advance(fixedSchedule[i],-2*Days));
    boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));

	boost::shared_ptr<NonstandardSwaption> swaption(new NonstandardSwaption(underlying,exercise));

    // gsr model (1% mean reversion, intially 1% vol)

    exerciseDates.pop_back();
	std::vector<Date> stepDates(exerciseDates);
	std::vector<Real> vols(exerciseDates.size()+1,0.01);
	std::vector<Real> reversions(exerciseDates.size()+1,0.01);

    boost::shared_ptr<Gsr> gsr(new Gsr(yts,stepDates,vols,reversions,50.0));

    // engines for nonstandard swaption and standard swaption
    
    // this engine is used for standard swaptions used in model calibration
    boost::shared_ptr<PricingEngine> standardEngine(new Gaussian1dSwaptionEngine(gsr));
    // this engine is used for the non standard swaption
    boost::shared_ptr<PricingEngine> nonStandardEngine(new Gaussian1dNonstandardSwaptionEngine(gsr));

    swaption->setPricingEngine(nonStandardEngine);

    std::cout.precision(6);
    std::cout << std::fixed;

    std::cout << "============================================================" << std::endl;
    std::cout << "Model is not calibrated" << std::endl;
    std::cout << "============================================================" << std::endl;
    outputModel(stepDates,gsr);

    std::cout << "Calculate calibration basket" << std::endl;
    std::vector<boost::shared_ptr<CalibrationHelper> > basket = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket.size();i++) basket[i]->setPricingEngine(standardEngine);
    outputBasket(basket,*gsr->termStructure());

    std::cout << "Calibrate the model to the initial basket" << std::endl;
    LevenbergMarquardt lm;
    EndCriteria ec(2000,200,1E-8,1E-8,1E-8);
    gsr->calibrateVolatilitiesIterative(basket,lm,ec);
    outputModel(stepDates,gsr);

    std::cout << "Calculate calibration basket" << std::endl;

    std::vector<boost::shared_ptr<CalibrationHelper> > basket2 = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket2.size();i++) basket2[i]->setPricingEngine(standardEngine);
    outputBasket(basket2,*gsr->termStructure());

    std::cout << "Calibrate the model to the second basket" << std::endl;

    gsr->calibrateVolatilitiesIterative(basket2,lm,ec);
    outputModel(stepDates,gsr);

    std::cout << "Price the nonstandard swaption" << std::endl;

    Real npv0 = swaption->NPV();
 
    std::cout << "Shift the rate curve by 10bp up, down and reprice to compute delta, gamma" << std::endl;

    ytsQuote.linkTo(ytsQuote1);
    Real npv1 = swaption->NPV();
    ytsQuote.linkTo(ytsQuote2);
    Real npv2 = swaption->NPV();
    ytsQuote.linkTo(ytsQuote0);

    Real npv3 = swaption->NPV();

    std::cout << "NPV(-10bp) = " << npv2 << " NPV(0) = " << npv0 << " NPV(+10bp) = " << npv1 << std::endl;
    std::cout << "DV01 = " << (npv1-npv2) / 20.0 << std::endl;
    std::cout << "DV02 = " << (npv1-2.0*npv0+npv2) / 100.0 << std::endl;
    std::cout << "Vega = " << (npv3-npv0) << std::endl;

    std::cout << "============================================================" << std::endl;
    std::cout << "Compute delta, gamma with model recalibration" << std::endl;
    std::cout << "============================================================" << std::endl;
    
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec);
    outputModel(stepDates,gsr);
    npv0 = swaption->NPV();
    
    ytsQuote.linkTo(ytsQuote1);
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    npv1 = swaption->NPV();
    ytsQuote.linkTo(ytsQuote2);
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    npv2 = swaption->NPV();
    ytsQuote.linkTo(ytsQuote0);

    for(Size i=0;i<basket2.size();i++) boost::dynamic_pointer_cast<SimpleQuote>(*basket2[i]->volatility())
                                           ->setValue(volQuote1->value());
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    npv3 = swaption->NPV();
    for(Size i=0;i<basket2.size();i++) boost::dynamic_pointer_cast<SimpleQuote>(*basket2[i]->volatility())
                                           ->setValue(volQuote0->value());

    std::cout << "NPV(-10bp) = " << npv2 << " NPV(0) = " << npv0 << " NPV(+10bp) = " << npv1 << std::endl;
    std::cout << "DV01 = " << (npv1-npv2) / 20.0 << std::endl;
    std::cout << "DV02 = " << (npv1-2.0*npv0+npv2) / 100.0 << std::endl;
    std::cout << "Vega = " << (npv3-npv0) << std::endl;

    std::cout << "============================================================" << std::endl;
    std::cout << "Compute delta, gamma with basket recalculation and recalibration" << std::endl;
    std::cout << "============================================================" << std::endl;

    gsr->calibrateVolatilitiesIterative(basket2,lm,ec);
    outputModel(stepDates,gsr);
    npv0 = swaption->NPV(); 

    ytsQuote.linkTo(ytsQuote1);
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    std::vector<boost::shared_ptr<CalibrationHelper> > basket3a = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket3a.size();i++) basket3a[i]->setPricingEngine(standardEngine);
    outputBasket(basket3a,*gsr->termStructure());
    gsr->calibrateVolatilitiesIterative(basket3a,lm,ec); outputModel(stepDates,gsr);
    npv1 = swaption->NPV();

    ytsQuote.linkTo(ytsQuote2);

    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    std::vector<boost::shared_ptr<CalibrationHelper> > basket3b = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket3b.size();i++) basket3b[i]->setPricingEngine(standardEngine);
    outputBasket(basket3b,*gsr->termStructure());
    gsr->calibrateVolatilitiesIterative(basket3b,lm,ec); outputModel(stepDates,gsr);
    npv2 = swaption->NPV();

    ytsQuote.linkTo(ytsQuote0);

    for(Size i=0;i<basket2.size();i++) boost::dynamic_pointer_cast<SimpleQuote>(*basket2[i]->volatility())->
                                           setValue(volQuote1->value());
    volQuote.linkTo(volQuote1);
    gsr->calibrateVolatilitiesIterative(basket2,lm,ec); outputModel(stepDates,gsr);
    std::vector<boost::shared_ptr<CalibrationHelper> > basket3c = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket3c.size();i++) basket3c[i]->setPricingEngine(standardEngine);
    outputBasket(basket3c,*gsr->termStructure());
    gsr->calibrateVolatilitiesIterative(basket3c,lm,ec); outputModel(stepDates,gsr);
    npv3 = swaption->NPV();
    for(Size i=0;i<basket2.size();i++) boost::dynamic_pointer_cast<SimpleQuote>(*basket2[i]->volatility())->
                                           setValue(volQuote0->value());
    volQuote.linkTo(volQuote0);


    std::cout << "NPV(-10bp) = " << npv2 << " NPV(0) = " << npv0 << " NPV(+10bp) = " << npv1 << std::endl;
    std::cout << "DV01 = " << (npv1-npv2) / 20.0 << std::endl;
    std::cout << "DV02 = " << (npv1-2.0*npv0+npv2) / 100.0 << std::endl;
    std::cout << "Vega = " << (npv3-npv0) << std::endl;

    return;

}

void example02() {

    Date refDate(17, June, 2013);
    Settings::instance().evaluationDate() = refDate;
    Date effective = TARGET().advance(refDate, 2 * Days);
    Date maturity = TARGET().advance(effective, 10 * Years);

    Handle<Quote> rate3m(new SimpleQuote(0.03));
    Handle<Quote> rate6m(new SimpleQuote(0.04));

    Handle<YieldTermStructure> yts3m(boost::shared_ptr<YieldTermStructure>(
        new FlatForward(0, TARGET(), rate3m, Actual365Fixed())));
    Handle<YieldTermStructure> yts6m(boost::shared_ptr<YieldTermStructure>(
        new FlatForward(0, TARGET(), rate6m, Actual365Fixed())));

    Handle<Quote> volQuote6m(new SimpleQuote(0.20));

    boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(
        new ConstantSwaptionVolatility(0, TARGET(), ModifiedFollowing,
                                       volQuote6m, Actual365Fixed()));

    boost::shared_ptr<IborIndex> iborIndex3m(new Euribor(3 * Months, yts3m));
    boost::shared_ptr<IborIndex> iborIndex6m(new Euribor(6 * Months, yts6m));
    boost::shared_ptr<SwapIndex> standardSwapBase(
        new EuriborSwapIsdaFixA(10 * Years, yts6m));

    // 3m atm swaption 5y into 10y

    std::vector<Real> fixedNominal(10), floatingNominal(20), fixedRate(10);
    for (Size i = 0; i < 10; i++) {
        fixedNominal[i] = 100.0;
        floatingNominal[2 * i] = floatingNominal[2 * i + 1] = fixedNominal[i];
        fixedRate[i] = 0.04;
    }

    Schedule fixedSchedule(effective, maturity, 1 * Years, TARGET(),
                           ModifiedFollowing, ModifiedFollowing,
                           DateGeneration::Forward, false);
    Schedule floatingSchedule(effective, maturity, 6 * Months, TARGET(),
                              ModifiedFollowing, ModifiedFollowing,
                              DateGeneration::Forward, false);

    boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(
        VanillaSwap::Payer, fixedNominal, floatingNominal, fixedSchedule,
        fixedRate, Thirty360(), floatingSchedule, iborIndex3m, 1.0, 0.0,
        Actual360()));

    std::vector<Date> exerciseDates(1, Date(fixedSchedule[5]));
    boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));
    boost::shared_ptr<NonstandardSwaption> swaption(
        new NonstandardSwaption(underlying, exercise));

    // gsr model (1% mean reversion, intially 1% vol)

    std::vector<Date> stepDates;
    std::vector<Real> vols(1, 0.01);
    std::vector<Real> reversions(1, 0.01);

    boost::shared_ptr<Gsr> gsr(
        new Gsr(yts6m, stepDates, vols, reversions, 50.0));

    // engines for nonstandard swaption and standard swaption

    // this engine is used for standard swaptions used in model calibration
    boost::shared_ptr<PricingEngine> standardEngine(
        new Gaussian1dSwaptionEngine(gsr));
    // this engine is used for the non standard swaption
    boost::shared_ptr<PricingEngine> nonStandardEngine(
        new Gaussian1dNonstandardSwaptionEngine(gsr));

    swaption->setPricingEngine(nonStandardEngine);

    std::vector<boost::shared_ptr<CalibrationHelper> > basket =
        swaption->calibrationBasket(
            standardSwapBase, swaptionVol,
            BasketGeneratingEngine::MaturityStrikeByDeltaGamma);

    std::cout << "6m swaption that matches 3m swaption:" << std::endl;
    outputBasket(basket,*yts6m);

}

void example03() {

    Date refDate(7, January, 2014);
    Settings::instance().evaluationDate() = refDate;
    Date effective = TARGET().advance(refDate, 2 * Days);

    Date expiry = TARGET().advance(refDate,1*Months); // option expiry
    Date start = TARGET().advance( TARGET().advance(expiry,2*Days), 1*Years); // start date underlying
    Date end = TARGET().advance( start, 5*Years);

    Handle<Quote> rate(new SimpleQuote(0.03));

    Handle<YieldTermStructure> yts(boost::shared_ptr<YieldTermStructure>(
        new FlatForward(0, TARGET(), rate, Actual365Fixed())));

    Handle<Quote> volQuote6m(new SimpleQuote(0.20));

    boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(
        new ConstantSwaptionVolatility(0, TARGET(), ModifiedFollowing,
                                       volQuote6m, Actual365Fixed()));

    boost::shared_ptr<IborIndex> iborIndex(new Euribor(6 * Months, yts));
    boost::shared_ptr<SwapIndex> standardSwapBase(
                               new EuriborSwapIsdaFixA(5 * Years, yts));

    // swaption 1m expiry into 1y forward starting swap (5y maturity)

    std::vector<Real> fixedNominal(5), floatingNominal(10), fixedRate(5);
    for (Size i = 0; i < 5; i++) {
        fixedNominal[i] = 100.0;
        floatingNominal[2 * i] = floatingNominal[2 * i + 1] = fixedNominal[i];
        fixedRate[i] = 0.03;
    }

    Schedule fixedSchedule(start, end, 1 * Years, TARGET(),
                           ModifiedFollowing, ModifiedFollowing,
                           DateGeneration::Forward, false);
    Schedule floatingSchedule(start, end, 6 * Months, TARGET(),
                              ModifiedFollowing, ModifiedFollowing,
                              DateGeneration::Forward, false);

    boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(
        VanillaSwap::Payer, fixedNominal, floatingNominal, fixedSchedule,
        fixedRate, Thirty360(), floatingSchedule, iborIndex, 1.0, 0.0,
        Actual360()));

    std::vector<Date> exerciseDates(1, expiry);
    boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));
    boost::shared_ptr<NonstandardSwaption> swaption(
        new NonstandardSwaption(underlying, exercise));

    // gsr model (1% mean reversion, intially 1% vol)

    std::vector<Date> stepDates;
    std::vector<Real> vols(1, 0.006);
    std::vector<Real> reversions(1, 0.02);

    boost::shared_ptr<Gsr> gsr(
                               new Gsr(yts, stepDates, vols, reversions, 50.0));

    // engines for nonstandard swaption and standard swaption

    // this engine is used for standard swaptions used in model calibration
    boost::shared_ptr<PricingEngine> standardEngine(
        new Gaussian1dSwaptionEngine(gsr));
    // this engine is used for the non standard swaption
    boost::shared_ptr<PricingEngine> nonStandardEngine(
        new Gaussian1dNonstandardSwaptionEngine(gsr));

    swaption->setPricingEngine(nonStandardEngine);

    std::vector<boost::shared_ptr<CalibrationHelper> > basket =
        swaption->calibrationBasket(
            standardSwapBase, swaptionVol,
            BasketGeneratingEngine::MaturityStrikeByDeltaGamma);

    std::cout << "standard swaption matching 1m option on forward starting swap 1y/10y" << std::endl;
    outputBasket(basket,*yts);

}

int main(int argc, char* argv[]) {

    if(argc==1 || !strcmp(argv[1],"1")) {
        std::cout << "########################################" << std::endl;
        std::cout << "Example 1 non standard bermudan swaption" << std::endl;
        std::cout << "########################################" << std::endl;
        example01();
    }

    if(argc==1 || !strcmp(argv[1],"2")) {
        std::cout << "########################################" << std::endl;
        std::cout << "Example 2 3m - 6m - swaption" << std::endl;
        std::cout << "########################################" << std::endl;
        example02();
    }

    if(argc==1 || !strcmp(argv[1],"3")) {
        std::cout << "########################################" << std::endl;
        std::cout << "Example 3 midcurve swaption" << std::endl;
        std::cout << "########################################" << std::endl;
        example03();
    }



}
