#include <ql/quantlib.hpp>

#include <boost/make_shared.hpp>
#include <iostream>
#include <fstream>

using namespace QuantLib;

void outputBasket(std::vector<boost::shared_ptr<CalibrationHelper> > basket,
                  boost::shared_ptr<TermStructure> termStructure) {

    std::cout << "Calibration Basket:" << std::endl;
    std::cout << "expiry;maturityDate;expiryTime;maturityTime;nominal;rate;marketvol" << std::endl;
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

int main(int, char* []) {

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

    // spread Quote

    Real spreadLevel = 0.0100;
    boost::shared_ptr<Quote> spreadQuote0(new SimpleQuote(spreadLevel));

    RelinkableHandle<Quote> spreadQuote(spreadQuote0);

    // non standard swaption instrument (10y, amortizing nominal and step up coupon, yearly exercise dates)
    // we use the nonstandard swap and the nonstandard swaption for bond pricing, i.e. set the floating side to zero
    // receiver swap means we are long the bond
    // we are short the call right, so the rebate is positive

    std::vector<Real> fixedNominal(10), floatingNominal(20,0.0), fixedRate(10);
    for(Size i=0;i<10;i++) {
        fixedNominal[i] = 100.0;//-i*10.0;//-i*5000000;//(i>0 ? fixedNominal[i-1] : 100000000.0 )*1.075;
        floatingNominal[2*i] = floatingNominal[2*i+1] = 0.0; //fixedNominal[i];
        fixedRate[i] = 0.035;//+0.0030*i;
    }

    Schedule fixedSchedule(effective,maturity,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);
    Schedule floatingSchedule(effective,maturity,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);

    boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(VanillaSwap::Receiver,fixedNominal,floatingNominal,
                                                                      fixedSchedule,fixedRate,Thirty360(),floatingSchedule,
                                                                      iborIndex,1.0,0.0,Actual360(),true,true));

    std::vector<Date> exerciseDates;
    std::vector<Real> rebates;
    for(Size i=1;i<10;i++) {
        exerciseDates.push_back(TARGET().advance(fixedSchedule[i],-2*Days));
        rebates.push_back(-100.0);
    }

    //BermudanExercise exerciseTmp(exerciseDates, false);
    boost::shared_ptr<RebatedExercise> exercise =
        boost::make_shared<RebatedExercise>(
            BermudanExercise(exerciseDates, false), rebates, 2, TARGET(),
            Following);

    boost::shared_ptr<NonstandardSwaption> swaption(
        new NonstandardSwaption(underlying, exercise));

    // pricing of vanilla bond part

    std::cout << "============================================================" << std::endl;
    std::cout << "Vanilla part pricing" << std::endl;
    std::cout << "============================================================" << std::endl;

    Leg vanilla = underlying->leg(0);
    Leg other = underlying->leg(1);

    Real vanillaNpv = CashFlows::npv(vanilla,*yts,spreadQuote->value(),yts->dayCounter(),Continuous,NoFrequency,false);
    Real otherNpv = CashFlows::npv(other,*yts,spreadQuote->value(),yts->dayCounter(),Continuous,NoFrequency,false);

    std::cout << "npv = " << vanillaNpv << " (other = " << otherNpv << ")" << std::endl;

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
    boost::shared_ptr<PricingEngine> nonStandardEngine(new Gaussian1dNonstandardSwaptionEngine(gsr,64,7.0,true,false,
                                                                                        spreadQuote));

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



}
