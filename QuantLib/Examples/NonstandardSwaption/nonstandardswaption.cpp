#include <ql/quantlib.hpp>

#include <iostream>
#include <fstream>

using namespace QuantLib;

void outputBasket(std::vector<boost::shared_ptr<CalibrationHelper>> basket,
                  boost::shared_ptr<TermStructure> termStructure) {

	std::cout << "Calibration Basket:" << std::endl;
    std::cout << "expiry;maturityDate;expiryTime;maturityTime;nominal;rate;marketvol" << std::endl;
    for(Size j=0;j<basket.size();j++) {
        boost::shared_ptr<SwaptionHelper> helper = boost::dynamic_pointer_cast<SwaptionHelper>(basket[j]);
        Date endDate = helper->underlyingSwap()->fixedSchedule().dates().back();
        Real nominal = helper->underlyingSwap()->nominal();
        Real vol = helper->volatility();
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
        std::cout << expiries[i] << " & " << model->params()[i] << " \\\\" << std::endl;
    }
    std::cout << std::endl;

}

int main(int, char* []) {
    
    Date refDate(15,June,2013);
    Settings::instance().evaluationDate() = refDate;
	Date effective = TARGET().advance(refDate,2*Days);
	Date maturity = TARGET().advance(effective,10*Years);

    // market data: flat yts 3%, flat vol 20%
    
    Real rateLevel = 0.03;
    Real volLevel = 0.20;

    boost::shared_ptr<Quote> ytsQuote0(new SimpleQuote(rateLevel));
    boost::shared_ptr<Quote> ytsQuote1(new SimpleQuote(rateLevel+0.0010)); // 10bp shift for dv01 calculation
    
    RelinkableHandle<Quote> ytsQuote(ytsQuote0);

    Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(0,TARGET(),ytsQuote,
                                                                                              Actual365Fixed())));
	boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(new ConstantSwaptionVolatility(0,TARGET(),
                                                                         ModifiedFollowing,volLevel,Actual365Fixed()));
   
	boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months,yts));
	boost::shared_ptr<SwapIndex> standardSwapBase(new EuriborSwapIsdaFixA(10*Years,yts));

    // non standard swaption instrument (10y, amortizing nominal and step up coupon, yearly exercise dates)

    std::vector<Real> fixedNominal(10), floatingNominal(20), fixedRate(10);
	for(Size i=0;i<10;i++) {
		fixedNominal[i] = 100.0-i*10.0;//-i*5000000;//(i>0 ? fixedNominal[i-1] : 100000000.0 )*1.075;
		floatingNominal[2*i] = floatingNominal[2*i+1] = fixedNominal[i];
		fixedRate[i] = 0.035;//+0.0030*i;
	}

	Schedule fixedSchedule(effective,maturity,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);
	Schedule floatingSchedule(effective,maturity,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);

	boost::shared_ptr<NonstandardSwap> underlying(new NonstandardSwap(NonstandardSwap::Payer,fixedNominal,floatingNominal,
                                                                      fixedSchedule,fixedRate,Thirty360(),floatingSchedule,
                                                                      iborIndex,0.0,Actual360()));
    
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
    boost::shared_ptr<PricingEngine> standardEngine(new GsrSwaptionEngine(gsr));
    // this engine is used for the non standard swaption
    boost::shared_ptr<PricingEngine> nonStandardEngine(new GsrNonstandardSwaptionEngine(gsr));

    swaption->setPricingEngine(nonStandardEngine);

    std::cout.precision(6);
    std::cout << std::fixed;

    std::cout << "Model is not calibrated" << std::endl;
    outputModel(stepDates,gsr);

    std::cout << "Calculate calibration basket" << std::endl;
    std::vector<boost::shared_ptr<CalibrationHelper>> basket = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket.size();i++) basket[i]->setPricingEngine(standardEngine);
    outputBasket(basket,*gsr->termStructure());

    std::cout << "Calibrate the model to the initial basket" << std::endl;
    LevenbergMarquardt lm;
    EndCriteria ec(2000,200,1E-8,1E-8,1E-8);
    gsr->calibrateIterative(basket,lm,ec);
    outputModel(stepDates,gsr);

    std::cout << "Calculate calibration basket" << std::endl;

    std::vector<boost::shared_ptr<CalibrationHelper>> basket2 = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket2.size();i++) basket2[i]->setPricingEngine(standardEngine);
    outputBasket(basket2,*gsr->termStructure());

    std::cout << "Calibrate the model to the second basket" << std::endl;

    gsr->calibrateIterative(basket2,lm,ec);
    outputModel(stepDates,gsr);

    std::cout << "Price the nonstandard swaption" << std::endl;

    Real npv0 = swaption->NPV();
    std::cout << "NPV              = " << npv0 << std::endl;

    std::cout << "Shift the rate curve by 10bp and reprice to compute delta" << std::endl;

    ytsQuote.linkTo(ytsQuote1);
    Real npv1 = swaption->NPV();
    std::cout << "NPV(+10bp)       = " << npv1 << " DV01 = " << (npv1-npv0) << std::endl;

    std::cout << "Recalibrate the model, reprice and compute delta" << std::endl;

    gsr->calibrateIterative(basket2,lm,ec);
    outputModel(stepDates,gsr);
    Real npv2 = swaption->NPV();
    std::cout << "NPV(+10bp,recal) = " << npv2 << " DV01 = " << (npv2-npv0) << std::endl;

    std::cout << "Recompute calibration basket, reclibrate to that basket and compute delta" << std::endl;

    std::vector<boost::shared_ptr<CalibrationHelper>> basket3 = swaption->calibrationBasket(standardSwapBase,swaptionVol);
	for(Size i=0;i<basket2.size();i++) basket3[i]->setPricingEngine(standardEngine);
    outputBasket(basket3,*gsr->termStructure());
    gsr->calibrateIterative(basket3,lm,ec);
    outputModel(stepDates,gsr);
    Real npv3 = swaption->NPV();
    std::cout << "NPV(+10bp,recompBasket) = " << npv3 << " DV01 = " << (npv3-npv0) << std::endl;
   


}

