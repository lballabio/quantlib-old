#include <ql/quantlib.hpp>

using namespace QuantLib;

void example01() {

    Date refDate(17,June,2013); 
    Settings::instance().evaluationDate() = refDate;
	Date effective = TARGET().advance(refDate,2*Days);
	Date maturity = TARGET().advance(effective,10*Years);

    // market data: flat yts 3%, flat vol 20%
    
    Real rateLevel = 0.03;
    Real rateLevel2 = 0.01;
    Real volLevel = 0.20;

    Handle<Quote> ytsQuote(boost::shared_ptr<Quote>(new SimpleQuote(rateLevel)));
    Handle<Quote> ytsQuote2(boost::shared_ptr<Quote>(new SimpleQuote(rateLevel2)));
    
    Handle<YieldTermStructure> yts( boost::shared_ptr<YieldTermStructure>(new FlatForward(refDate,ytsQuote,
                                                                                              Actual365Fixed())));
    Handle<YieldTermStructure> yts2( boost::shared_ptr<YieldTermStructure>(new FlatForward(refDate,ytsQuote2,
                                                                                              Actual365Fixed())));

    Handle<Quote> volQuote(boost::shared_ptr<Quote>(new SimpleQuote(volLevel)));

	boost::shared_ptr<SwaptionVolatilityStructure> swaptionVol(new ConstantSwaptionVolatility(refDate, TARGET(),
                                                          ModifiedFollowing,volQuote,Actual365Fixed()));
   
	boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months,yts));
	boost::shared_ptr<SwapIndex> standardSwapBase(new EuriborSwapIsdaFixA(10*Years,yts,yts2));
    

    // 10y swaption yearly callable

	Schedule fixedSchedule(effective,maturity,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);
	Schedule floatingSchedule(effective,maturity,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
                           DateGeneration::Forward,false);

    boost::shared_ptr<VanillaSwap> underlying(new VanillaSwap(VanillaSwap::Payer, 100000.0, fixedSchedule, 0.02,
                                                              Thirty360(), floatingSchedule, iborIndex, 0.00, Actual360()));

    std::vector<Date> exerciseDates;
    std::vector<Period> exerciseTenors;
	for(Size i=1;i<10;i++) { // < 10
        exerciseDates.push_back(TARGET().advance(fixedSchedule[i],-5*Days));
        exerciseTenors.push_back( (10-i)*Years );
    }

    boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));

	boost::shared_ptr<Swaption> swaption(new Swaption(underlying,exercise));

    // gsr model (1% mean reversion, 1% vol)

    std::vector<Date> stepDates;
    std::vector<Real> vols(1,0.01);
    std::vector<Real> reversions(1,0.01);

    boost::shared_ptr<Gsr> gsr(new Gsr(yts2,stepDates,vols,reversions,60));

    // hull white model (1% mean reversion, 1% vol)
    
    boost::shared_ptr<HullWhite> hullwhite(new HullWhite(yts2,0.01,0.01));

    // markov model

    // boost::shared_ptr<MarkovFunctional> mf(new MarkovFunctional(yts2, 0.01, stepDates, vols, Handle<SwaptionVolatilityStructure>(swaptionVol), exerciseDates, exerciseTenors,standardSwapBase));

    // pricing engines

    //boost::math::ntl::RR::SetPrecision(128);

    boost::shared_ptr<PricingEngine> gsrEngine(new Gaussian1dSwaptionEngine(gsr,64,7.0,true,false,
                                                                            Handle<YieldTermStructure>(),
                                                                            Gaussian1dSwaptionEngine::Digital));
    // boost::shared_ptr<PricingEngine> mfEngine(new Gaussian1dSwaptionEngine(mf,64,7.0,true,false));
    // boost::shared_ptr<TreeSwaptionEngine> treeEngine(new TreeSwaptionEngine(hullwhite,512,yts));
    boost::shared_ptr<FdHullWhiteSwaptionEngine> fdEngine(new FdHullWhiteSwaptionEngine(hullwhite));

    // swaption->setPricingEngine(treeEngine);
    // Real hullwhiteNpv = swaption->NPV();
    swaption->setPricingEngine(fdEngine);
    Real fdNpv = swaption->NPV();
    // swaption->setPricingEngine(mfEngine);
    // Real mfNpv = swaption->NPV();
    swaption->setPricingEngine(gsrEngine);
    Real gsrNpv = swaption->NPV();

    
    std::cout.precision(12);
    std::cout << std::fixed;
    std::cout << "GSR     " << gsrNpv << std::endl;
    // std::cout << "HW tree " << hullwhiteNpv << std::endl;
    std::cout << "FD      " << fdNpv << std::endl;
    // std::cout << "MF      " << mfNpv << " numeraire time is " << mf->numeraireTime() << std::endl;

    std::vector<Real> prob = swaption->result<std::vector<Real> >("probabilities");
    std::cout << "PROBS (GSR):" << std::endl;
    Real sum = 0.0;
    for(Size i=0;i<prob.size();i++) {
        std::cout << "#" << (i+1) << " : " << prob[i] << std::endl;
        sum += prob[i];
    }
    std::cout << "SUM = " << sum << std::endl;

}

int main(int argc, char* argv[]) {

    if(argc==1 || !strcmp(argv[1],"1")) {
        std::cout << "############################################" << std::endl;
        std::cout << "bermudan swaption pricing engine comparision" << std::endl;
        std::cout << "############################################" << std::endl;
        example01();
    }


}

