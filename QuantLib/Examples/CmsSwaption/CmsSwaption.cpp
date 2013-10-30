#include <ql/quantlib.hpp>
using namespace QuantLib;

int main(int, char* []) {


    try {

      Date refDate(13,November,2013);
      Date settlDate = TARGET().advance(refDate,2*Days);
      Settings::instance().evaluationDate()=refDate;

      Handle<Quote> rateLevel(new SimpleQuote(0.03));
      Handle<YieldTermStructure> yts(new FlatForward(refDate,rateLevel,
						     Actual365Fixed()));

      Handle<Quote> volatilityLevel(new SimpleQuote(0.30));

      Handle<Quote>  reversionLevel(new SimpleQuote(0.02));

      boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months,yts));
      boost::shared_ptr<SwapIndex> swapIndex(new EuriborSwapIsdaFixA(10*Years,yts));   
      
      iborIndex->addFixing(refDate,0.0200);
      swapIndex->addFixing(refDate,0.0315);

      // Handle<SwaptionVolatilityStructure> swaptionVol(new ConstantSwaptionVolatility(refDate,TARGET(),
      //  						  Following,   
      //  						  Handle<Quote>(new SimpleQuote(0.13)),
      //  						  Actual365Fixed()));

      Handle<SwaptionVolatilityStructure> swaptionVol(new SingleSabrSwaptionVolatility(refDate,TARGET(),
      									       Following,
                                               0.10,0.80,-0.20,0.080,
                                               Actual365Fixed(),
      									       swapIndex));

      Date termDate = TARGET().advance(settlDate,10*Years);
      
      Schedule sched1(settlDate,termDate,1*Years,TARGET(),ModifiedFollowing,ModifiedFollowing,
                      DateGeneration::Forward,false);
      Schedule sched2(settlDate,termDate,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
			  DateGeneration::Forward,false);

      Real nominal = 100000.0;

      boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap(VanillaSwap::Payer,
                                                                   nominal,nominal,
                                                                   sched1,swapIndex,Thirty360(),
                                                                   sched2,iborIndex,Actual360()));

      boost::shared_ptr<NumericHaganPricer> haganPricer(new NumericHaganPricer(swaptionVol,
                                                                               GFunctionFactory::Standard,
                                                                               reversionLevel));
      setCouponPricer(cmsswap->leg(0), haganPricer);
      
      std::vector<Date> exerciseDates;
      std::vector<Date> sigmaSteps;
      std::vector<Real> sigma;
      
      sigma.push_back(0.01);
      for(Size i=1;i<sched1.size()-1;i++) {
          exerciseDates.push_back(swapIndex->fixingDate(sched1[i]));
          sigmaSteps.push_back(exerciseDates.back());
          sigma.push_back(0.01);
      }

      boost::shared_ptr<FloatFloatSwap> underlying(new FloatFloatSwap(VanillaSwap::Receiver,
                                                                   nominal,nominal,
                                                                   sched1,swapIndex,Thirty360(),
                                                                   sched2,iborIndex,Actual360()));

      boost::shared_ptr<Exercise> exercise(new BermudanExercise(exerciseDates));
      boost::shared_ptr<FloatFloatSwaption> callRight(new FloatFloatSwaption(underlying,exercise));

      std::vector<Date> cmsFixingDates(exerciseDates);
      std::vector<Period> cmsTenors(exerciseDates.size(),10*Years);

      boost::shared_ptr<MarkovFunctional> mf(new MarkovFunctional(
          yts, reversionLevel->value(), sigmaSteps, sigma, swaptionVol,
          cmsFixingDates, cmsTenors, swapIndex,
          MarkovFunctional::ModelSettings().withYGridPoints(16)
                                           .withYStdDevs(5.0)
                                           .withGaussHermitePoints(16)
          .withUpperRateBound(2.0)
          .withAdjustments(MarkovFunctional::ModelSettings::SabrSmile)));

      boost::shared_ptr<Gaussian1dFloatFloatSwaptionEngine> floatEngine(new Gaussian1dFloatFloatSwaptionEngine(mf));

      callRight->setPricingEngine(floatEngine);

      boost::shared_ptr<SwapIndex> swapBase(new EuriborSwapIsdaFixA(30*Years,yts));
      std::vector<boost::shared_ptr<CalibrationHelper> > basket = 
          callRight->calibrationBasket(swapBase,*swaptionVol,BasketGeneratingEngine::Naive);

      boost::shared_ptr<Gaussian1dSwaptionEngine> stdEngine(new Gaussian1dSwaptionEngine(mf));
      for(Size i=0;i<basket.size();i++) basket[i]->setPricingEngine(stdEngine);
      
      LevenbergMarquardt opt;
      EndCriteria ec(2000,500,1E-8,1E-8,1E-8);
      mf->calibrate(basket,opt,ec);

      std::cout << "model vol \t swaption market \t swaption model " << std::endl;
      for(Size i=0;i<basket.size();i++) {
          std::cout << mf->volatilities()[i] << "\t" << basket[i]->marketValue() << "\t" << basket[i]->modelValue() << std::endl;
      }
      std::cout << mf->volatilities().back() << std::endl;

      Real analyticSwapNpv = CashFlows::npv(cmsswap->leg(1), **yts, false) - CashFlows::npv(cmsswap->leg(0), **yts, false);
      Real callRightNpv = callRight->NPV();
      Real firstCouponNpv = cmsswap->leg(0)[0]->amount() * yts->discount(cmsswap->leg(0)[0]->date()) - cmsswap->leg(1)[0]->amount() * yts->discount(cmsswap->leg(1)[0]->date());
      Real underlyingNpv = callRight->result<Real>("underlyingValue") + firstCouponNpv;

      std::cout << "Swap Npv (Hagan)   : " << analyticSwapNpv << std::endl;
      std::cout << "Call Right Npv (MF): " << callRightNpv << std::endl;
      std::cout << "Underlying Npv (MF): " << underlyingNpv << std::endl;
      
      std::cout << "Model trace : " << std::endl << mf->modelOutputs() << std::endl;
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }

}

