#include <ql/quantlib.hpp>
using namespace QuantLib;

int main(int, char* []) {


    try {

      std::cout << "CmsSwaption Example - price single cms coupon in hagan pricers and mf" << std::endl << std::endl;

      Date refDate(23,May,2013);
      Date settlDate = TARGET().advance(refDate,2*Days);
      Settings::instance().evaluationDate()=refDate;

      Handle<YieldTermStructure> yts(new FlatForward(refDate,Handle<Quote>(new SimpleQuote(0.03)),
						     Actual365Fixed()));
      
      Date effDate = TARGET().advance(settlDate,20*Years);
      Date termDate = TARGET().advance(effDate,6*Months);

      Schedule sched(effDate,termDate,6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,
			  DateGeneration::Forward,false);

      boost::shared_ptr<IborIndex> iborIndex(new Euribor(6*Months));
      boost::shared_ptr<SwapIndex> swapIndexBase(new EuriborSwapIsdaFixA(10*Years,yts));     

      Handle<SwaptionVolatilityStructure> vol(new ConstantSwaptionVolatility(refDate,TARGET(),
       						  Following,   
       						  Handle<Quote>(new SimpleQuote(0.13)),
       						  Actual365Fixed()));
      
      // Handle<SwaptionVolatilityStructure> vol(new SingleSabrSwaptionVolatility(refDate,TARGET(),
      // 									       Following,
      // 									       0.10,0.95,-0.3,0.3,
      // 									       Actual365Fixed(),
      // 									       swapIndexBase));

      boost::shared_ptr<FloatFloatSwap> cmsswap(new FloatFloatSwap(VanillaSwap::Receiver,1.0,0.0,sched,swapIndexBase,
                                                     Actual360(), sched, iborIndex, Actual360()));

      std::vector<Date> volstepdates;
      std::vector<Real> vols(1,0.01);
      std::vector<Date> swaptionExpiries;
      std::vector<Period> swaptionTenors;

      swaptionExpiries.push_back(swapIndexBase->fixingDate(sched[0]));
      swaptionTenors.push_back(swapIndexBase->tenor());

      // --------------------------------
      // Output Smile Section
      // --------------------------------

      // std::vector<Real> moneygrid;
      // for(int i=0;i<=100;i++) moneygrid.push_back(0.03*i);
      // moneygrid.push_back(5.0);
      // moneygrid.push_back(10.0);
      // moneygrid.push_back(15.0);
      // moneygrid.push_back(20.0);

      // std::vector<Real> moneyGridSvi; // for svi calibration
      // //moneyGridSvi.push_back(0.1);
      // //moneyGridSvi.push_back(0.2);
      // moneyGridSvi.push_back(0.5);
      // moneyGridSvi.push_back(0.8);
      // moneyGridSvi.push_back(1.0);
      // moneyGridSvi.push_back(2.0);
      // moneyGridSvi.push_back(5.0);
      // //moneyGridSvi.push_back(10.0);
      // //moneyGridSvi.push_back(20.0);
      
      // //boost::shared_ptr<SmileSection> sec = vol->smileSection(swaptionExpiries[0],swaptionTenors[0]);
      // boost::shared_ptr<SmileSection> sec = vol->smileSection(Date(27,May,2041),1*Years);
      // boost::shared_ptr<SmileSection> sec2(new KahaleSmileSection(sec,0.0304547,false/*,moneygrid*/));
      // boost::shared_ptr<SmileSection> secsvi(new SviSmileSection(sec,Null<Real>(),moneyGridSvi));
      // std::cout << "strike;vol;call;digcall;dens;vol2;call2;digcall2;dens2;vol3;call3;digcall3;dens3" << 
      // 	  std::endl;
      // Real strike = 0.0001;
      // Real integral = 0.0000;
      // while(strike <= 3500.0000) {
      //  	// std::cout << strike << ";" << 
      // 	//   sec->volatility(strike) << ";" << 
      // 	//   sec->optionPrice(strike) << ";" << 
      // 	//   sec->digitalOptionPrice(strike) << ";" <<
      // 	//   sec->density(strike) << ";" <<
      // 	//   sec2->volatility(strike) << ";" << 
      // 	//   sec2->optionPrice(strike) << ";" << 
      // 	//   sec2->digitalOptionPrice(strike) << ";" <<
      //   //   sec2->density(strike) << ";" <<
      // 	//   secsvi->volatility(strike) << ";" << 
      // 	//   secsvi->optionPrice(strike) << ";" << 
      // 	//   secsvi->digitalOptionPrice(strike) << ";" <<
      // 	//   secsvi->density(strike) << std::endl;
      // 	integral += 0.0001 * sec2->digitalOptionPrice(strike);
      // 	strike += 0.0001;
      // }
      
      // std::cout << "integral = " << integral << std::endl;

      // return 0.0;

      // --------------------------
      // set ntl precision
      // --------------------------

      //boost::math::ntl::RR::SetPrecision(150);

      // --------------------------
      // Compute CMS Coupons
      // --------------------------

      boost::shared_ptr<MarkovFunctional> mf(new MarkovFunctional(yts,0.01,volstepdates,vols,vol,
								 swaptionExpiries,swaptionTenors,
								 swapIndexBase,
								 MarkovFunctional::ModelSettings()
								  .withYGridPoints(64)
								  .withYStdDevs(7.0)
								  .withGaussHermitePoints(32)
								  .withLowerRateBound(0.0)
								  .withUpperRateBound(2.0)
								  .withDigitalGap(1E-8)
								  .withMarketRateAccuracy(1E-10)
								  //.withAdjustments(MarkovFunctional::ModelSettings::AdjustDigitals)
								  //.withAdjustments(MarkovFunctional::ModelSettings::AdjustYts)
								  //.withAdjustments(MarkovFunctional::ModelSettings::NoPayoffExtrapolation)
								  ));

      boost::shared_ptr<Exercise> exercise(new EuropeanExercise(swaptionExpiries[0]));
      boost::shared_ptr<FloatFloatSwaption> cmsswaption(new FloatFloatSwaption(cmsswap,exercise));

      boost::shared_ptr<PricingEngine> cmsswaptionEngine(new Gaussian1dFloatFloatSwaptionEngine(mf,64,10.0,true,false));
      
      cmsswaption->setPricingEngine(cmsswaptionEngine);
      
      boost::shared_ptr<CmsCoupon> cmsCoupon=boost::dynamic_pointer_cast<CmsCoupon>(cmsswap->leg(0)[0]);
      boost::shared_ptr<CmsCouponPricer> hagan1(new AnalyticHaganPricer(vol,GFunctionFactory::NonParallelShifts,Handle<Quote>(new SimpleQuote(0.02))));
      boost::shared_ptr<CmsCouponPricer> hagan2(new NumericHaganPricer(vol,GFunctionFactory::NonParallelShifts,
      							   Handle<Quote>(new SimpleQuote(0.01)),
      							   0.0,1.5,1E-4));
      boost::shared_ptr<CmsReplicationPricer> repl(
			 new CmsReplicationPricer(vol,Handle<Quote>(new SimpleQuote(0.01)),yts,
			 CmsReplicationPricer::Settings()
						  .withRateBound(200,0.0000,5.0000)
						  //.withVegaRatio(200,0.001,0.0001,5.0000)
						  //.withPriceThreshold(200,1E-6,0.0001,2.0000)
						  //.withMonotonicPrices(true)
						  ));

      Real accrualTime = cmsCoupon->accrualPeriod();


      std::cout << "coupon fixing " << cmsCoupon->fixingDate() << " payment " << cmsCoupon->date() 
                << " accrualTime " << cmsCoupon->accrualPeriod() << std::endl;

      cmsCoupon->setPricer(repl);
      std::cout << "rate hagan repl      = " << cmsCoupon->amount() / accrualTime << std::endl;
      cmsCoupon->setPricer(hagan1);
      std::cout << "rate hagan analytic  = " << cmsCoupon->amount() / accrualTime << std::endl;
      cmsCoupon->setPricer(hagan2);
      std::cout << "rate hagan numerical = " << cmsCoupon->amount() / accrualTime << std::endl;

      Real mfNpv = cmsswaption->result<Real>("underlyingValue");
      Real mfAmount = mfNpv / yts->discount(cmsCoupon->date());
      Real mfRate = mfAmount / accrualTime;

      std::cout << "rate mf              = " << mfRate << std::endl;

      // -----------------------------------------------------
      // output model details
      // -----------------------------------------------------

      //std::cout << "Model outputs " << std::endl << mf->modelOutputs() << std::endl << std::endl;

      // -----------------------------------------------------
      // generate basket
      // -----------------------------------------------------
      
      std::vector<boost::shared_ptr<CalibrationHelper>> basket = 
          cmsswaption->calibrationBasket(swapIndexBase,*vol,BasketGeneratingEngine::Naive);


    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }

}

