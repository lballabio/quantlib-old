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

      /*Handle<SwaptionVolatilityStructure> vol(new ConstantSwaptionVolatility(refDate,TARGET(),
						  Following,   
						  Handle<Quote>(new SimpleQuote(0.20)),
						  Actual365Fixed()));
      */
      Handle<SwaptionVolatilityStructure> vol(new SingleSabrSwaptionVolatility(refDate,TARGET(),
									       Following,
									       0.10,0.95,-0.3,0.3,
									       Actual365Fixed(),
									       swapIndexBase));


      boost::shared_ptr<CmsSwap> cmsswap(new CmsSwap(CmsSwap::Payer,1.0,sched,swapIndexBase,
						     0.0,Null<Real>(),Null<Real>(),Actual360(),
						     sched,iborIndex,0.0,Actual360()));

      std::vector<Date> volstepdates;
      std::vector<Real> vols(1,0.01);
      std::vector<Date> swaptionExpiries;
      std::vector<Period> swaptionTenors;

      swaptionExpiries.push_back(swapIndexBase->fixingDate(sched[0]));
      swaptionTenors.push_back(swapIndexBase->tenor());

      // test output smile
      /*std::vector<Real> moneygrid;
      for(int i=0;i<=100;i++) moneygrid.push_back(0.03*i);
      moneygrid.push_back(5.0);
      moneygrid.push_back(10.0);
      moneygrid.push_back(15.0);
      moneygrid.push_back(20.0);

      boost::shared_ptr<SmileSection> sec = vol->smileSection(swaptionExpiries[0],swaptionTenors[0]);
      boost::shared_ptr<SmileSection> sec2(new KahaleSmileSection(sec,Null<Real>(),false,moneygrid));
      std::cout << "strike;vol;call;digcall;dens;vol2;call2;digcall2;dens2" << std::endl;
      Real strike = 0.0001;
      while(strike <= 0.1000) {
	std::cout << strike << ";" << 
	  sec->volatility(strike) << ";" << 
	  sec->optionPrice(strike) << ";" << 
	  sec->digitalOptionPrice(strike) << ";" <<
	  sec->density(strike) << ";" <<
	  sec2->volatility(strike) << ";" << 
	  sec2->optionPrice(strike) << ";" << 
	  sec2->digitalOptionPrice(strike) << ";" <<
	  sec2->density(strike) << std::endl;
	strike += 0.0010;
      }
      
      return 0;*/

      // Compute CMS Coupons
  
      boost::shared_ptr<MarkovFunctional> mf(new MarkovFunctional(yts,0.01,volstepdates,vols,vol,
								 swaptionExpiries,swaptionTenors,
								 swapIndexBase,
								 MarkovFunctional::ModelSettings()
								  .withYGridPoints(64)
								  .withYStdDevs(8.0)
								  .withGaussHermitePoints(32)
								  .withUpperRateBound(2.0)
								  //.withAdjustments(MarkovFunctional::ModelSettings::AdjustYts)
								  ));

      boost::shared_ptr<Exercise> exercise(new EuropeanExercise(swaptionExpiries[0]));
      boost::shared_ptr<CmsSwaption> cmsswaption(new CmsSwaption(cmsswap,exercise));

      boost::shared_ptr<PricingEngine> cmsswaptionEngine(new MarkovFunctionalCmsSwaptionEngine(mf,64,8.0,true,false));
      
      cmsswaption->setPricingEngine(cmsswaptionEngine);
      
      boost::shared_ptr<CmsCoupon> cmsCoupon=boost::dynamic_pointer_cast<CmsCoupon>(cmsswap->structuredLeg()[0]);
      boost::shared_ptr<CmsCouponPricer> hagan1(new AnalyticHaganPricer(vol,GFunctionFactory::NonParallelShifts,Handle<Quote>(new SimpleQuote(0.02))));
      boost::shared_ptr<CmsCouponPricer> hagan2(new NumericHaganPricer(vol,GFunctionFactory::NonParallelShifts,
      							   Handle<Quote>(new SimpleQuote(0.01)),
      							   0.0,1.0,1E-6));
      Real accrualTime = cmsCoupon->accrualPeriod();

      //std::cout << "Model outputs " << std::endl << mf->modelOutputs() << std::endl << std::endl;
      std::cout << "coupon fixing " << cmsCoupon->fixingDate() << " payment " << cmsCoupon->date() << " accrualTime " << cmsCoupon->accrualPeriod() << std::endl;

      cmsCoupon->setPricer(hagan1);
      std::cout << "rate hagan analytic  = " << cmsCoupon->amount() / accrualTime << std::endl;
      cmsCoupon->setPricer(hagan2);
      std::cout << "rate hagan numerical = " << cmsCoupon->amount() / accrualTime << std::endl;

      Real mfNpv = cmsswaption->result<Real>("underlyingValue");
      Real mfAmount = mfNpv / yts->discount(cmsCoupon->date());
      Real mfRate = mfAmount / accrualTime;

      std::cout << "rate mf    = " << mfRate << std::endl;

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

