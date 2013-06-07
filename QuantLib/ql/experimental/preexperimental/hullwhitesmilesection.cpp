#include <hullwhitesmilesection.hpp>

namespace QuantLib {

    HullWhiteSmileSection::HullWhiteSmileSection(const Date& optionDate, const Period& swapLength, boost::shared_ptr<SwapIndex> swapIndex,
			const Handle<YieldTermStructure>& yts, const Real reversion, const Real sigma) : 
						SmileSection(optionDate,yts->dayCounter(),yts->referenceDate()), 
							optionDate_(optionDate), swapLength_(swapLength), swapIndex_(swapIndex), yts_(yts), reversion_(reversion), sigma_(sigma) {

        //std::cout << "model reversion = " << reversion << " sigma=" << sigma << std::endl;
		model_ = boost::shared_ptr<HullWhite>(new HullWhite(yts_,reversion,sigma));
		jamshidianEngine_ = boost::shared_ptr<JamshidianSwaptionEngine>(new JamshidianSwaptionEngine(model_));
		discountingEngine_ = boost::shared_ptr<DiscountingSwapEngine>(new DiscountingSwapEngine(yts_));
		boost::shared_ptr<VanillaSwap> underlying = MakeVanillaSwap(swapLength_,swapIndex_->iborIndex(),0.03) // dummy strike
																	.withEffectiveDate(swapIndex_->valueDate(optionDate_))  
																	.withFixedLegCalendar(swapIndex_->fixingCalendar())
													                .withFixedLegDayCount(swapIndex_->dayCounter())
													                .withFixedLegTenor(swapIndex_->fixedLegTenor())
													                .withFixedLegConvention(swapIndex_->fixedLegConvention())
													                .withFixedLegTerminationDateConvention(swapIndex_->fixedLegConvention());																
		underlying->setPricingEngine(discountingEngine_);
		atm_ = underlying->fairRate();
		//std::cout << "SECTION option date " << optionDate << " tenor " << swapLength << std::setprecision(8) << " atm " << atm_ << std::endl;

    }

     Real HullWhiteSmileSection::volatilityImpl(Rate strike) const {
		
		boost::shared_ptr<VanillaSwap> underlying = MakeVanillaSwap(swapLength_,swapIndex_->iborIndex(),strike)
																	.withEffectiveDate(swapIndex_->valueDate(optionDate_))  
																	.withFixedLegCalendar(swapIndex_->fixingCalendar())
													                .withFixedLegDayCount(swapIndex_->dayCounter())
													                .withFixedLegTenor(swapIndex_->fixedLegTenor())
													                .withFixedLegConvention(swapIndex_->fixedLegConvention())
													                .withFixedLegTerminationDateConvention(swapIndex_->fixedLegConvention())
																	.withType(VanillaSwap::Payer);
		underlying->setPricingEngine(discountingEngine_);
		boost::shared_ptr<Exercise> exercise(new EuropeanExercise(optionDate_));
		Swaption swaption(underlying,exercise);
		swaption.setPricingEngine(jamshidianEngine_);
		Real npv = swaption.NPV();
		Real annuity = fabs(underlying->fixedLegBPS() * 10000.0);

		//Real blackNpv=blackFormula(Option::Call,strike,atm,0.30*sqrt(exerciseTime()));
		//std::cout << std::setprecision(8) << "SECTION atm=" << atm_ << " annuity=" << annuity << " npv=" << npv << " strike= " << strike << std::endl;

		Real vol=0.0;
		try {
			vol=blackFormulaImpliedStdDev(Option::Call,strike,atm_,npv,annuity) / sqrt(exerciseTime());
		} catch(QuantLib::Error) {
			//std::cout << " can not imply vol for npv " << npv << " black(300%)=" << blackFormula(Option::Call,strike,atm_,sqrt(exerciseTime())*3.00,annuity) << 
			//	" black(1%)=" << blackFormula(Option::Call,strike,atm_,sqrt(exerciseTime())*0.01,annuity) << std::endl;
			if(npv>blackFormula(Option::Call,strike,atm_,3.00,annuity)) vol =3.0/sqrt(exerciseTime());
			else vol=0.0;
		}
		return vol;

     }

}
