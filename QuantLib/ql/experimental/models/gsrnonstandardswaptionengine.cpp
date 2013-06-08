/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ql/experimental/models/gsrnonstandardswaptionengine.hpp>
#include <ql/utilities/disposable.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/time/daycounters/actualactual.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>

//#include <iostream> // only for debug

namespace QuantLib {

	Disposable<std::vector<boost::shared_ptr<CalibrationHelper>>> GsrNonstandardSwaptionEngine::calibrationBasket(boost::shared_ptr<SwapIndex> standardSwapBase, 
		boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolatility,
		const NonstandardSwaption::CalibrationBasketType basketType ) const {

		QL_REQUIRE(!standardSwapBase->forwardingTermStructure().empty(), "standard swap base forwarding term structure must not be empty.");
		QL_REQUIRE(!standardSwapBase->exogenousDiscount() || !standardSwapBase->discountingTermStructure().empty(), "standard swap base discounting term structure must not be empty.");

		std::vector<boost::shared_ptr<CalibrationHelper>> result;

		for(Size i=0; i< arguments_.exercise->dates().size() ; i++) {

			Date expiry = arguments_.exercise->date(i);

			// determine the indices on both legs representing the cashflows that are part of the exercise into right

			Size fixedIdx = std::upper_bound(arguments_.fixedResetDates.begin(), arguments_.fixedResetDates.end(), expiry-1 ) - arguments_.fixedResetDates.begin();
			Size floatingIdx = std::upper_bound(arguments_.floatingResetDates.begin(), arguments_.floatingResetDates.end(), expiry-1 ) - arguments_.floatingResetDates.begin();
			Real type = (Real)arguments_.type;

			if(fixedIdx < arguments_.fixedResetDates.size()) { // do nothing if no cashflows on fixed leg are present

				//std::cout << "  Tail Fixed Leg: "; for(Size i=fixedIdx;i<arguments_.fixedResetDates.size();i++) std::cout << arguments_.fixedPayDates[i] << " "; std::cout << std::endl;
				//std::cout << "  Tail Float Leg: "; for(Size i=floatingIdx;i<arguments_.floatingResetDates.size();i++) std::cout << arguments_.floatingPayDates[i] << " "; std::cout << std::endl;

				// determine the npv, first and second order derivatives at $y=0$ of the underlying swap (which we normalize to be a payer swap)
			
				const Real h = 0.0001; 			// finite difference step in $y$, make this a parameter of the engine ?
				Real npvm = type*(floatingLegNpv(floatingIdx,expiry,-h) - fixedLegNpv(fixedIdx,expiry,-h));
				Real npv = type*(floatingLegNpv(floatingIdx,expiry,0.0) - fixedLegNpv(fixedIdx,expiry,0.0));
				Real npvp = type*(floatingLegNpv(floatingIdx,expiry,h) - fixedLegNpv(fixedIdx,expiry,h));

				Real delta = (npvp-npvm)/(2.0*h);
				Real gamma = (npvp-2.0*npv+npvm)/(h*h);

				// debug
				/*std::cout << " SOURCE SWAP:  NPV = " << npv << "  Delta = " << delta << "  Gamma = " << gamma << std::endl;
				for(Real y = -5.0 ; y <= 5.0001; y+=0.1) {
					std::cout << floatingLegNpv(floatingIdx,expiry,y) - fixedLegNpv(fixedIdx,expiry,y) << ";";
				}
				std::cout << std::endl;*/
				// end debug

				class MatchHelper : public CostFunction {

				public:

					MatchHelper(const NonstandardSwap::Type type, const Real npv, const Real delta, const Real gamma, const Handle<Gsr>& model, const boost::shared_ptr<SwapIndex> indexBase, const Date& expiry, const Real h) : 
							type_(type), mdl_(model), indexBase_(indexBase), expiry_(expiry), npv_(npv), delta_(delta), gamma_(gamma), h_(h) {}

					Real NPV(boost::shared_ptr<VanillaSwap> swap, Real fixedRate, Real nominal, Real y, int type) const {
						Real npv=0.0;
						for(Size i=0; i < swap->fixedLeg().size() ; i++) {
							boost::shared_ptr<FixedRateCoupon> c = boost::dynamic_pointer_cast<FixedRateCoupon>(swap->fixedLeg()[i]);
							npv -= fixedRate * c->accrualPeriod() * nominal * mdl_->zerobond(c->date(),expiry_,y);
						}
						for(Size i=0; i < swap->floatingLeg().size() ; i++) {
							boost::shared_ptr<IborCoupon> c = boost::dynamic_pointer_cast<IborCoupon>(swap->floatingLeg()[i]);
							npv += mdl_->forwardRate(c->fixingDate(),c->iborIndex(),expiry_,y) * c->accrualPeriod() * nominal * mdl_->zerobond(c->date(),expiry_,y);
						}
						return (Real)type*npv;
					}

					Real value(const Array &v) const {
						Array vals = values(v);
						Real res=0.0;
						for(Size i=0;i<vals.size();i++) {
							res += vals[i]*vals[i];
						}
						return std::sqrt(res/vals.size());
					}

					Disposable<Array> values(const Array &v) const {
						// transformations
						int type = type_; // start with same type as non standard underlying (1 means payer, -1 receiver)
						Real nominal = fabs(v[0]);
						if(v[0] < 0.0) type *=-1;
						Real maturity = fabs(v[1]);
						Real fixedRate = fabs(v[2]);
						if(v[2] < 0.0) type *=-1;
						Size years = (Size)std::floor(maturity);
						maturity -= (Real)years; maturity *= 12.0;
						Size months = (Size)std::floor(maturity);
						Real alpha = 1.0-(maturity-(Real)months);
						if(years==0 && months==0) {
							months=1; // ensure a maturity of at least one months ...
							alpha=1.0; // ... but in this case only look at the lower maturity swap
						}
						//maturity -= (Real)months; maturity *= 365.25;
						//Size days = (Size)std::floor(maturity);
						//Real alpha = 1.0-(maturity-(Real)days);
						// generate swap
						Period lowerPeriod = years*Years+months*Months;//+days*Days;
						Period upperPeriod = lowerPeriod + 1*Months; // 1*Days;
						//std::cout << "lower period = " << lowerPeriod << " upper period = " << upperPeriod << std::endl;
						boost::shared_ptr<SwapIndex> tmpIndexLower, tmpIndexUpper;
						if(indexBase_->exogenousDiscount()) {
							tmpIndexLower = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),lowerPeriod,indexBase_->fixingDays(),indexBase_->currency(),indexBase_->fixingCalendar(),
							                    indexBase_->fixedLegTenor(),indexBase_->fixedLegConvention(),indexBase_->dayCounter(),indexBase_->iborIndex(),indexBase_->discountingTermStructure()));
							tmpIndexUpper = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),upperPeriod,indexBase_->fixingDays(),indexBase_->currency(),indexBase_->fixingCalendar(),
							                    indexBase_->fixedLegTenor(),indexBase_->fixedLegConvention(),indexBase_->dayCounter(),indexBase_->iborIndex(),indexBase_->discountingTermStructure()));
						}
						else {
							tmpIndexLower = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),lowerPeriod,indexBase_->fixingDays(),indexBase_->currency(),indexBase_->fixingCalendar(),
							                    indexBase_->fixedLegTenor(),indexBase_->fixedLegConvention(),indexBase_->dayCounter(),indexBase_->iborIndex()));
							tmpIndexUpper = boost::shared_ptr<SwapIndex>(new SwapIndex(indexBase_->familyName(),upperPeriod,indexBase_->fixingDays(),indexBase_->currency(),indexBase_->fixingCalendar(),
							                    indexBase_->fixedLegTenor(),indexBase_->fixedLegConvention(),indexBase_->dayCounter(),indexBase_->iborIndex()));
						}
						boost::shared_ptr<VanillaSwap> swapLower = tmpIndexLower->underlyingSwap(expiry_);
						boost::shared_ptr<VanillaSwap> swapUpper = tmpIndexUpper->underlyingSwap(expiry_);
						// compute npv, delta, gamma
						Real npvm = alpha*NPV(swapLower,fixedRate,nominal,-h_,type) + (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,-h_,type);
						Real npv = alpha*NPV(swapLower,fixedRate,nominal,0.0,type) + (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,0.0,type);
						Real npvu = alpha*NPV(swapLower,fixedRate,nominal,h_,type) + (1.0-alpha)*NPV(swapUpper,fixedRate,nominal,h_,type);
						Real delta = (npvu-npvm) / (2.0*h_);
						Real gamma = (npvu-2.0*npv+npvm) / (h_*h_);
						// return target function values
						Array res(3);
						res[0] = (npv-npv_) / delta_;
						res[1] = (delta-delta_) / delta_;
						res[2] = (gamma-gamma_) / gamma_;
						// debug
						//std::cout << " ** type=" << type << " nom=" << nominal << " rat=" << fixedRate << " mat=" << fabs(v[1]) << " alpha=" << alpha << " ==> npv=" << npv << "/" << npv_ << " delta=" << delta << "/" << delta_ << " gamma=" << gamma << "/" << gamma_ << std::endl; 
						/*for(Real y = -5.0 ; y <= 5.0001; y+=0.1) {
							std::cout << NPV(swapLower,fixedRate,nominal,y,type) << ";"; 
						}
						std::cout << std::endl;*/
						// end debug
						return res;
					}

					const Real npv_,delta_,gamma_,h_;
					const Date& expiry_;
					const boost::shared_ptr<SwapIndex> indexBase_;
					const Handle<Gsr>& mdl_;
					const NonstandardSwap::Type type_;

				};

				boost::shared_ptr<SwaptionHelper> helper;

				if(basketType == NonstandardSwaption::Naive) {
					Real swapLength = swaptionVolatility->dayCounter().yearFraction(standardSwapBase->valueDate(expiry),arguments_.fixedPayDates.back());
					boost::shared_ptr<SwaptionVolatilityCube> cube = boost::dynamic_pointer_cast<SwaptionVolatilityCube>(swaptionVolatility);
					Real atm;
					if(cube) atm = cube->atmVol()->volatility(expiry,swapLength,0.03,true);
					else atm = swaptionVolatility->volatility(expiry,swapLength,0.03,true);
					helper = boost::shared_ptr<SwaptionHelper>(new SwaptionHelper(expiry,arguments_.fixedPayDates.back(),Handle<Quote>(new SimpleQuote(atm)),standardSwapBase->iborIndex(),
							standardSwapBase->fixedLegTenor(),standardSwapBase->dayCounter(),standardSwapBase->iborIndex()->dayCounter(), standardSwapBase->exogenousDiscount() ? standardSwapBase->discountingTermStructure() : standardSwapBase->forwardingTermStructure(),
							CalibrationHelper::RelativePriceError,Null<Real>(),arguments_.fixedNominal[fixedIdx]));
				}
				else {
					boost::shared_ptr<MatchHelper> matchHelper_;
					matchHelper_ = boost::shared_ptr<MatchHelper>(new MatchHelper(arguments_.swap->type(),npv,delta,gamma,model_,standardSwapBase,expiry,h));

					// Optimize

					Array initial(3); // compute some more or less smart start values for optimization
					Real nominalSum=0.0, weightedRate=0.0;
					for(Size i=fixedIdx;i<arguments_.fixedResetDates.size();i++) {
						nominalSum += arguments_.fixedNominal[i];
						weightedRate += arguments_.fixedNominal[i] * arguments_.fixedRate[i]; 
					}
					Real nominalAvg = nominalSum / (arguments_.fixedResetDates.size() - fixedIdx);
					Real weightedMaturity=0.0, plainMaturity=0.0;
					for(Size i=fixedIdx;i<arguments_.fixedResetDates.size();i++) {
						weightedMaturity += ActualActual().yearFraction(arguments_.fixedResetDates[i],arguments_.fixedPayDates[i])*arguments_.fixedNominal[i];
					}
					weightedMaturity /= nominalAvg;
					weightedRate /= nominalSum;
					initial[0] = nominalAvg;
					initial[1] = weightedMaturity;
					initial[2] = weightedRate;

					EndCriteria ec(1000,200,1E-8,1E-8,1E-8); // make these criteria and the optimizer itself parameters of the method ?
					Constraint constraint = NoConstraint();
					Problem p(*matchHelper_,constraint,initial);
					LevenbergMarquardt lm;

					EndCriteria::Type ret = lm.minimize(p,ec);
					QL_REQUIRE(ret != EndCriteria::None && ret != EndCriteria::Unknown && ret != EndCriteria::MaxIterations, "optimizer returns error (" << ret << ")");
					Array solution = p.currentValue();

					//std::cout << "  Optimizer start values: nominal=" << initial[0] << " rate=" << initial[1] << " maturity /y=" << initial[2] / 12.0<< std::endl;

					Real maturity = fabs(solution[1]);

					Size years = (Size)std::floor(maturity);
					maturity -= (Real)years; maturity *= 12.0;
					Size months = (Size)std::floor(maturity+0.5);
					if(years==0 && months==0) months=1; // ensure a maturity of at least one months
					//maturity -= (Real)months; maturity *= 365.25;
					//Size days = (Size)std::floor(maturity);
				
					Period matPeriod = years*Years+months*Months;//+days*Days;

					helper = boost::shared_ptr<SwaptionHelper>(new SwaptionHelper(expiry,matPeriod,Handle<Quote>(new SimpleQuote(swaptionVolatility->volatility(expiry,matPeriod,solution[2],true))),standardSwapBase->iborIndex(),
						standardSwapBase->fixedLegTenor(),standardSwapBase->dayCounter(),standardSwapBase->iborIndex()->dayCounter(), standardSwapBase->exogenousDiscount() ? standardSwapBase->discountingTermStructure() : standardSwapBase->forwardingTermStructure(),
						CalibrationHelper::RelativePriceError,fabs(solution[2]),fabs(solution[0])));
				}

				result.push_back(helper);

			}

		}

		return result;

	}

	Real GsrNonstandardSwaptionEngine::fixedLegNpv(Size idx, Date& referenceDate, Real y) const {

		Real npv=0.0;
		for(Size i=idx; i<arguments_.fixedResetDates.size(); i++) {
			npv += arguments_.fixedCoupons[i] * model_->zerobond(arguments_.fixedPayDates[i],referenceDate,y);
		}

		return npv;

	}

	Real GsrNonstandardSwaptionEngine::floatingLegNpv(Size idx, Date& referenceDate, Real y) const {
		
		Real npv=0.0;
		for(Size i=idx; i<arguments_.floatingResetDates.size(); i++) {
			npv += (model_->forwardRate(arguments_.floatingFixingDates[i],arguments_.swap->iborIndex(),referenceDate,y) + arguments_.floatingSpreads[i]) *
				   arguments_.floatingAccrualTimes[i] *
				   arguments_.floatingNominal[i] * model_->zerobond(arguments_.floatingPayDates[i],referenceDate,y);
		}

		return npv;

	}

    void GsrNonstandardSwaptionEngine::calculate() const {

		boost::shared_ptr<Interpolation> spreadF_, spreadD_; // zero rate spreads to market curves

		// build spread curves, which we just represent as interpolated differences between the respective curves zero rates
		
		const Size yN = 24; // per year discretization steps for spread interpolation objects

		Real T = model_->forwardMeasureTime();

		Size N=(int)(yN*T+0.5);
		std::vector<Real> t(N);
		std::vector<Real> yF(N),yD(N);
		for(Size i=0;i<N;++i) {
			t[i]=i*(T/((Real)N-1));
		}

		for(Size i=0;i<N;++i) yD[i] = discountYts_.empty() ? 0.0 : 
			                              discountYts_->zeroRate(t[i],QuantLib::Continuous,QuantLib::NoFrequency,true) -
										  model_->termStructure()->zeroRate(t[i],QuantLib::Continuous,QuantLib::NoFrequency,true);

		for(Size i=0;i<N;++i) yF[i] = forwardYts_.empty() ? 0.0 : 
			                              forwardYts_->zeroRate(t[i],QuantLib::Continuous,QuantLib::NoFrequency,true) -
										  model_->termStructure()->zeroRate(t[i],QuantLib::Continuous,QuantLib::NoFrequency,true);

		spreadD_ = boost::shared_ptr<Interpolation>(new CubicInterpolation(t.begin(),t.end(),yD.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0));
		spreadF_ = boost::shared_ptr<Interpolation>(new CubicInterpolation(t.begin(),t.end(),yF.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0));

		spreadD_->enableExtrapolation();
		spreadF_->enableExtrapolation();

        QL_REQUIRE(arguments_.settlementType==Settlement::Physical,
                   "cash-settled swaptions not yet implemented ...");

		Date today = Settings::instance().evaluationDate();

		if(arguments_.exercise->dates().back() <= today) {	// swaption is expired, possibly generated swap is not valued
			results_.value = 0.0;
			return;
		}

		int idx = arguments_.exercise->dates().size()-1;
		int minIdxAlive = std::upper_bound(arguments_.exercise->dates().begin(), arguments_.exercise->dates().end(), today) - arguments_.exercise->dates().begin();

		NonstandardSwap swap = *arguments_.swap;
		Option::Type type = arguments_.type==NonstandardSwap::Payer ? Option::Call : Option::Put;
		Schedule schedule = swap.fixedSchedule();
		Schedule floatSchedule = swap.floatingSchedule();

		Array npv0(2*integrationPoints_+1,0.0), npv1(2*integrationPoints_+1,0.0);
		Array z = model_->yGrid(stddevs_, integrationPoints_);
		Array p(z.size(),0.0);

		Date expiry1 = Null<Date>(), expiry0;
		Time expiry1Time = Null<Real>(), expiry0Time;

		do {

			if(idx == minIdxAlive-1)
				expiry0 = today;
			else
				expiry0 = arguments_.exercise->dates()[idx];

			expiry0Time = std::max(model_->termStructure()->timeFromReference(expiry0),0.0);

			Size j1 = std::upper_bound(schedule.dates().begin(), schedule.dates().end(), expiry0 - 1 ) - schedule.dates().begin();
			Size k1 = std::upper_bound(floatSchedule.dates().begin(), floatSchedule.dates().end(), expiry0 -1 ) - floatSchedule.dates().begin();

			for(Size k=0; k < (expiry0 > today ? npv0.size() : 1); k++) {

				Real price = 0.0;
				if(expiry1Time != Null<Real>()) {
					Array yg = model_->yGrid(stddevs_, integrationPoints_, expiry1Time, expiry0Time, expiry0Time > 0 ? z[k] : 0.0);
					CubicInterpolation payoff0(z.begin(),z.end(),npv1.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<yg.size();i++) {
						p[i] = payoff0(yg[i],true);
					}
					CubicInterpolation payoff1(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<z.size()-1;i++) {
						price += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1.cCoefficients()[i], payoff1.bCoefficients()[i], payoff1.aCoefficients()[i], p[i], z[i], z[i], z[i+1] );
					}
					if(extrapolatePayoff_) {
							if(flatPayoffExtrapolation_) {
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[0], z[0], -100.0 , z[0] );
							}
							else {
								if(type == Option::Call) price += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1.cCoefficients()[z.size()-2], payoff1.bCoefficients()[z.size()-2], payoff1.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								if(type == Option::Put) price += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1.cCoefficients()[0], payoff1.bCoefficients()[0], payoff1.aCoefficients()[0], p[0], z[0], -100.0 , z[0] );
							}
					}	
				}

				npv0[k] = price;

				if(expiry0 >today) {
					Real floatingLegNpv = 0.0;
					//floatingLegNpv = (model_->zerobond(schedule.date(j1),expiry0,z[k]) - model_->zerobond(arguments_.fixedPayDates.back(),expiry0,z[k])); // approximation
					for(Size l=k1;l<arguments_.floatingCoupons.size();l++) {
						floatingLegNpv += arguments_.floatingNominal[l] * arguments_.floatingAccrualTimes[l] *
							                (arguments_.floatingSpreads[l]+model_->forwardRate(arguments_.floatingFixingDates[l],arguments_.swap->iborIndex(),expiry0,z[k],spreadF_)) * 
											model_->zerobond(arguments_.floatingPayDates[l],expiry0,z[k],spreadD_);
					}
					Real fixedLegNpv = 0.0;
					for(Size l=j1;l<arguments_.fixedCoupons.size();l++) {
						fixedLegNpv += arguments_.fixedCoupons[l] * model_->zerobond(arguments_.fixedPayDates[l],expiry0,z[k],spreadD_);
					}
					npv0[k] = std::max( npv0[k] * (npv0[k]>0.0 ? model_->numeraire(expiry0Time,z[k],spreadD_) : 0.0) , (type==Option::Call ? 1.0 : -1.0) * ( floatingLegNpv - fixedLegNpv ) ) / model_->numeraire(expiry0Time,z[k],spreadD_);
				}

			}

			npv1.swap(npv0);
			expiry1 = expiry0;
			expiry1Time = expiry0Time;

		} while(--idx >= minIdxAlive-1);

		results_.value = npv1[0] * model_->numeraire(0.0,0.0,spreadD_);

	}

}

