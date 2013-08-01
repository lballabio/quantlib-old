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
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/time/daycounters/actualactual.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>

namespace QuantLib {

	Disposable<std::vector<boost::shared_ptr<CalibrationHelper> > > 
        GsrNonstandardSwaptionEngine::calibrationBasket(boost::shared_ptr<SwapIndex> standardSwapBase, 
                       boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolatility,
                       const NonstandardSwaption::CalibrationBasketType basketType ) const {

		QL_REQUIRE(!standardSwapBase->forwardingTermStructure().empty(), 
                   "standard swap base forwarding term structure must not be empty.");
		QL_REQUIRE(!standardSwapBase->exogenousDiscount() || !standardSwapBase->discountingTermStructure().empty(), 
                   "standard swap base discounting term structure must not be empty.");
        
        if(standardSwapBase->exogenousDiscount())
            effectiveDiscountYts_ = &**standardSwapBase->discountingTermStructure();
        else
            effectiveDiscountYts_ = &**standardSwapBase->forwardingTermStructure();

        effectiveForwardYts_ = &**standardSwapBase->forwardingTermStructure();

        initSpreadCurves();

		std::vector<boost::shared_ptr<CalibrationHelper>> result;

        int minIdxAlive = std::upper_bound(arguments_.exercise->dates().begin(), arguments_.exercise->dates().end(),
                                           Settings::instance().evaluationDate()) - arguments_.exercise->dates().begin();

		for(Size i=minIdxAlive; i< arguments_.exercise->dates().size() ; i++) {

			Date expiry = arguments_.exercise->date(i);
            Real rebate = arguments_.exercise->rebate(i);
            Date rebateDate = arguments_.exercise->rebatePaymentDate(i);            

			// determine the indices on both legs representing the cashflows that are part of the exercise into right

			Size fixedIdx = std::upper_bound(arguments_.fixedResetDates.begin(), 
                                arguments_.fixedResetDates.end(), expiry-1 ) - arguments_.fixedResetDates.begin();
			Size floatingIdx = std::upper_bound(arguments_.floatingResetDates.begin(), 
                                arguments_.floatingResetDates.end(), expiry-1 ) - arguments_.floatingResetDates.begin();
			Real type = (Real)arguments_.type;

			if(fixedIdx < arguments_.fixedResetDates.size()) {  // do nothing if no cashflows on fixed leg are present

                // determine the npv, first and second order derivatives at $y=0$ of the underlying swap 
			
                const Real h = 0.0001; 			// finite difference step in $y$, make this a parameter of the engine ?
                Real zSpreadDsc = zSpread_.empty() ? 1.0 : 
                                   exp(-zSpread_->value()*model_->termStructure()->dayCounter().
                                       yearFraction(expiry,rebateDate));

                Real npvm = type*(floatingLegNpv(floatingIdx,expiry,-h) - fixedLegNpv(fixedIdx,expiry,-h)) +
                             rebate * model_->zerobond(rebateDate,expiry,-h,spreadD_) * zSpreadDsc;
                Real npv = type*(floatingLegNpv(floatingIdx,expiry,0.0) - fixedLegNpv(fixedIdx,expiry,0.0)) +
                            rebate * model_->zerobond(rebateDate,expiry,0,spreadD_) *  zSpreadDsc;
                Real npvp = type*(floatingLegNpv(floatingIdx,expiry,h) - fixedLegNpv(fixedIdx,expiry,h)) +
                             rebate * model_->zerobond(rebateDate,expiry,h,spreadD_) * zSpreadDsc;

                Real delta = (npvp-npvm)/(2.0*h);
                Real gamma = (npvp-2.0*npv+npvm)/(h*h);
                
                //std::cout << "EXOTIC npv " << npv << " delta " << delta << " gamma " << gamma << std::endl;
                //debug: output the global npv for x=-5...5
#ifdef DEBUGOUTPUT
                Real xtmp=-5.0;
                std::cout << "********************************************EXERCISE " << expiry << " ******************" 
                          << std::endl;
                std::cout << "globalExoticNpv;";
                while(xtmp<=5.0+QL_EPSILON) {
                    std::cout <<  type*(floatingLegNpv(floatingIdx,expiry,xtmp) - fixedLegNpv(fixedIdx,expiry,xtmp)) << ";";
                    xtmp +=0.1;
                }
                std::cout << std::endl;
#endif

                boost::shared_ptr<SwaptionHelper> helper;

                switch(basketType) {

                case NonstandardSwaption::Naive: {
                    Real swapLength = swaptionVolatility->dayCounter().yearFraction(standardSwapBase->valueDate(expiry),
                                                                                    arguments_.fixedPayDates.back());
                    boost::shared_ptr<SwaptionVolatilityCube> cube = 
                        boost::dynamic_pointer_cast<SwaptionVolatilityCube>(swaptionVolatility);
                    Real atm;
                    if(cube) atm = cube->atmVol()->volatility(expiry,swapLength,0.03,true);
                    else atm = swaptionVolatility->volatility(expiry,swapLength,0.03,true);
                    helper = boost::shared_ptr<SwaptionHelper>(new SwaptionHelper(expiry,arguments_.fixedPayDates.back(),
                                   Handle<Quote>(new SimpleQuote(atm)),standardSwapBase->iborIndex(),
                                   standardSwapBase->fixedLegTenor(),standardSwapBase->dayCounter(),
                                   standardSwapBase->iborIndex()->dayCounter(), 
                                   standardSwapBase->exogenousDiscount() ? standardSwapBase->discountingTermStructure() : 
                                   standardSwapBase->forwardingTermStructure(),
                                   CalibrationHelper::RelativePriceError,Null<Real>(),1.0));

                    break;
                }

                case NonstandardSwaption::MaturityStrikeByDeltaGamma: {
                    boost::shared_ptr<MatchHelper> matchHelper_;
                    matchHelper_ = boost::shared_ptr<MatchHelper>(new MatchHelper(arguments_.swap->type(),npv,
                                                     delta,gamma,model_,standardSwapBase,expiry,h,spreadD_,spreadF_));

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
                        weightedMaturity += ActualActual().yearFraction(arguments_.fixedResetDates[i],
                                                                        arguments_.fixedPayDates[i])*arguments_.fixedNominal[i];
                    }

                    QL_REQUIRE(nominalSum > 0.0, "sum of nominals on fixed leg must be positive (" << nominalSum << ")");

                    weightedMaturity /= nominalAvg;
                    weightedRate /= nominalSum;
                    initial[0] = nominalAvg;
                    initial[1] = weightedMaturity;
                    initial[2] = weightedRate;

                    EndCriteria ec(1000,200,1E-8,1E-8,1E-8); // make these criteria and the optimizer itself 
                    // parameters of the method ?
                    Constraint constraint = NoConstraint();
                    Problem p(*matchHelper_,constraint,initial);
                    LevenbergMarquardt lm;

                    EndCriteria::Type ret = lm.minimize(p,ec);
                    QL_REQUIRE(ret != EndCriteria::None && ret != EndCriteria::Unknown && 
                               ret != EndCriteria::MaxIterations, "optimizer returns error (" << ret << ")");
                    Array solution = p.currentValue();

                    Real maturity = fabs(solution[1]);

                    Size years = (Size)std::floor(maturity);
                    maturity -= (Real)years; maturity *= 12.0;
                    Size months = (Size)std::floor(maturity+0.5);
                    if(years==0 && months==0) months=1; // ensure a maturity of at least one months
                    //maturity -= (Real)months; maturity *= 365.25;
                    //Size days = (Size)std::floor(maturity);
				
                    Period matPeriod = years*Years+months*Months;//+days*Days;

                    // we have to floor the strike of the calibration instrument because
                    // ql only allows for lognormal swaptions here at the moment
                    solution[2] = std::max(solution[2], 0.0001); // floor at 1bp
                    
                    // also the calibrated nominal may be zero, so we floor it, too
                    solution[0] = std::max(solution[0], 0.000001); // float at 0.01bp

                    helper = boost::shared_ptr<SwaptionHelper>(new SwaptionHelper(expiry,matPeriod,
                          Handle<Quote>(new SimpleQuote(swaptionVolatility->volatility(expiry,matPeriod,solution[2],true))),
                          standardSwapBase->iborIndex(),standardSwapBase->fixedLegTenor(),
                          standardSwapBase->dayCounter(),standardSwapBase->iborIndex()->dayCounter(), 
                          standardSwapBase->exogenousDiscount() ? standardSwapBase->discountingTermStructure() : 
                          standardSwapBase->forwardingTermStructure(),
                          CalibrationHelper::RelativePriceError,solution[2],fabs(solution[0])));
                    break;
                }

                default:
                    QL_FAIL("Calibration basket type not known (" << basketType << ")");
                
                }

                result.push_back(helper);

            }

        }

		return result;

    }

	Real GsrNonstandardSwaptionEngine::fixedLegNpv(Size idx, Date& referenceDate, Real y) const {

		Real npv=0.0;
		for(Size i=idx; i<arguments_.fixedResetDates.size(); i++) {
			npv += arguments_.fixedCoupons[i] * model_->zerobond(arguments_.fixedPayDates[i],referenceDate,y,spreadD_) *
                (zSpread_.empty() ? 1.0 : 
                 exp(-zSpread_->value()*model_->termStructure()->dayCounter().
                     yearFraction(referenceDate,arguments_.fixedPayDates[i])));
		}

		return npv;

	}

	Real GsrNonstandardSwaptionEngine::floatingLegNpv(Size idx, Date& referenceDate, Real y) const {
		
		Real npv=0.0;
		for(Size i=idx; i<arguments_.floatingResetDates.size(); i++) {
            Real amount;
            if(!arguments_.floatingIsRedemptionFlow[i])
                amount = 
                    (arguments_.floatingGearings[i]*
                     model_->forwardRate(arguments_.floatingFixingDates[i],arguments_.swap->iborIndex(),
                                         referenceDate,y,spreadF_) + arguments_.floatingSpreads[i]) *
                                         arguments_.floatingAccrualTimes[i] * arguments_.floatingNominal[i];
            else
                amount = arguments_.floatingCoupons[i];
			npv +=  amount * 
                model_->zerobond(arguments_.floatingPayDates[i],referenceDate,y,spreadD_) *
                (zSpread_.empty() ? 1.0 : 
                 exp(-zSpread_->value()*model_->termStructure()->dayCounter().
                     yearFraction(referenceDate,arguments_.floatingPayDates[i])));
		}

		return npv;

	}

    void GsrNonstandardSwaptionEngine::initSpreadCurves() const {
        
		// build spread curves, which we just represent as interpolated differences between the respective curves zero rates
		
		const Size yN = 24; // per year discretization steps for spread interpolation objects

		Real T = model_->forwardMeasureTime();

		Size N=(int)(yN*T+0.5);
        t_.resize(N); yF_.resize(N); yD_.resize(N);
		for(Size i=0;i<N;++i) {
			t_[i]=i*(T/((Real)N-1));
		}

		for(Size i=0;i<N;++i) yD_[i] = effectiveDiscountYts_->zeroRate(t_[i],QuantLib::Continuous,QuantLib::NoFrequency,true) -
									  model_->termStructure()->zeroRate(t_[i],QuantLib::Continuous,QuantLib::NoFrequency,true);

		for(Size i=0;i<N;++i) yF_[i] = effectiveForwardYts_->zeroRate(t_[i],QuantLib::Continuous,QuantLib::NoFrequency,true) -
									  model_->termStructure()->zeroRate(t_[i],QuantLib::Continuous,QuantLib::NoFrequency,true);
        
		spreadD_ = boost::shared_ptr<Interpolation>(new CubicInterpolation(t_.begin(),t_.end(),yD_.begin(),
                    CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0));
		spreadF_ = boost::shared_ptr<Interpolation>(new CubicInterpolation(t_.begin(),t_.end(),yF_.begin(),
                    CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0));

		spreadD_->enableExtrapolation();
		spreadF_->enableExtrapolation();

    }

    void GsrNonstandardSwaptionEngine::calculate() const {
        
        effectiveForwardYts_ = &**model_->termStructure();
        if(arguments_.iborIndex != NULL) 
            if(!arguments_.iborIndex->forwardingTermStructure().empty())
                effectiveForwardYts_ = &**arguments_.iborIndex->forwardingTermStructure();

        QL_REQUIRE(arguments_.settlementType==Settlement::Physical,
                   "cash-settled swaptions not yet implemented ...");

		Date today = Settings::instance().evaluationDate();

		if(arguments_.exercise->dates().back() <= today) {	// swaption is expired, possibly generated swap is not valued
			results_.value = 0.0;
			return;
		}

		int idx = arguments_.exercise->dates().size()-1;
		int minIdxAlive = std::upper_bound(arguments_.exercise->dates().begin(), arguments_.exercise->dates().end(), 
                                           today) - arguments_.exercise->dates().begin();

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

			Size j1 = std::upper_bound(schedule.dates().begin(), schedule.dates().end(), expiry0 - 1 ) - 
                schedule.dates().begin();
			Size k1 = std::upper_bound(floatSchedule.dates().begin(), floatSchedule.dates().end(), expiry0 -1 ) - 
                floatSchedule.dates().begin();

			for(Size k=0; k < (expiry0 > today ? npv0.size() : 1); k++) {

				Real price = 0.0;
				if(expiry1Time != Null<Real>()) {
                    Real zSpreadDf = zSpread_.empty() ? 1.0 : std::exp(-zSpread_->value()*(expiry1Time-expiry0Time));
					Array yg = model_->yGrid(stddevs_, integrationPoints_, expiry1Time, expiry0Time, expiry0Time > 0 ? 
                                             z[k] : 0.0);
					CubicInterpolation payoff0(z.begin(),z.end(),npv1.begin(),CubicInterpolation::Spline,true,
                                               CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<yg.size();i++) {
						p[i] = payoff0(yg[i],true);
					}
					CubicInterpolation payoff1(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,true,
                                               CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<z.size()-1;i++) {
						price += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1.cCoefficients()[i], 
                            payoff1.bCoefficients()[i], payoff1.aCoefficients()[i], p[i], z[i], z[i], z[i+1] ) * zSpreadDf;
					}
					if(extrapolatePayoff_) {
							if(flatPayoffExtrapolation_) {
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, 
                                                   p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 ) * zSpreadDf;
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, 
                                                   p[0], z[0], -100.0 , z[0] ) * zSpreadDf;
							}
							else {
								if(type == Option::Call) price += model_->gaussianShiftedPolynomialIntegral( 
                                     0.0, payoff1.cCoefficients()[z.size()-2], payoff1.bCoefficients()[z.size()-2], 
                                     payoff1.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 )
                                                             *zSpreadDf;
								if(type == Option::Put) price += model_->gaussianShiftedPolynomialIntegral( 
                                     0.0, payoff1.cCoefficients()[0], payoff1.bCoefficients()[0], 
                                     payoff1.aCoefficients()[0], p[0], z[0], -100.0 , z[0] )*zSpreadDf;
							}
					}	
				}

				npv0[k] = price;

				if(expiry0 >today) {
					Real floatingLegNpv = 0.0;
					//floatingLegNpv = (model_->zerobond(schedule.date(j1),expiry0,z[k]) 
                    //- model_->zerobond(arguments_.fixedPayDates.back(),expiry0,z[k])); // approximation
					for(Size l=k1;l<arguments_.floatingCoupons.size();l++) {
                        Real zSpreadDf = zSpread_.empty() ? 1.0 : 
                            std::exp(-zSpread_->value()*(model_->termStructure()->dayCounter()
                                                         .yearFraction(expiry0,arguments_.floatingPayDates[l])));
                        Real amount;
                        if(arguments_.floatingIsRedemptionFlow[l])
                            amount = arguments_.floatingCoupons[l];
                        else 
                            amount = arguments_.floatingNominal[l] * arguments_.floatingAccrualTimes[l] *
							    (arguments_.floatingGearings[l] *
                                 model_->forwardRate(arguments_.floatingFixingDates[l],
                                                                      arguments_.swap->iborIndex(),expiry0,z[k],spreadF_)
                                 + arguments_.floatingSpreads[l]);
						floatingLegNpv +=  amount * model_->zerobond(arguments_.floatingPayDates[l],expiry0,z[k],spreadD_)
                            * zSpreadDf;
					}
					Real fixedLegNpv = 0.0;
					for(Size l=j1;l<arguments_.fixedCoupons.size();l++) {
                        Real zSpreadDf = zSpread_.empty() ? 1.0 : 
                            std::exp(-zSpread_->value()*(model_->termStructure()->dayCounter()
                                                         .yearFraction(expiry0,arguments_.fixedPayDates[l])));
						fixedLegNpv += arguments_.fixedCoupons[l] * model_->zerobond(arguments_.fixedPayDates[l],
                                                                                     expiry0,z[k],spreadD_) * zSpreadDf;
					}
                    Real rebate = arguments_.exercise->rebate(idx);
                    Date rebateDate = arguments_.exercise->rebatePaymentDate(idx);
                    Real zSpreadDf = zSpread_.empty() ? 1.0 : 
                        std::exp(-zSpread_->value()*(model_->termStructure()->dayCounter()
                                                     .yearFraction(expiry0,rebateDate)));
					npv0[k] = std::max( npv0[k],  
                                        ((type==Option::Call ? 1.0 : -1.0) * ( floatingLegNpv - fixedLegNpv ) + 
                                         rebate * model_->zerobond(rebateDate,expiry0,z[k],spreadD_) * zSpreadDf) / 
                                        model_->numeraire(expiry0Time,z[k],spreadD_) );
				}

			}

			npv1.swap(npv0);
			expiry1 = expiry0;
			expiry1Time = expiry0Time;

		} while(--idx >= minIdxAlive-1);

		results_.value = npv1[0] * model_->numeraire(0.0,0.0,spreadD_);

	}

}

