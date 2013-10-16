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


#include <ql/experimental/models/basketgeneratingengine.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>
#include <ql/quotes/simplequote.hpp>

namespace QuantLib {

    	Disposable<std::vector<boost::shared_ptr<CalibrationHelper> > > 
        BasketGeneratingEngine::calibrationBasket(const boost::shared_ptr<Exercise>& exercise,
                                                  boost::shared_ptr<SwapIndex> standardSwapBase, 
                                                  boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolatility,
                                                  const CalibrationBasketType basketType ) const {

		QL_REQUIRE(!standardSwapBase->forwardingTermStructure().empty(), 
                   "standard swap base forwarding term structure must not be empty.");
		QL_REQUIRE(!standardSwapBase->exogenousDiscount() || !standardSwapBase->discountingTermStructure().empty(), 
                   "standard swap base discounting term structure must not be empty.");

		std::vector<boost::shared_ptr<CalibrationHelper>> result;

        int minIdxAlive = std::upper_bound(exercise->dates().begin(), exercise->dates().end(),
                                           Settings::instance().evaluationDate()) - exercise->dates().begin();

		for(Size i=minIdxAlive; i<exercise->dates().size() ; i++) {

			Date expiry = exercise->date(i);
            Real rebate = exercise->rebate(i);
            Date rebateDate = exercise->rebatePaymentDate(i);            

            // determine the npv, first and second order derivatives at $y=0$ of the underlying swap 
			
            const Real h = 0.0001; 			// finite difference step in $y$, make this a parameter of the engine ?
            Real zSpreadDsc = oas_.empty() ? 1.0 : 
                exp(-oas_->value()*onefactormodel_->termStructure()->dayCounter().
                    yearFraction(expiry,rebateDate));

            Real npvm = underlyingNpv(expiry,-h) +
                rebate * onefactormodel_->zerobond(rebateDate,expiry,-h,discountCurve_) * zSpreadDsc;
            Real npv = underlyingNpv(expiry,0.0) +
                rebate * onefactormodel_->zerobond(rebateDate,expiry,0,discountCurve_) *  zSpreadDsc;
            Real npvp = underlyingNpv(expiry,h) +
                rebate * onefactormodel_->zerobond(rebateDate,expiry,h,discountCurve_) * zSpreadDsc;

            Real delta = (npvp-npvm)/(2.0*h);
            Real gamma = (npvp-2.0*npv+npvm)/(h*h);

            if(!(npv == 0.0 && delta == 0.0 && gamma == 0.0)) {

#ifdef DEBUGOUTPUT
                std::cout << "EXOTIC npv " << npv << " delta " << delta << " gamma " << gamma << std::endl;
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

                case Naive: {
                    Real swapLength = swaptionVolatility->dayCounter().yearFraction(standardSwapBase->valueDate(expiry),
                                                                                    underlyingLastDate());
                    boost::shared_ptr<SwaptionVolatilityCube> cube = 
                        boost::dynamic_pointer_cast<SwaptionVolatilityCube>(swaptionVolatility);
                    Real atm;
                    if(cube) atm = cube->atmVol()->volatility(expiry,swapLength,0.03,true);
                    else atm = swaptionVolatility->volatility(expiry,swapLength,0.03,true);
                    Real atmStrike = standardSwapBase->fixing(expiry);
                    // we have to floor the strike of the calibration instrument, see warning in the header
                    atmStrike = std::max(atmStrike, 0.00001);                    
                    helper = boost::shared_ptr<SwaptionHelper>(new SwaptionHelper(expiry,underlyingLastDate(),
                                   Handle<Quote>(new SimpleQuote(atm)),standardSwapBase->iborIndex(),
                                   standardSwapBase->fixedLegTenor(),standardSwapBase->dayCounter(),
                                   standardSwapBase->iborIndex()->dayCounter(), 
                                   standardSwapBase->exogenousDiscount() ? standardSwapBase->discountingTermStructure() : 
                                   standardSwapBase->forwardingTermStructure(),
                                   CalibrationHelper::RelativePriceError,atmStrike,1.0));

                    break;
                }

                case MaturityStrikeByDeltaGamma: {
                    boost::shared_ptr<MatchHelper> matchHelper_;
                    matchHelper_ = boost::shared_ptr<MatchHelper>(new MatchHelper(underlyingType(),npv,
                                                     delta,gamma,onefactormodel_,standardSwapBase,expiry,h));

                    // Optimize

                    Array initial = initialGuess(expiry);
                    QL_REQUIRE(initial.size()==3, "initial guess must have size 3 (but is " << initial.size() << ")");

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

                    // we have to floor the strike of the calibration instrument, see warning in the header
                    solution[2] = std::max(solution[2], 0.00001); // floor at 0.1bp
                    
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


}
