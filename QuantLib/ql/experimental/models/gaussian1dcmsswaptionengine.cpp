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

#include <ql/experimental/models/gaussian1dcmsswaptionengine.hpp>

namespace QuantLib {


    void Gaussian1dCmsSwaptionEngine::calculate() const {

		// pricing

		Date today = Settings::instance().evaluationDate();

		if(arguments_.exercise->dates().back() <= today) {	// swaption is expired, possibly generated swap is not valued
			results_.value = 0.0;
			return;
		}

		// event dates are structured coupon fixing dates and exercise dates

		std::vector<Date> events;
		events.insert(events.end(),arguments_.exercise->dates().begin(),arguments_.exercise->dates().end());
		events.insert(events.end(),arguments_.structuredFixingDates.begin(),arguments_.structuredFixingDates.end());
		std::sort(events.begin(), events.end());
		std::vector<Date>::iterator it = std::unique(events.begin(),events.end());
		events.resize( std::distance(events.begin(),it) );

		// only events starting tommorow are of interest by definition of the deal part that is exericsed into,

		std::vector<Date>::iterator filit = std::upper_bound(events.begin(),events.end(),today);
		while(events[0] <= today) events.erase(events.begin(),filit);

		int idx = events.size()-1;

		CmsSwap swap = *arguments_.swap;
		Option::Type type = arguments_.type==VanillaSwap::Payer ? Option::Call : Option::Put;

		Array npv0(2*integrationPoints_+1,0.0), npv1(2*integrationPoints_+1,0.0); // arrays for npvs of the option
		Array npv0a(2*integrationPoints_+1,0.0), npv1a(2*integrationPoints_+1,0.0); // arrays for npvs of the underlying
		Array z = model_->yGrid(stddevs_, integrationPoints_);
		Array p(z.size(),0.0), pa(z.size(),0.0);

		Date event1 = Null<Date>(), event0;
		Time event1Time = Null<Real>(), event0Time;
		
		bool isExercise, isStructuredFixing;

		Size jEnd = arguments_.floatingPayDates.size(); // float coupons left to include in pricing

		do {

			// we are at event0 date, which can be a structured coupon fixing date or an exercise date or both.

			if(idx == -1)
				event0 = today;
			else
				event0 = events[idx];
			
			if( std::find( arguments_.exercise->dates().begin(), arguments_.exercise->dates().end(), event0 ) != 
                arguments_.exercise->dates().end() )
				isExercise = true;
			else
				isExercise = false;
			
			if( std::find( arguments_.structuredFixingDates.begin(), arguments_.structuredFixingDates.end(), event0 ) != 
                arguments_.structuredFixingDates.end() )
				isStructuredFixing = true;
			else
				isStructuredFixing = false;

			event0Time = std::max(model_->termStructure()->timeFromReference(event0),0.0);
            
            // locate first float coupon with start date >= event date
			Size jStart = std::upper_bound( arguments_.floatingResetDates.begin(), arguments_.floatingResetDates.end(), 
                                            event0-1 ) - arguments_.floatingResetDates.begin(); 


			for(Size k=0; k < (event0 > today ? npv0.size() : 1); k++) {

				// roll back
				
				Real price=0.0, pricea = 0.0;
				if(event1Time != Null<Real>()) {
					Array yg = model_->yGrid(stddevs_, integrationPoints_, event1Time, 
                                             event0Time, event0Time > 0 ? z[k] : 0.0);
					CubicInterpolation payoff0(z.begin(),z.end(),npv1.begin(),CubicInterpolation::Spline,
                                               true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					CubicInterpolation payoff0a(z.begin(),z.end(),npv1a.begin(),CubicInterpolation::Spline,
                                                true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<yg.size();i++) {
						p[i] = payoff0(yg[i],true);
						pa[i] = payoff0a(yg[i],true);
					}
					CubicInterpolation payoff1(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,
                                               true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					CubicInterpolation payoff1a(z.begin(),z.end(),pa.begin(),CubicInterpolation::Spline,
                                                true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);
					for(Size i=0;i<z.size()-1;i++) {
						price += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1.cCoefficients()[i], 
                                      payoff1.bCoefficients()[i], payoff1.aCoefficients()[i], p[i], z[i], z[i], z[i+1] );
						pricea += model_->gaussianShiftedPolynomialIntegral( 0.0, payoff1a.cCoefficients()[i], 
                                      payoff1a.bCoefficients()[i], payoff1a.aCoefficients()[i], pa[i], z[i], z[i], z[i+1] );
					}
					if(extrapolatePayoff_) {
							if(flatPayoffExtrapolation_) {
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, 
                                                                  p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								price += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, 
                                                                  p[0], z[0], -100.0 , z[0] );
								pricea += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0,
                                                                  pa[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								pricea += model_->gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, 
                                                                  pa[0], z[0], -100.0 , z[0] );
							}
							else {
								if(type == Option::Call) price += model_->gaussianShiftedPolynomialIntegral( 0.0, 
                                   payoff1.cCoefficients()[z.size()-2], payoff1.bCoefficients()[z.size()-2], 
                                   payoff1.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								if(type == Option::Put) price += model_->gaussianShiftedPolynomialIntegral( 0.0, 
                                   payoff1.cCoefficients()[0], payoff1.bCoefficients()[0], payoff1.aCoefficients()[0], 
                                                                                                 p[0], z[0], -100.0 , z[0] );
								if(type == Option::Call) pricea += model_->gaussianShiftedPolynomialIntegral( 0.0, 
                                   payoff1a.cCoefficients()[z.size()-2], payoff1a.bCoefficients()[z.size()-2], 
                                   payoff1a.aCoefficients()[z.size()-2], pa[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
								if(type == Option::Put) pricea += model_->gaussianShiftedPolynomialIntegral( 0.0,
                                   payoff1a.cCoefficients()[0], payoff1a.bCoefficients()[0], payoff1a.aCoefficients()[0], 
                                                                                                  pa[0], z[0], -100.0 , z[0] );
							}
					}	
				}

				npv0[k] = price;
				npv0a[k] = pricea;

				// event date calculations

				if(event0 > today) {
					for(Size j=jStart; j<jEnd; j++) {
						npv0a[k] += (model_->forwardRate( arguments_.floatingFixingDates[j], 
                                                          event0, z[k], arguments_.swap->iborIndex() ) 
                                     + arguments_.floatingSpreads[j]) *
										arguments_.floatingAccrualTimes[j] *
										model_->zerobond( arguments_.floatingPayDates[j], event0, z[k], discountCurve_ ) / 
                            model_->numeraire(event0Time,z[k],discountCurve_);
					}
					if(isStructuredFixing) { // if event is structured fixing date and exercise date, 
                                             // structured coupon is part of the exercise into right (by definition)
						Size j = std::find( arguments_.structuredFixingDates.begin(), arguments_.structuredFixingDates.end(), 
                                            event0 ) - arguments_.structuredFixingDates.begin();
						Real rate = arguments_.structuredSpreads[j] + 
                            model_->swapRate( arguments_.structuredFixingDates[j], arguments_.swap->swapIndex()->tenor(), 
                                              event0, z[k], arguments_.swap->swapIndex() ); 
						if( arguments_.structuredCappedRates[j] != Null<Real>() ) rate = std::min( 
                                                                               arguments_.structuredCappedRates[j] , rate );
						if( arguments_.structuredFlooredRates[j] != Null<Real>() ) rate = std::max( 
                                                                              arguments_.structuredFlooredRates[j] , rate );
						npv0a[k] += rate * arguments_.structuredAccrualTimes[j] * 
                            model_->zerobond( arguments_.structuredPayDates[j], event0, z[k], discountCurve_ ) / 
                                           model_->numeraire(event0Time,z[k],discountCurve_);
					}
					if(isExercise) {
						npv0[k] = std::max( npv0[k] , (type==Option::Call ? 1.0 : -1.0) * npv0a[k] );
					}

				}

			}

			jEnd = jStart; // update float leg alive indices

			npv1.swap(npv0);
			npv1a.swap(npv0a);
			event1 = event0;
			event1Time = event0Time;

		} while(--idx >= -1);

		results_.value = npv1[0] * model_->numeraire(0.0,0.0,discountCurve_);
		results_.additionalResults["underlyingValue"] = npv1a[0] * model_->numeraire(0.0,0.0,discountCurve_);

	}

}
