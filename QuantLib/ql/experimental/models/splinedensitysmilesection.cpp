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

#include <ql/experimental/models/splinedensitysmilesection.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/bfgs.hpp>
#include <ql/math/optimization/constraint.hpp>

#include <iostream> // debug

namespace QuantLib {

    SplineDensitySmileSection::SplineDensitySmileSection(
        const boost::shared_ptr<SmileSection> source, const Real atm,
        const Real lowerBound, const Real upperBound,
        const bool deleteArbitragePoints,
        const std::vector<Real> &moneynessGrid)
        : SmileSection(*source), source_(source),
          lowerBound_(lowerBound), upperBound_(upperBound) {

        adjustment_ = 1.0;

        ssutils_ = boost::shared_ptr<SmileSectionUtils>(new SmileSectionUtils(
            *source, moneynessGrid, atm, deleteArbitragePoints));

        std::pair<Size, Size> af = ssutils_->arbitragefreeIndices();

        Size leftIndex = af.first;
        Size rightIndex = af.second;
        
        f_ = ssutils_->atmLevel();

        const std::vector<Real> &strikes = ssutils_->strikeGrid();
        const std::vector<Real> &calls = ssutils_->callPrices();

        k_.clear();
        c_.clear();
        d_.clear();
        
        k_.push_back(lowerBound_);
        c_.push_back(f_-lowerBound_);
        d_.push_back(0.0);

        Real vol = 0.10; // start with a lognormal density
        Real mu = std::log(f_) - vol*vol*exerciseTime()/2.0;
        NormalDistribution norm(mu,vol*sqrt(exerciseTime()));


        std::vector<Real> initialv;

        for(Size i=leftIndex; i<=rightIndex; i++) {
            if( strikes[i] > lowerBound_ && strikes[i] < upperBound_ ) {
                k_.push_back(strikes[i]);
                c_.push_back(calls[i]);
                d_.push_back( norm(std::log(strikes[i]))/ strikes[i] );
                initialv.push_back(std::sqrt(d_.back()));
            }
        }

        Array initial(initialv.begin(),initialv.end());

        k_.push_back(upperBound_);
        c_.push_back(0.0);
        d_.push_back(0.0);


        // density_ = boost::shared_ptr<CubicInterpolation>(new CubicInterpolation(k_.begin(), k_.end(),
        //                                                                    d_.begin(), CubicInterpolation::Spline, true,
        //                                                                    CubicInterpolation::SecondDerivative, 0.0,
        //                                                                    CubicInterpolation::SecondDerivative, 0.0));

        density_ = boost::shared_ptr<LinearInterpolation>(new LinearInterpolation(k_.begin(), k_.end(), d_.begin()));
        
        // // global calibration
        // SplineDensityCostFunction cost(this);
        // NoConstraint constraint;
        // Problem p(cost,constraint,initial);

   		// LevenbergMarquardt lm;
		// //Simplex lm(0.01);
		// //BFGS lm;
		// EndCriteria ec(5000,100,1e-16,1e-16,1e-16);

		// EndCriteria::Type ret = lm.minimize(p,ec);
		// QL_REQUIRE(ret!=EndCriteria::MaxIterations,"Optimizer returns maxiterations");

		// Array res = p.currentValue();
        // setDensity(res); // just to make really sure, we are at the optimal point

        // iterative calibration
        // for(Size i=1; i<k_.size()-1; i++) {
        //     SplineDensityCostFunction2 cost(this,i);
        //     NoConstraint constraint;
        //     Array initial2(1,initial[i-1]);
        //     Problem p(cost,constraint,initial2);

        //     LevenbergMarquardt lm;
        //     //Simplex lm(0.01);
        //     //BFGS lm;
        //     EndCriteria ec(5000,100,1e-35,1e-35,1e-35);

        //     EndCriteria::Type ret = lm.minimize(p,ec);
        //     QL_REQUIRE(ret!=EndCriteria::MaxIterations,"Optimizer returns maxiterations");

        //     Array res = p.currentValue();
        //     setDensity(res[0],i); // just to make really sure, we are at the optimal point
        // }

        update();

    }

    void SplineDensitySmileSection::update() {

        // const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        // const std::vector<Real> &pb_ = density_->bCoefficients();
        // const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;

        eta_.clear();
        lambda_.clear();

        etaSum_ = 0.0;
        expectation_ = 0.0;
        for (Size i = 0; i < k_.size() - 1; i++) {
            Real dk = k_[i + 1] - k_[i];
            Real dk2 = dk*dk;
            // eta_.push_back(1.0 / 4.0 * pa_[i] * dk2*dk2 +
            //                1.0 / 3.0 * pb_[i] * dk2*dk +
            //                1.0 / 2.0 * pc_[i] * dk2 +
            //                pd_[i] * dk);
            // expectation_ +=
            //     k_[i+1] * eta_.back() - 1.0 / 20.0 * pa_[i] * dk2 * dk2 * dk -
            //     1.0 / 12.0 * pb_[i] * dk2 * dk2 -
            //     1.0 / 6.0 * pc_[i] * dk2 * dk - 1.0 / 2.0 * pd_[i] * dk2;

            // linear interpolation
            Real pa=0.0,pb=0.0;
            Real pc=(d_[i+1]-d_[i])/(k_[i+1]-k_[i]);
            eta_.push_back(1.0 / 4.0 * pa * dk2*dk2 +
                           1.0 / 3.0 * pb * dk2*dk +
                           1.0 / 2.0 * pc * dk2 +
                           pd_[i] * dk);

            expectation_ +=
                k_[i+1] * eta_.back() - 1.0 / 20.0 * pa * dk2 * dk2 * dk -
                1.0 / 12.0 * pb * dk2 * dk2 -
                1.0 / 6.0 * pc * dk2 * dk - 1.0 / 2.0 * pd_[i] * dk2;

            lambda_.push_back(etaSum_);
            etaSum_ += eta_.back();
            //std::cout << k_[i] << " " << k_[i+1] << " etasum " << etaSum_ << std::endl;
        }

        // adjustment_ = 1.0 / etaSum_;
        // expectation_ *= adjustment_;

        //std::cout << "expectation is " << expectation_ << " adjustment is " << adjustment_ << std::endl;

    }

    Real SplineDensitySmileSection::setDensity(const Real d, const Size i) {
        
        d_[i] = d*d;

        density_->update();
        update();

        Real p = optionPrice(k_[i]);
        std::cout << std::setprecision(16) << "strike " << k_[i] << " dens " << d << " market " << c_[i] << " spline " << p << " error " << (p-c_[i]) << std::endl;
        
        return p-c_[i];

    }

    Disposable<Array> SplineDensitySmileSection::setDensity(const Array& d) {
        
        Array errors(c_.size()-2);
        
        std::cout << "***************************" << std::endl;
        Real signChanges = 0.0;
        for(Size i=1;i<d_.size()-1;i++) {
            d_[i] = d[i-1]*d[i-1];
            std::cout << "knot " <<  i << " dens " << d_[i] << std::endl;
            if( (d_[i]-d_[i-1]) * (d_[i+1]-d_[i]) < 0.0)
                signChanges+=1.0;
        }
        if(signChanges > 0.0) signChanges -= 1.0;

        density_->update();
        update();

        Real penalty =
            1.0 + /*((std::exp(10.0* (f_ - expectation_) * (f_ - expectation_)) - 1.0) +*/
            (std::exp((1.0 - adjustment_) * (1.0 - adjustment_)) - 1.0);
            //(std::exp(signChanges)-1.0);

        for (Size i = 1; i < c_.size() - 1; i++) {
            errors[i-1] = (c_[i] - optionPrice(k_[i])) * penalty;
            std::cout << "strike " << k_[i] << " market " << c_[i] << " spline " << c_[i] - errors[i-1] << std::endl;
        }
        std::cout << "penatly = " << penalty << std::endl;

        return errors;

    }

    Real SplineDensitySmileSection::optionPrice(Rate strike, Option::Type type, Real discount) const {
        
        if(type == Option::Put) return discount*(optionPrice(strike, Option::Call, 1.0) - f_ + strike);

        //strike += expectation_-f_;

        if(strike <= lowerBound_) return f_-strike;
        if(strike >= upperBound_) return 0.0;

        Real price = f_- lowerBound_;// + expectation_-f_;

        // const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        // const std::vector<Real> &pb_ = density_->bCoefficients();
        // const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;

        for(Size j=0; j<=index(strike); j++) {

            Real mu = std::min(k_[j+1],strike);
            Real dk = mu - k_[j];
            Real dk2 = dk*dk;

            // price += (dk * (adjustment_*lambda_[j] - 1.0) + adjustment_*(
            //                         1.0 / 20.0 * pa_[j] * dk2*dk2*dk +
            //                         1.0 / 12.0 * pb_[j] * dk2*dk2 +
            //                         1.0 / 6.0 * pc_[j] * dk2*dk +
            //                         1.0 / 2.0 * pd_[j] * dk2));
            
            // linear interpolation
            Real pa=0.0,pb=0.0;
            Real pc=(d_[j+1]-d_[j])/(k_[j+1]-k_[j]);
            price += (dk * (adjustment_*lambda_[j] - 1.0) + adjustment_*(
                                    1.0 / 20.0 * pa * dk2*dk2*dk +
                                    1.0 / 12.0 * pb * dk2*dk2 +
                                    1.0 / 6.0 * pc * dk2*dk +
                                    1.0 / 2.0 * pd_[j] * dk2));

        }

        return price;

    }

    Real SplineDensitySmileSection::digitalOptionPrice(Rate strike, Option::Type type, Real discount, Real gap) const {

        //return SmileSection::digitalOptionPrice(strike,type,discount,gap); // debug
        
        if(type == Option::Put) return discount*(1.0 - digitalOptionPrice(strike, Option::Call, 1.0));

        //strike += expectation_-f_;

        if(strike <= lowerBound_) return discount;
        if(strike >= upperBound_) return 0.0;

        Real price = 1.0;

        // const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        // const std::vector<Real> &pb_ = density_->bCoefficients();
        // const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;
        
        Size n = index(strike);

        for(Size j=0; j<n; j++) {
            price -= adjustment_*eta_[j];
        }

        Real dk = strike - k_[n];
        Real dk2 = dk*dk;

        // price -= adjustment_* ( 1.0 / 4.0 * pa_[n] * dk2*dk2 +
        //          1.0 / 3.0 * pb_[n] * dk2*dk +
        //                         1.0 / 2.0 * pc_[n] * dk2 + pd_[n] * dk);

        // linear interpolation
        Real pa = 0.0, pb = 0.0, pc = (c_[n+1]-c_[n])/(k_[n+1]-k_[n]);
        price -= adjustment_* ( 1.0 / 4.0 * pa * dk2*dk2 +
                 1.0 / 3.0 * pb * dk2*dk +
                                1.0 / 2.0 * pc * dk2 + pd_[n] * dk);

        return price;

    }

    Real SplineDensitySmileSection::density(Rate strike, Real discount, Real gap) const {

        return SmileSection::density(strike,discount,gap); // debug
        //strike += expectation_-f_;

        if(strike <= lowerBound_ || strike >= upperBound_) return 0.0;
        
        return adjustment_*density_->operator()(strike)*discount;

    }

    Size SplineDensitySmileSection::index(Real strike) const {

        QL_REQUIRE(strike > lowerBound_ && strike < upperBound_, "strike (" << strike <<") outside bounds ("
                   << lowerBound_ << "," << upperBound_ << ")");
        return std::upper_bound(k_.begin(),k_.end(),strike) - k_.begin() - 1;

    }

    Real SplineDensitySmileSection::volatilityImpl(Rate strike) const {
        strike = std::max(strike,0.00001);  // assume only non shifted lognormal
                                            // here ... TODO later ... (should be
                                            // shifted lognormal with shift equal
                                            // to minus lowerBound_

        Option::Type type = strike >= f_ ? Option::Call : Option::Put;        
        Real p = optionPrice(strike,type);
        Real vol = 0.0;
        try {
            vol = blackFormulaImpliedStdDev(type, strike, f_, p) /
                  sqrt(exerciseTime());
        }
        catch (...) {
        }
        return vol;
    }

}
