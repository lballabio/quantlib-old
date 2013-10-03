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

#include <ql/experimental/models/sbsmilesection.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#include <ql/pricingengines/blackformula.hpp>

#include <iostream> // debug

namespace QuantLib {

    SbSmileSection::SbSmileSection(const Time timeToExpiry,
                                   const Real forward,
                                   const Real lambda,
                                   const Real delta,
                                   const Real gamma,
                                   const Size resolution,
                                   const Real lower,
                                   const Real upper) :
        SmileSection(timeToExpiry, DayCounter()),
        f_(forward), lambda_(lambda), delta_(delta), gamma_(gamma) {

        QL_REQUIRE(lambda_>0.0,"lambda must be positive (" << lambda_ << ")");

        adjustment_ = 1.0;

        // set xi as to match forward approximately
        Real omega = std::exp( 1.0 / (delta_*delta_) );
        Real Omega = gamma_ / delta_;
        Real origE = - std::sqrt(omega)*std::sinh(Omega);
        xi_ = f_ - lambda_*origE;
        std::cout << "o=" << omega << " O=" << Omega << " E " << origE << std::endl;

        // lower and upper bound for $y$ spline discretization
        minStrike_ = lower * lambda_ + xi_;
        maxStrike_ = upper *lambda_ + xi_;

        k_.clear();
        d_.clear();

        NormalDistribution norm;

        k_.push_back(minStrike_);
        d_.push_back(0.0);
        for(Size i=1;i<resolution;i++) {
            Real y = lower+(upper-lower)*((double)i)/((double)resolution);
            Real x = xi_+y*lambda_;
            // Real p = delta_*lambda_/((x-xi_)*(lambda_+xi_-x)) * norm( gamma_+delta_*std::log( (x-xi_)/(lambda_+xi_-x) ) ); // system S_B
            Real p = delta_/lambda_/std::sqrt(1.0+y*y) * norm( gamma_+delta_*std::log( y + (std::sqrt(1.0+y*y)))); // system S_U
            k_.push_back(x);
            d_.push_back(p);
        }
        k_.push_back(maxStrike_); 
        d_.push_back(0.0);

        std::cout << "building spline..." << std::endl;

        density_ = boost::shared_ptr<CubicInterpolation>(new CubicInterpolation(k_.begin(), k_.end(),
                                                                           d_.begin(), CubicInterpolation::Spline, true,
                                                                           CubicInterpolation::SecondDerivative, 0.0,
                                                                           CubicInterpolation::SecondDerivative, 0.0));

        std::cout << "end..." << std::endl;

        update();

    }

    void SbSmileSection::update() {

        const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        const std::vector<Real> &pb_ = density_->bCoefficients();
        const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;

        eta_.clear();
        kappa_.clear();

        etaSum_ = 0.0;
        expectation_ = 0.0;
        for (Size i = 0; i < k_.size() - 1; i++) {
            Real dk = k_[i + 1] - k_[i];
            Real dk2 = dk*dk;
            eta_.push_back(1.0 / 4.0 * pa_[i] * dk2*dk2 +
                           1.0 / 3.0 * pb_[i] * dk2*dk +
                           1.0 / 2.0 * pc_[i] * dk2 +
                           pd_[i] * dk);
            // linear interpolation
            // Real pa=0.0,pb=0.0;
            // Real pc=(d_[i+1]-d_[i])/(k_[i+1]-k_[i]);
            // eta_.push_back(1.0 / 4.0 * pa * dk2*dk2 +
            //                1.0 / 3.0 * pb * dk2*dk +
            //                1.0 / 2.0 * pc * dk2 +
            //                pd_[i] * dk);

            expectation_ +=
                k_[i+1] * eta_.back() - 1.0 / 20.0 * pa_[i] * dk2 * dk2 * dk -
                1.0 / 12.0 * pb_[i] * dk2 * dk2 -
                1.0 / 6.0 * pc_[i] * dk2 * dk - 1.0 / 2.0 * pd_[i] * dk2;

            kappa_.push_back(etaSum_);
            etaSum_ += eta_.back();
        }

        adjustment_ = 1.0 / etaSum_;
        expectation_ *= adjustment_;
        
        std::cout << "minstrike = " << minStrike_ << " maxstrike = " << maxStrike_ << std::endl;
        std::cout << "expectation = " << expectation_ << " adjustment = " << adjustment_ << std::endl;

    }

    Real SbSmileSection::optionPrice(Rate strike, Option::Type type, Real discount) const {
        
        if(type == Option::Put) return discount*(optionPrice(strike, Option::Call, 1.0) - f_ + strike);

        strike += expectation_-f_;

        if(strike <= minStrike_) return expectation_-strike;
        if(strike >= maxStrike_) return 0.0; 

        Real price = expectation_ - minStrike_;

        const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        const std::vector<Real> &pb_ = density_->bCoefficients();
        const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;

        for(Size j=0; j<=index(strike); j++) {

            Real mu = std::min(k_[j+1],strike);
            Real dk = mu - k_[j];
            Real dk2 = dk*dk;

            price += (dk * (adjustment_*kappa_[j] - 1.0) + adjustment_*(
                                    1.0 / 20.0 * pa_[j] * dk2*dk2*dk +
                                    1.0 / 12.0 * pb_[j] * dk2*dk2 +
                                    1.0 / 6.0 * pc_[j] * dk2*dk +
                                    1.0 / 2.0 * pd_[j] * dk2));
            
            // linear interpolation
            // Real pa=0.0,pb=0.0;
            // Real pc=(d_[j+1]-d_[j])/(k_[j+1]-k_[j]);
            // price += (dk * (adjustment_*kappa_[j] - 1.0) + adjustment_*(
            //                         1.0 / 20.0 * pa * dk2*dk2*dk +
            //                         1.0 / 12.0 * pb * dk2*dk2 +
            //                         1.0 / 6.0 * pc * dk2*dk +
            //                         1.0 / 2.0 * pd_[j] * dk2));

        }

        return price;

    }

    Real SbSmileSection::digitalOptionPrice(Rate strike, Option::Type type, Real discount, Real gap) const {

        //return SmileSection::digitalOptionPrice(strike,type,discount,gap); // debug
        
        if(type == Option::Put) return discount*(1.0 - digitalOptionPrice(strike, Option::Call, 1.0));

        strike += expectation_-f_;

        if(strike <= minStrike_) return discount;
        if(strike >= maxStrike_) return 0.0; 

        Real price = 1.0;

        const std::vector<Real> &pa_ = density_->cCoefficients(); // just for our convenience
        const std::vector<Real> &pb_ = density_->bCoefficients();
        const std::vector<Real> &pc_ = density_->aCoefficients();
        const std::vector<Real> &pd_ = d_;
        
        Size n = index(strike);

        for(Size j=0; j<n; j++) {
            price -= adjustment_*eta_[j];
        }

        Real dk = strike - k_[n];
        Real dk2 = dk*dk;

        price -= adjustment_* ( 1.0 / 4.0 * pa_[n] * dk2*dk2 +
                 1.0 / 3.0 * pb_[n] * dk2*dk +
                                1.0 / 2.0 * pc_[n] * dk2 + pd_[n] * dk);

        // linear interpolation
        // Real pa = 0.0, pb = 0.0, pc = (c_[n+1]-c_[n])/(k_[n+1]-k_[n]);
        // price -= adjustment_* ( 1.0 / 4.0 * pa * dk2*dk2 +
        //          1.0 / 3.0 * pb * dk2*dk +
        //                         1.0 / 2.0 * pc * dk2 + pd_[n] * dk);

        return price;

    }

    Real SbSmileSection::density(Rate strike, Real discount, Real gap) const {

        // return SmileSection::density(strike,discount,gap); // debug
        strike += expectation_-f_;

        if(strike <= minStrike_ || strike >= maxStrike_) return 0.0; // only useful for SB
        
        return adjustment_*density_->operator()(strike)*discount;

    }

    Size SbSmileSection::index(Real strike) const {

        // QL_REQUIRE(strike > xi_ && strike < xi_+lambda_, "strike (" << strike <<") outside bounds ("
        //            << xi_ << "," << xi_+lambda_ << ")");
        return std::upper_bound(k_.begin(),k_.end(),strike) - k_.begin() - 1;

    }

    Real SbSmileSection::volatilityImpl(Rate strike) const {
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
