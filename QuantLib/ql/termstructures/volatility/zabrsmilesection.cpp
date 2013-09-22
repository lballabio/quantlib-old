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

#include <ql/termstructures/volatility/zabrsmilesection.hpp>
#include <ql/pricingengines/blackformula.hpp>

#include <iostream>

namespace QuantLib {

    ZabrSmileSection::ZabrSmileSection(Time timeToExpiry, Rate forward,
                                       const std::vector<Real> &zabrParams,
                                       const Evaluation evaluation,
                                       const std::vector<Real> &moneyness,
                                       const Size localVolRefinement)
        : SmileSection(timeToExpiry, DayCounter(),
                       false/*evaluation == ShortMaturityNormal*/ // at the moment with stick with lognormal sections
                           ? SmileSection::Normal
                           : SmileSection::ShiftedLognormal),
          evaluation_(evaluation), forward_(forward), params_(zabrParams),
          localVolRefinement_(localVolRefinement) {

        init(moneyness);
    }

    ZabrSmileSection::ZabrSmileSection(const Date &d, Rate forward,
                                       const std::vector<Real> &zabrParams,
                                       const DayCounter &dc,
                                       const Evaluation evaluation,
                                       const std::vector<Real> &moneyness,
                                       const Size localVolRefinement)
        : SmileSection(d, dc, Date(), false/*evaluation == ShortMaturityNormal*/ // at the moment we stick with lognormal sections
                                          ? SmileSection::Normal
                                          : SmileSection::ShiftedLognormal),
          evaluation_(evaluation), forward_(forward), params_(zabrParams),
          localVolRefinement_(localVolRefinement) {

        init(moneyness);
    }

    void ZabrSmileSection::init(const std::vector<Real> &moneyness) {

        QL_REQUIRE(params_.size() == 5,
                   "zabr expects 5 parameters (alpha,beta,nu,rho,gamma) but ("
                       << params_.size() << ") given");

        // set up strike grid for local vol or full fd flavour of this section
        // this is shared with SmileSectionUtils - unify later ?
        static const Real defaultMoney[] = { 0.0,  0.01, 0.05, 0.10, 0.25, 0.40,
                                             0.50, 0.60, 0.70, 0.80, 0.90, 1.0,
                                             1.25, 1.5,  1.75, 2.0,  5.0,  7.5,
                                             10.0, 15.0, 20.0 };
        std::vector<Real> tmp;
        if (moneyness.size() == 0)
            tmp = std::vector<Real>(defaultMoney, defaultMoney + 21);
        else
            tmp = std::vector<Real>(moneyness);

        strikes_.clear(); // should not be necessary, anyway
        Real lastF = 0.0;
        for (Size i = 0; i < tmp.size(); i++) {
            Real f = tmp[i] * forward_;
            if (f > 0.0) {
                if (evaluation_ == LocalVolatility && i > 0) {
                    for (Size j = 1; j < localVolRefinement_; j++) {
                        strikes_.push_back(lastF +
                                           ((double)j) * (f - lastF) /
                                               (localVolRefinement_ + 1));
                    }
                }
                lastF = f;
                strikes_.push_back(f);
            }
        }

        //debug
        // for(Size i=0;i<strikes_.size();i++)
        //     std::cout << strikes_[i] << std::endl;

        model_ = boost::shared_ptr<ZabrModel>(
            new ZabrModel(exerciseTime(), forward_, params_[0], params_[1],
                          params_[2], params_[3], params_[4]));

        // precompute call price function for local vol or full fd flavours

        if (evaluation_ == LocalVolatility) {
            callPrices_ = model_->fdPrice(strikes_);
        }

        if (evaluation_ == FullFd) {
            callPrices_.clear();
            for (Size i = 0; i < strikes_.size(); i++) {
                callPrices_.push_back(model_->fullFdPrice(strikes_[i]));
            }
        }

        if (evaluation_ == LocalVolatility || evaluation_ == FullFd) {

            strikes_.insert(strikes_.begin(), 0.0);
            callPrices_.insert(callPrices_.begin(), forward_);

            callPriceFct_ =
                boost::shared_ptr<Interpolation>(new CubicInterpolation(
                    strikes_.begin(), strikes_.end(), callPrices_.begin(),
                    CubicInterpolation::Spline, true,
                    CubicInterpolation::SecondDerivative, 0.0,
                    CubicInterpolation::SecondDerivative, 0.0));

            // on the right side we extrapolate exponetially (because spline
            // does not make sense)
            // we precompute the necessary parameters here
            static const Real eps =
                1E-5; // gap for first derivative computation

            Real c0 = callPriceFct_->operator()(strikes_.back());
            Real c0p =
                (callPriceFct_->operator()(strikes_.back() - eps) - c0) / eps;

            a_ = c0p / c0;
            b_ = std::log(c0) + a_ * strikes_.back();
        }
    }

    Real ZabrSmileSection::optionPrice(Rate strike, Option::Type type,
                                       Real discount) const {

        switch (evaluation_) {

        case ShortMaturityLognormal:
        //case ShortMaturityNormal: // produce lognormal vol from normal expansion price
            if(!(params_[1]<1.0) && strike <= 0.0) return forward_*discount;
            else  return SmileSection::optionPrice(strike, type, discount);

        case ShortMaturityNormal:
            return bachelierBlackFormula(
                type, strike, forward_,
                model_->normalVolatility(strike) * std::sqrt(exerciseTime()),
                discount);

        case LocalVolatility:
        case FullFd:
            QL_REQUIRE(type == Option::Call,
                       "local volatility only supports calls at the moment");
            if (strike <= strikes_.back())
                return callPriceFct_->operator()(strike) * discount;
            else
                return exp(-a_ * strike + b_) * discount;

        default:
            QL_FAIL("Unknown evaluation (" << evaluation_ << ")");
        }
    }

    Real ZabrSmileSection::volatilityImpl(Rate strike) const {

        switch (evaluation_) {

        case ShortMaturityLognormal: {
            strike = std::max(1E-6, strike);
            return model_->lognormalVolatility(strike);
        }

        // case ShortMaturityNormal: // at the moment we want to produce a lognormal vol from the normal expansion
        //     if(!(params_[1]<1.0)) strike = std::max(1E-6,strike);
        //     return model_->normalVolatility(strike);
            
        case ShortMaturityNormal:
        case LocalVolatility:
        case FullFd: {
            Real impliedVol = 0.0;
            try {
                Option::Type type;
                if (strike >= model_->forward() ||
                    evaluation_ == LocalVolatility ||
                    evaluation_ == FullFd) // use otm option to imply
                                                    // vol
                    type = Option::Call; // todo: implement put price in
                                         // zabrsmilesection for LocalVolatility
                else
                    type = Option::Put;
                impliedVol = blackFormulaImpliedStdDev(
                                 type, strike, model_->forward(),
                                 optionPrice(strike, type, 1.0), 1.0) /
                             std::sqrt(exerciseTime());
            }
            catch (...) {
            }
            return impliedVol;
        }

        default:
            QL_FAIL("Unknown evaluation (" << evaluation_ << ")");
        }
    }
}
