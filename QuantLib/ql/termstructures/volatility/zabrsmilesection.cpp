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

namespace QuantLib {

    ZabrSmileSection::ZabrSmileSection(Time timeToExpiry, Rate forward,
                                       const std::vector<Real> &zabrParams,
                                       const Evaluation evaluation)
        : SmileSection(timeToExpiry), evaluation_(evaluation),
          forward_(forward), params_(zabrParams) {

        init();
    }

    ZabrSmileSection::ZabrSmileSection(const Date &d, Rate forward,
                                       const std::vector<Real> &zabrParams,
                                       const DayCounter &dc,
                                       const Evaluation evaluation)
        : SmileSection(d, dc), evaluation_(evaluation), forward_(forward),
          params_(zabrParams) {

        init();
    }

    void ZabrSmileSection::init() {

        QL_REQUIRE(params_.size() == 5,
                   "zabr expects 5 parameters (alpha,beta,nu,rho,gamma) but ("
                       << params_.size() << ") given");

        model_ = boost::shared_ptr<ZabrModel>(
            new ZabrModel(exerciseTime(), forward_, params_[0], params_[1],
                          params_[2], params_[3], params_[4]));
    }

    Real ZabrSmileSection::optionPrice(Rate strike, Option::Type type,
                                       Real discount) const {

        switch (evaluation_) {

        case ShortMaturityLognormal:
            return SmileSection::optionPrice(strike, type, discount);

        case ShortMaturityNormal:
            return bachelierBlackFormula(
                type, strike, model_->forward(),
                model_->normalVolatility(strike) * std::sqrt(exerciseTime()),
                discount);

        case LocalVolatility:
            QL_REQUIRE(type == Option::Call, "local volatility only supports calls at the moment");
            return model_->fdPrice(strike) * discount;

        default:
            QL_FAIL("Unknown evaluation (" << evaluation_ << ")");
        }
    }

    Real ZabrSmileSection::volatilityImpl(Rate strike) const {

        switch (evaluation_) {

        case ShortMaturityLognormal: {
            strike = std::max(0.00001, strike);
            return model_->lognormalVolatility(strike);
        }

        case ShortMaturityNormal:
        case LocalVolatility: {
            Real impliedVol = 0.0;
            try {
                Option::Type type;
                if (strike >= model_->forward() || evaluation_ == LocalVolatility) // use otm option to imply vol
                    type = Option::Call; // todo: implement put price in zabrsmilesection for LocalVolatility
                else
                    type = Option::Put;
                impliedVol =
                    blackFormulaImpliedStdDev(
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
