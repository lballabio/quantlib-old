
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/curvestate.hpp>
#include <ql/models/marketmodels/curvestates/cmswapcurvestate.hpp>
#include <ql/models/marketmodels/curvestates/coterminalswapcurvestate.hpp>
#include <ql/models/marketmodels/curvestates/lmmcurvestate.hpp>

namespace QuantLibAddin {

    CMSwapCurveState::CMSwapCurveState(std::vector<QuantLib::Time>& rateTimes, QuantLib::Size spanningForwards) {
        libraryObject_ = boost::shared_ptr<QuantLib::CMSwapCurveState>(new
            QuantLib::CMSwapCurveState(rateTimes, spanningForwards));
    }

    CoterminalSwapCurveState::CoterminalSwapCurveState(std::vector<QuantLib::Time>& rateTimes) {
        libraryObject_ = boost::shared_ptr<QuantLib::CoterminalSwapCurveState>(new
            QuantLib::CoterminalSwapCurveState(rateTimes));
    }

    LMMCurveState::LMMCurveState(std::vector<QuantLib::Time>& rateTimes) {
        libraryObject_ = boost::shared_ptr<QuantLib::LMMCurveState>(new
            QuantLib::LMMCurveState(rateTimes));
    }

    std::vector<QuantLib::Rate> qlForwardsFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus) {
        std::vector<QuantLib::Rate> result(taus.size());
        QuantLib::forwardsFromDiscountRatios(firstValidIndex, ds, taus,
                                             result);
        return result;
    }

    std::vector<QuantLib::Rate> qlCoterminalSwapRatesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus) {
        std::vector<QuantLib::Real> rates(taus.size());
        std::vector<QuantLib::Real> annuities(taus.size());
        QuantLib::coterminalFromDiscountRatios(firstValidIndex, ds, taus,
                                               rates, annuities);
        return rates;
    }

    std::vector<QuantLib::Real> qlCoterminalSwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus) {
        std::vector<QuantLib::Real> rates(taus.size());
        std::vector<QuantLib::Real> annuities(taus.size());
        QuantLib::coterminalFromDiscountRatios(firstValidIndex, ds, taus,
                                               rates, annuities);
        return annuities;
    }

    std::vector<QuantLib::Rate> qlConstantMaturitySwapRatesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus) {
        std::vector<QuantLib::Real> rates(taus.size());
        std::vector<QuantLib::Real> annuities(taus.size());
        QuantLib::constantMaturityFromDiscountRatios(spanningForwards,
                                                     firstValidIndex, ds, taus,
                                                     rates, annuities);
        return rates;
    }

    std::vector<QuantLib::Real> qlConstantMaturitySwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus) {
        std::vector<QuantLib::Real> rates(taus.size());
        std::vector<QuantLib::Real> annuities(taus.size());
        QuantLib::constantMaturityFromDiscountRatios(spanningForwards,
                                                     firstValidIndex, ds, taus,
                                                     rates, annuities);
        return annuities;
    }

}
