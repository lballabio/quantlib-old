
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
#include <qlo/driftcalculators.hpp>
#include <ql/models/marketmodels/driftcomputation/lmmdriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/cmsmmdriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/lmmnormaldriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/smmdriftcalculator.hpp>

namespace QuantLibAddin {
        
  LMMDriftCalculator::LMMDriftCalculator(
                                    const QuantLib::Matrix& pseudo,
                                    const std::vector<QuantLib::Rate>& displ,
                                    const std::vector<QuantLib::Time>& taus,
                                    QuantLib::Size numeraire,
                                    QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_ = boost::shared_ptr<QuantLib::LMMDriftCalculator>(new
            QuantLib::LMMDriftCalculator(pseudo, displ,
                                         taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::compute(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::computePlain(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computePlain(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::computeReduced(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computeReduced(cs, drifts_);
        return drifts_;
    }

    LMMNormalDriftCalculator::LMMNormalDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_ = boost::shared_ptr<QuantLib::LMMNormalDriftCalculator>(
            new QuantLib::LMMNormalDriftCalculator(pseudo,
                                                   taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::compute(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::computePlain(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computePlain(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::computeReduced(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computeReduced(cs, drifts_);
        return drifts_;
    }

    CMSMMDriftCalculator::CMSMMDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Rate>& displ,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive,
                                QuantLib::Size spanningFwds)
    : drifts_(taus.size()) {
        libraryObject_= boost::shared_ptr<QuantLib::CMSMMDriftCalculator>(new
            QuantLib::CMSMMDriftCalculator(pseudo, displ,
                                           taus, numeraire, alive,
                                           spanningFwds));
    }

    std::vector<QuantLib::Real> CMSMMDriftCalculator::compute(
                                const QuantLib::CMSwapCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    SMMDriftCalculator::SMMDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Rate>& displ,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_= boost::shared_ptr<QuantLib::SMMDriftCalculator>(new
            QuantLib::SMMDriftCalculator(pseudo, displ,
                                         taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> SMMDriftCalculator::compute(
                                const QuantLib::CoterminalSwapCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

}
