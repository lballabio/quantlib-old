/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

#include <ql/experimental/models/qg1dlocalvolmodel.hpp>

#include <ql/instruments/vanillaswap.hpp>

namespace QuantLib {

Qg1dLocalVolModel::Qg1dLocalVolModel(const Handle<YieldTermStructure> &yts)
    : TermStructureConsistentModel(yts) {
    // default integrator, may be changed in derived classes
    integrator_ = boost::make_shared<SimpsonIntegral>(1E-10, 100);
}

Real Qg1dLocalVolModel::zerobond(const Real T, const Real t, const Real x,
                                 const Real y,
                                 const Handle<YieldTermStructure> &yts) const {
    Real tmp = G(t, T);
    const Handle<YieldTermStructure> &ytsTmp =
        yts.empty() ? termStructure() : yts;
    return ytsTmp->discount(T) / ytsTmp->discount(t) *
           std::exp(-G(t, T) * x - 0.5 * tmp * tmp * y);
}

Real Qg1dLocalVolModel::swapRate(const Real T0, const Real t,
                                 const std::vector<Real> &fixedTimes,
                                 const std::vector<Real> &taus, const Real x,
                                 const Real y,
                                 const Handle<YieldTermStructure> &yts) const {
    Real a = 0.0;
    for (Size i = 0; i < fixedTimes.size(); ++i) {
        a += taus[i] * zerobond(fixedTimes[i], t, x, y, yts);
    }
    return (zerobond(T0, t, x, y, yts) -
            zerobond(fixedTimes.back(), t, x, y, yts)) /
           a;
}

Real Qg1dLocalVolModel::dSwapRateDx(
    const Real T0, const Real t, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Real x, const Real y,
    const Handle<YieldTermStructure> &yts) const {
    Real a = 0.0, sum = 0.0;
    for (Size i = 0; i < fixedTimes.size(); ++i) {
        Real tmp = taus[i] * zerobond(fixedTimes[i], t, x, y, yts);
        a += tmp;
        sum += tmp * G(t, fixedTimes[i]);
    }
    Real Tn = fixedTimes.back();
    Real z0 = zerobond(T0, t, x, y, yts);
    Real zn = zerobond(Tn, t, x, y, yts);
    return (-G(t, T0) * z0 + G(t, Tn) * zn) / a + (z0 - zn) / (a * a) * sum;
}

Real Qg1dLocalVolModel::zerobond(const Date &maturity,
                                 const Date &referenceDate, const Real x,
                                 const Real y,
                                 const Handle<YieldTermStructure> &yts) {
    return zerobond(termStructure()->timeFromReference(maturity),
                    termStructure()->timeFromReference(referenceDate), x, y,
                    yts);
}

void Qg1dLocalVolModel::timesAndTaus(const Date &startDate,
                                     const boost::shared_ptr<SwapIndex> &index,
                                     const Period &tenor, Real &T0,
                                     std::vector<Real> &times,
                                     std::vector<Real> &taus) const {
    T0 = termStructure()->timeFromReference(startDate);
    std::vector<Date> dates =
        index->underlyingSwap(index->clone(tenor)->fixingDate(startDate))
            ->fixedSchedule()
            .dates();
    times.resize(dates.size() - 1);
    taus.resize(dates.size() - 1);
    for (Size i = 1; i < dates.size(); ++i) {
        times[i - 1] = termStructure()->timeFromReference(dates[i]);
        taus[i - 1] = index->dayCounter().yearFraction(dates[i - 1], dates[i]);
    }
}

Real Qg1dLocalVolModel::swapRate(const Date &startDate,
                                 const Date &referenceDate,
                                 const boost::shared_ptr<SwapIndex> &index,
                                 const Period &tenor, const Real x,
                                 const Real y) const {
    Real T0;
    std::vector<Real> taus, times;
    timesAndTaus(startDate, index, tenor, T0, times, taus);
    return swapRate(T0, termStructure()->timeFromReference(referenceDate),
                    times, taus, x, y, index->forwardingTermStructure());
}

Real Qg1dLocalVolModel::dSwapRateDx(const Date &startDate,
                                    const Date &referenceDate,
                                    const boost::shared_ptr<SwapIndex> &index,
                                    const Period &tenor,
                                    const Real x, const Real y) const {
    Real T0;
    std::vector<Real> taus, times;
    timesAndTaus(startDate, index, tenor, T0, times, taus);
    return dSwapRateDx(termStructure()->timeFromReference(startDate),
                       termStructure()->timeFromReference(referenceDate), times,
                       taus, x, y, index->forwardingTermStructure());
}

} // namespace QuantLib
