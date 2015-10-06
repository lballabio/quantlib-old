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

#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/instruments/vanillaswap.hpp>

namespace QuantLib {

Qg1dLocalVolModel::Qg1dLocalVolModel(const Handle<YieldTermStructure> &yts)
    : TermStructureConsistentModel(yts) {
    // default integrator, may be changed in derived classes
    integrator_ = boost::make_shared<GaussLobattoIntegral>(100, 1E-8, 1E-8);
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
    Real result0, result1, result2;
    swapRate_d0_d1_d2(T0, t, fixedTimes, taus, x, y, yts, result0, result1,
                      result2, true, false, false);
    return result0;
}

Real Qg1dLocalVolModel::dSwapRateDx(
    const Real T0, const Real t, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Real x, const Real y,
    const Handle<YieldTermStructure> &yts) const {
    Real result0, result1, result2;
    swapRate_d0_d1_d2(T0, t, fixedTimes, taus, x, y, yts, result0, result1,
                      result2, false, true, false);
    return result1;
}

Real Qg1dLocalVolModel::d2SwapRateDx2(
    const Real T0, const Real t, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Real x, const Real y,
    const Handle<YieldTermStructure> &yts) const {
    Real result0, result1, result2;
    swapRate_d0_d1_d2(T0, t, fixedTimes, taus, x, y, yts, result0, result1,
                      result2, false, false, true);
    return result2;
}

Real Qg1dLocalVolModel::xApprox(const Real t, const Real T0,
                                const std::vector<Real> &fixedTimes,
                                const std::vector<Real> &taus,
                                const Handle<YieldTermStructure> &yts) const {
    Real s0, tmp1, tmp2;
    swapRate_d0_d1_d2(T0, 0.0, fixedTimes, taus, 0.0, 0.0, yts, s0, tmp1, tmp2,
                      true, false, false);
    Real x0 = sInvX(t, T0, fixedTimes, taus, yts, s0);
    Real y0 = yApprox(t);
    Real result0, dSdx, result2;
    swapRate_d0_d1_d2(T0, t, fixedTimes, taus, x0, y0, yts, result0, dSdx,
                      result2, false, true, false);
    Real d2XdS2 = -1.0 / (dSdx * dSdx);
    Real varS = varSApprox(t, T0, fixedTimes, taus, yts);
    return d2XdS2 * varS;
}

Disposable<std::vector<Real> > Qg1dLocalVolModel::xi(
    const Real t, const Real T0, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Handle<YieldTermStructure> &yts,
    const std::vector<Real> &s) const {
    Real x = xApprox(t, T0, fixedTimes, taus, yts);
    Real y = yApprox(t);
    Real d0, d1, d2;
    swapRate_d0_d1_d2(T0, t, fixedTimes, taus, x, y, yts, d0, d1, d2, true,
                      true, true);
    std::vector<Real> tmp(s.size());
    for (Size i = 0; i < tmp.size(); ++i)
        tmp[i] = (-d1 + std::sqrt(d1 * d1 - 2.0 * d2 * (d0 - s[i]))) / d2 + x;
    return tmp;
}

Real Qg1dLocalVolModel::xi(const Real t, const Real T0,
                           const std::vector<Real> &fixedTimes,
                           const std::vector<Real> &taus,
                           const Handle<YieldTermStructure> &yts,
                           const Real s) const {
    std::vector<Real> tmp(1, s);
    return xi(t, T0, fixedTimes, taus, yts, tmp)[0];
}

Disposable<std::vector<Real> > Qg1dLocalVolModel::phi(
    const Real t, const std::vector<Real> &s, const Real T0,
    const std::vector<Real> &fixedTimes, const std::vector<Real> &taus,
    const Handle<YieldTermStructure> &yts, bool numericalInversion) const {
    std::vector<Real> x(s.size());
    if (numericalInversion) {
        for (Size i = 0; i < x.size(); ++i)
            x[i] = sInvX(t, T0, fixedTimes, taus, yts, s[i]);
    } else {
        x = xi(t, T0, fixedTimes, taus, yts, s);
    }
    Real y = yApprox(t);
    std::vector<Real> tmp(x.size());
    for (Size i = 0; i < tmp.size(); ++i)
        tmp[i] = dSwapRateDx(T0, t, fixedTimes, taus, x[i], y, yts) *
                 sigma_f(t, t, x[i], y);
    return tmp;
}

Real Qg1dLocalVolModel::phi(const Real t, const Real s, const Real T0,
                            const std::vector<Real> &fixedTimes,
                            const std::vector<Real> &taus,
                            const Handle<YieldTermStructure> &yts,
                            bool numericalInversion) const {
    std::vector<Real> tmp(1, s);
    return phi(t, tmp, T0, fixedTimes, taus, yts, numericalInversion)[0];
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

void Qg1dLocalVolModel::swapRate_d0_d1_d2(
    const Real T0, const Real t, const std::vector<Real> &fixedTimes,
    const std::vector<Real> &taus, const Real x, const Real y,
    const Handle<YieldTermStructure> &yts, Real &result_d0, Real &result_d1,
    Real &result_d2, const bool compute_d0, const bool compute_d1,
    const bool compute_d2) const {

    Real a = 0.0, sum = 0.0, sum2 = 0.0;

    for (Size i = 0; i < fixedTimes.size(); ++i) {
        Real tmp = taus[i] * zerobond(fixedTimes[i], t, x, y, yts);
        a += tmp;
        if (compute_d1 || compute_d2) {
            Real g = G(t, fixedTimes[i]);
            sum += tmp * g;
            if (compute_d2)
                sum2 += tmp * g * g;
        }
    }
    Real Tn = fixedTimes.back();
    Real z0 = zerobond(T0, t, x, y, yts);
    Real zn = zerobond(Tn, t, x, y, yts);
    Real g0 = 0.0, gn = 0.0; // avoid maybe-uninitialized warnings
    if (compute_d2 || compute_d1) {
        g0 = G(t, T0);
        gn = G(t, Tn);
    }
    if (compute_d0 || compute_d2)
        result_d0 = (z0 - zn) / a;
    if (compute_d1 || compute_d2)
        result_d1 = (-g0 * z0 + gn * zn) / a + (z0 - zn) / (a * a) * sum;
    if (compute_d2)
        result_d2 =
            ((g0 * g0 * z0 - gn * gn * zn) * a + (gn * zn - g0 * z0) * sum +
             (result_d1 * sum - result_d0 * sum2) * a + result_d0 * sum * sum) /
            (a * a);
}

// date based methods

Real Qg1dLocalVolModel::zerobond(const Date &maturity,
                                 const Date &referenceDate, const Real x,
                                 const Real y,
                                 const Handle<YieldTermStructure> &yts) {
    return zerobond(termStructure()->timeFromReference(maturity),
                    termStructure()->timeFromReference(referenceDate), x, y,
                    yts);
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
                                    const Period &tenor, const Real x,
                                    const Real y) const {
    Real T0;
    std::vector<Real> taus, times;
    timesAndTaus(startDate, index, tenor, T0, times, taus);
    return dSwapRateDx(termStructure()->timeFromReference(startDate),
                       termStructure()->timeFromReference(referenceDate), times,
                       taus, x, y, index->forwardingTermStructure());
}

Real Qg1dLocalVolModel::d2SwapRateDx2(const Date &startDate,
                                      const Date &referenceDate,
                                      const boost::shared_ptr<SwapIndex> &index,
                                      const Period &tenor, const Real x,
                                      const Real y) const {
    Real T0;
    std::vector<Real> taus, times;
    timesAndTaus(startDate, index, tenor, T0, times, taus);
    return d2SwapRateDx2(termStructure()->timeFromReference(startDate),
                         termStructure()->timeFromReference(referenceDate),
                         times, taus, x, y, index->forwardingTermStructure());
}

} // namespace QuantLib
