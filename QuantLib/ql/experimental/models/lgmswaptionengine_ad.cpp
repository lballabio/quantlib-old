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

#include <ql/experimental/models/lgmswaptionengine_ad.hpp>
#include <ql/payoff.hpp>

namespace QuantLib {

namespace {

// ad library interface
// for the plain interface, remove ad_ from the method name
// and the parameter dres from the parameter list
// for a stub interface remove extern "C" and add a dummy
// implementation (with just a return statement)
extern "C" void lgm_swaption_engine_(
    int *n_times, double *times, double *modpar, int *n_expiries, int *expiries,
    int *callput, int *n_floats, int *float_startidxes, double *float_mults,
    double *index_acctimes, double *float_spreads, int *float_t1s,
    int *float_t2s, int *float_tps, int *fix_startidxes, int *n_fixs,
    double *fix_cpn, int *fix_tps, int *integration_points, double *stddevs,
    double *res /*, double *dres*/);
}

void LgmSwaptionEngineAD::calculate() const {

    // collect data needed for core computation routine

    QL_REQUIRE(arguments_.settlementType == Settlement::Physical,
               "cash-settled swaptions not yet implemented ...");

    Date settlement = model_->termStructure()->referenceDate();

    if (arguments_.exercise->dates().back() <=
        settlement) { // swaption is expired, possibly generated swap is not
                      // valued
        results_.value = 0.0;
        return;
    }

    int idxMax = static_cast<int>(arguments_.exercise->dates().size()) - 1;
    int minIdxAlive = static_cast<int>(
        std::upper_bound(arguments_.exercise->dates().begin(),
                         arguments_.exercise->dates().end(), settlement) -
        arguments_.exercise->dates().begin());

    VanillaSwap swap = *arguments_.swap;

    int callput = arguments_.type == VanillaSwap::Payer ? 1 : -1;

    Schedule fixedSchedule = swap.fixedSchedule();
    Schedule floatSchedule = swap.floatingSchedule();

    Date expiry0;

    std::vector<int> fix_startidxes, float_startidxes;
    std::vector<Date> expiryDates;

    for (int idx = minIdxAlive; idx <= idxMax; ++idx) {
        expiry0 = arguments_.exercise->dates()[idx];
        expiryDates.push_back(expiry0);

        Size j1 = std::upper_bound(fixedSchedule.dates().begin(),
                                   fixedSchedule.dates().end(), expiry0 - 1) -
                  fixedSchedule.dates().begin();
        Size k1 = std::upper_bound(floatSchedule.dates().begin(),
                                   floatSchedule.dates().end(), expiry0 - 1) -
                  floatSchedule.dates().begin();

        fix_startidxes.push_back(j1);
        float_startidxes.push_back(k1);
    }

    std::vector<double> float_mults, index_acctimes, float_spreads, fix_cpn;
    std::vector<Date> floatt1Dates, floatt2Dates, floattpDates;
    std::vector<Date> fixtpDates;

    for (Size i = 0; i < arguments_.floatingFixingDates.size(); ++i) {
        float_mults.push_back(arguments_.nominal *
                              arguments_.floatingAccrualTimes[i]);
        float_spreads.push_back(arguments_.floatingSpreads[i]);
        boost::shared_ptr<IborIndex> index = arguments_.swap->iborIndex();
        Date d1 = index->valueDate(arguments_.floatingFixingDates[i]);
        Date d2 = index->maturityDate(d1);
        double acctime = index->dayCounter().yearFraction(d1, d2, d1, d2);
        floatt1Dates.push_back(d1);
        floatt2Dates.push_back(d2);
        floattpDates.push_back(arguments_.floatingPayDates[i]);
        index_acctimes.push_back(acctime);
    }

    for (Size i = 0; i < arguments_.fixedCoupons.size(); ++i) {
        fix_cpn.push_back(arguments_.fixedCoupons[i]);
        fixtpDates.push_back(arguments_.fixedPayDates[i]);
    }

    // join all dates and fill index vectors

    std::vector<Date> allDates;
    std::vector<double> allTimes; // with settlement as first entry !
    std::vector<int> expiries, floatt1s, floatt2s, floattps, fixtps;
    std::vector<double> modpar;

    allDates.reserve(expiryDates.size() + floatt1Dates.size() +
                     floatt2Dates.size() + floattpDates.size() +
                     fixtpDates.size());

    allTimes.reserve(allDates.size());
    modpar.reserve(3 * allDates.size());

    allDates.push_back(settlement);
    allDates.insert(allDates.end(), expiryDates.begin(), expiryDates.end());
    allDates.insert(allDates.end(), floatt1Dates.begin(), floatt1Dates.end());
    allDates.insert(allDates.end(), floatt2Dates.begin(), floatt2Dates.end());
    allDates.insert(allDates.end(), floattpDates.begin(), floattpDates.end());
    allDates.insert(allDates.end(), fixtpDates.begin(), fixtpDates.end());

    std::sort(allDates.begin(), allDates.end());
    allDates.erase(unique(allDates.begin(), allDates.end()), allDates.end());

    for (Size i = 0; i < allDates.size(); ++i) {
        allTimes.push_back(
            model_->termStructure()->timeFromReference(allDates[i]));
    }

    for (Size i = 0; i < allDates.size(); ++i) {
        modpar.push_back(model_->parametrization()->H(allTimes[i]));
    }
    for (Size i = 0; i < allDates.size(); ++i) {
        modpar.push_back(model_->parametrization()->zeta(allTimes[i]));
    }
    for (Size i = 0; i < allDates.size(); ++i) {
        modpar.push_back(model_->termStructure()->discount(allTimes[i]));
    }

    for (Size i = 0; i < expiryDates.size(); ++i) {
        expiries.push_back(
            std::find(allDates.begin(), allDates.end(), expiryDates[i]) -
            allDates.begin());
    }
    for (Size i = 0; i < floatt1Dates.size(); ++i) {
        floatt1s.push_back(
            std::find(allDates.begin(), allDates.end(), floatt1Dates[i]) -
            allDates.begin());
    }
    for (Size i = 0; i < floatt2Dates.size(); ++i) {
        floatt2s.push_back(
            std::find(allDates.begin(), allDates.end(), floatt2Dates[i]) -
            allDates.begin());
    }
    for (Size i = 0; i < floattpDates.size(); ++i) {
        floattps.push_back(
            std::find(allDates.begin(), allDates.end(), floattpDates[i]) -
            allDates.begin());
    }
    for (Size i = 0; i < fixtpDates.size(); ++i) {
        fixtps.push_back(
            std::find(allDates.begin(), allDates.end(), fixtpDates[i]) -
            allDates.begin());
    }

    // call core computation routine and set results

    int ntimes = allTimes.size();
    int nexpiries = expiries.size();
    int nfloats = floatt1s.size();
    int nfixs = fix_cpn.size();

    double res = 0.0;
    int integration_pts = integrationPoints_;
    double std_devs = stddevs_;
    lgm_swaption_engine_(&ntimes, &allTimes[0], &modpar[0], &nexpiries,
                         &expiries[0], &callput, &nfloats, &float_startidxes[0],
                         &float_mults[0], &index_acctimes[0], &float_spreads[0],
                         &floatt1s[0], &floatt2s[0], &floattps[0],
                         &fix_startidxes[0], &nfixs, &fix_cpn[0], &fixtps[0],
                         &integration_pts, &std_devs, &res);

    results_.value = res;
}

} // namespace QuantLib
