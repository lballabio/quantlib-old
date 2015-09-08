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

#include <ql/experimental/fx/fxtarf_helperquotes.hpp>
#include <ql/currencies/exchangeratemanager.hpp>
#include <ql/event.hpp>

namespace QuantLib {

FxTarfAccumulatedAmountHelper::FxTarfAccumulatedAmountHelper(
    const boost::shared_ptr<FxTarf> &instrument)
    : instrument_(instrument) {}

Real FxTarfAccumulatedAmountHelper::value() const {
    std::vector<Date> d = instrument_->fixingDates();
    Size i = 0;
    Real tmp = 0.0;
    while (i < d.size() && detail::simple_event(d[i]).hasOccurred()) {
        Real fixing = ExchangeRateManager::instance()
                          .lookup(instrument_->index()->sourceCurrency(),
                                  instrument_->index()->targetCurrency(), d[i])
                          .rate();
        instrument_->payout(fixing, tmp);
    }
    return tmp;
}

bool FxTarfAccumulatedAmountHelper::isValid() const { return true; }

FxTarfLastAmountHelper::FxTarfLastAmountHelper(
    const boost::shared_ptr<FxTarf> &instrument)
    : instrument_(instrument) {}

Real FxTarfLastAmountHelper::value() const {
    std::vector<Date> d = instrument_->fixingDates();
    Size i = 0;
    while (i < d.size() && detail::simple_event(d[i]).hasOccurred()) {
        ++i;
    }
    if (i == 0) {
        return 0.0;
    }
    Real fixing = ExchangeRateManager::instance()
                      .lookup(instrument_->index()->sourceCurrency(),
                              instrument_->index()->targetCurrency(), d[i - 1])
                      .rate();
    return instrument_->payout(fixing);
}

bool FxTarfLastAmountHelper::isValid() const { return true; }

} // namespace QuantLib
