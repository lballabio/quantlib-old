/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005 StatPro Italia srl
 Copyright (C) 2012 Simon Shakeshaft

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

#include <ql/currencies/oceania.hpp>

namespace QuantLib {

    namespace {

        struct null_deleter {
            void operator()(void const *) const {
            }
        };

    }

    Currency::Data AUDCurrency::audData_("Australian dollar", "AUD", 36,
                                         "A$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data NZDCurrency::nzdData_("New Zealand dollar", "NZD", 554,
                                         "NZ$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    AUDCurrency::AUDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&audData_, null_deleter());
    }

    NZDCurrency::NZDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&nzdData_, null_deleter());
    }

}