/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2008 StatPro Italia srl
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

#include <ql/currencies/asia.hpp>

namespace QuantLib {

    namespace {

        struct null_deleter {
            void operator()(void const *) const {
            }
        };

    }

    Currency::Data BDTCurrency::bdtData_("Bangladesh taka", "BDT", 50,
                                         "Bt", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data CNYCurrency::cnyData_("Chinese yuan", "CNY", 156,
                                         "Y", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data HKDCurrency::hkdData_("Honk Kong dollar", "HKD", 344,
                                         "HK$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data ILSCurrency::ilsData_("Israeli shekel", "ILS", 376,
                                         "NIS", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data INRCurrency::inrData_("Indian rupee", "INR", 356,
                                         "Rs", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data IQDCurrency::iqdData_("Iraqi dinar", "IQD", 368,
                                         "ID", "", 1000,
                                         Rounding(),
                                         "%2% %1$.3f");

    Currency::Data IRRCurrency::irrData_("Iranian rial", "IRR", 364,
                                         "Rls", "", 1,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data JPYCurrency::jpyData_("Japanese yen", "JPY", 392,
                                                   "\xA5", "", 100,
                                                   Rounding(),
                                                   "%3% %1$.0f");

    Currency::Data KRWCurrency::krwData_("South-Korean won", "KRW", 410,
                                         "W", "", 100,
                                         Rounding(),
                                         "%3% %1$.0f");

    Currency::Data KWDCurrency::kwdData_("Kuwaiti dinar", "KWD", 414,
                                         "KD", "", 1000,
                                         Rounding(),
                                         "%3% %1$.3f");

    Currency::Data NPRCurrency::nprData_("Nepal rupee", "NPR", 524,
                                         "NRs", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data PKRCurrency::pkrData_("Pakistani rupee", "PKR", 586,
                                         "Rs", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data SARCurrency::sarData_("Saudi riyal", "SAR", 682,
                                         "SRls", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data SGDCurrency::sgdData_("Singapore dollar", "SGD", 702,
                                         "S$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data THBCurrency::thbData_("Thai baht", "THB", 764,
                                         "Bht", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data TWDCurrency::twdData_("Taiwan dollar", "TWD", 901,
                                                  "NT$", "", 100,
                                                  Rounding(),
                                                  "%3% %1$.2f");

    BDTCurrency::BDTCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&bdtData_, null_deleter());
    }

    CNYCurrency::CNYCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&cnyData_, null_deleter());
    }

    HKDCurrency::HKDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&hkdData_, null_deleter());
    }

    ILSCurrency::ILSCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&ilsData_, null_deleter());
    }

    INRCurrency::INRCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&inrData_, null_deleter());
    }

    IQDCurrency::IQDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&iqdData_, null_deleter());
    }

    IRRCurrency::IRRCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&irrData_, null_deleter());
    }

    JPYCurrency::JPYCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&jpyData_, null_deleter());
    }

    KRWCurrency::KRWCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&krwData_, null_deleter());
    }

    KWDCurrency::KWDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&kwdData_, null_deleter());
    }

    NPRCurrency::NPRCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&nprData_, null_deleter());
    }

    PKRCurrency::PKRCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&pkrData_, null_deleter());
    }

    SARCurrency::SARCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&sarData_, null_deleter());
    }

    SGDCurrency::SGDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&sgdData_, null_deleter());
    }

    THBCurrency::THBCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&thbData_, null_deleter());
    }

    TWDCurrency::TWDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&twdData_, null_deleter());
    }

}
