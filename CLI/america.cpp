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

#include <ql/currencies/america.hpp>

namespace QuantLib {

    namespace {

        struct null_deleter {
            void operator()(void const *) const {
            }
        };

    }

    Currency::Data ARSCurrency::arsData_("Argentinian peso", "ARS", 32,
                                         "", "", 100,
                                         Rounding(),
                                         "%2% %1$.2f");

    Currency::Data BRLCurrency::brlData_("Brazilian real", "BRL", 986,
                                         "R$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data CADCurrency::cadData_("Canadian dollar", "CAD", 124,
                                         "Can$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data CLPCurrency::clpData_("Chilean peso", "CLP", 152,
                                         "Ch$", "", 100,
                                         Rounding(),
                                         "%3% %1$.0f");

    Currency::Data COPCurrency::copData_("Colombian peso", "COP", 170,
                                         "Col$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data MXNCurrency::mxnData_("Mexican peso", "MXN", 484,
                                         "Mex$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data PEHCurrency::pehData_("Peruvian sol", "PEH", 999,
                                         "S./", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data PEICurrency::peiData_("Peruvian inti", "PEI", 998,
                                         "I/.", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data PENCurrency::penData_("Peruvian nuevo sol", "PEN", 604,
                                         "S/.", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data TTDCurrency::ttdData_("Trinidad & Tobago dollar", "TTD", 780,
                                         "TT$", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data USDCurrency::usdData_("U.S. dollar", "USD", 840,
                                        "$", "\xA2", 100,
                                        Rounding(),
                                        "%3% %1$.2f");

    Currency::Data VEBCurrency::vebData_("Venezuelan bolivar", "VEB", 862,
                                         "Bs", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    ARSCurrency::ARSCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&arsData_, null_deleter());
    }

    BRLCurrency::BRLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&brlData_, null_deleter());
    }

    CADCurrency::CADCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&cadData_, null_deleter());
    }

    CLPCurrency::CLPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&clpData_, null_deleter());
    }

    COPCurrency::COPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&copData_, null_deleter());
    }

    MXNCurrency::MXNCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&mxnData_, null_deleter());
    }

    PEHCurrency::PEHCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&pehData_, null_deleter());
    }

    PEICurrency::PEICurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&peiData_, null_deleter());
    }

    PENCurrency::PENCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&penData_, null_deleter());
    }

    TTDCurrency::TTDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&ttdData_, null_deleter());
    }

    USDCurrency::USDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&usdData_, null_deleter());
    }

    VEBCurrency::VEBCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&vebData_, null_deleter());
    }
}
