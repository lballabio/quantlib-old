
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_currencies_i
#define quantlib_currencies_i

%include common.i

%{
using QuantLib::Currency;
using QuantLib::EUR;
using QuantLib::USD;
using QuantLib::GBP;
using QuantLib::DEM;
using QuantLib::ITL;
using QuantLib::CHF;
using QuantLib::AUD;
using QuantLib::CAD;
using QuantLib::DKK;
using QuantLib::JPY;
using QuantLib::PLZ;
using QuantLib::SEK;
using QuantLib::CZK;
using QuantLib::EEK;
using QuantLib::ISK;
using QuantLib::NOK;
using QuantLib::SKK;
using QuantLib::HKD;
using QuantLib::NZD;
using QuantLib::SGD;
using QuantLib::GRD;
using QuantLib::HUF;
using QuantLib::LVL;
using QuantLib::ROL;
using QuantLib::BGL;
using QuantLib::CYP;
using QuantLib::LTL;
using QuantLib::MTL;
using QuantLib::TRL;
using QuantLib::ZAR;
using QuantLib::SIT;
using QuantLib::KRW;
using QuantLib::StringFormatter;
using QuantLib::CurrencyFormatter;

Currency currencyFromString(string s) {
    s = StringFormatter::toUppercase(s);
    if (s == "AUD")      return AUD;
    else if (s == "BGL") return BGL;
    else if (s == "CAD") return CAD;
    else if (s == "CHF") return CHF;
    else if (s == "CYP") return CYP;
    else if (s == "CZK") return CZK;
    else if (s == "DEM") return DEM;
    else if (s == "DKK") return DKK;
    else if (s == "EEK") return EEK;
    else if (s == "EUR") return EUR;
    else if (s == "GBP") return GBP;
    else if (s == "GRD") return GRD;
    else if (s == "HKD") return HKD;
    else if (s == "HUF") return HUF;
    else if (s == "ISK") return ISK;
    else if (s == "ITL") return ITL;
    else if (s == "JPY") return JPY;
    else if (s == "KRW") return KRW;
    else if (s == "LTL") return LTL;
    else if (s == "LVL") return LVL;
    else if (s == "MTL") return MTL;
    else if (s == "NOK") return NOK;
    else if (s == "NZD") return NZD;
    else if (s == "PLZ") return PLZ;
    else if (s == "ROL") return ROL;
    else if (s == "SEK") return SEK;
    else if (s == "SGD") return SGD;
    else if (s == "SIT") return SIT;
    else if (s == "SKK") return SKK;
    else if (s == "TRL") return TRL;
    else if (s == "USD") return USD;
    else if (s == "ZAR") return ZAR;
    else throw Error("unknown currency");
}

string currencyToString(Currency c) {
    return CurrencyFormatter::toString(c);
}
%}

MapToString(Currency,currencyFromString,currencyToString);


#endif
