
/*
 Copyright (C) 2000-2004 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef quantlib_currencies_i
#define quantlib_currencies_i

%include common.i
%include types.i
%include rounding.i

// currency objects

%{
using QuantLib::Currency;
%}

class Currency {
    #if defined(SWIGPYTHON)
    %rename(__nonzero__) isValid;
    #elif defined(SWIGRUBY)
    %rename("isValid?") isValid;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("numeric-code")           numericCode;
    %rename("fraction-symbol")        fractionSymbol;
    %rename("fractions-per-unit")     fractionsPerUnit;
    %rename("is-valid?")              isValid;
    %rename("triangulation-currency") triangulationCurrency;
    %rename(">string")                __str__;
    #endif
  public:
    const std::string& name() const;
    const std::string& code() const;
    Integer numericCode() const;
    const std::string& symbol() const;
    const std::string& fractionSymbol() const;
    Integer fractionsPerUnit() const;
    const Rounding& rounding() const;
    bool isValid() const;
    const Currency& triangulationCurrency() const;
    %extend {
        std::string __str__() {
            return self->name();
        }
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        bool __eq__(const Currency& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON)
        bool __ne__(const Currency& other) {
            return (*self) != other;
        }
        #endif
        #endif
    }
};

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("Currency=?") Currency_equal;
%inline %{
    bool Currency_equal(const Currency& c1, const Currency& c2) {
        return c1 == c2;
    }
%}
#endif

namespace QuantLib {
class ARSCurrency : public Currency {};
class ATSCurrency : public Currency {};
class AUDCurrency : public Currency {};
class BDTCurrency : public Currency {};
class BEFCurrency : public Currency {};
class BGLCurrency : public Currency {};
class BRLCurrency : public Currency {};
class BYRCurrency : public Currency {};
class CADCurrency : public Currency {};
class CHFCurrency : public Currency {};
class CLPCurrency : public Currency {};
class CNYCurrency : public Currency {};
class COPCurrency : public Currency {};
class CYPCurrency : public Currency {};
class CZKCurrency : public Currency {};
class DEMCurrency : public Currency {};
class DKKCurrency : public Currency {};
class EEKCurrency : public Currency {};
class ESPCurrency : public Currency {};
class EURCurrency : public Currency {};
class FIMCurrency : public Currency {};
class FRFCurrency : public Currency {};
class GBPCurrency : public Currency {};
class GRDCurrency : public Currency {};
class HKDCurrency : public Currency {};
class HUFCurrency : public Currency {};
class IEPCurrency : public Currency {};
class ILSCurrency : public Currency {};
class INRCurrency : public Currency {};
class IQDCurrency : public Currency {};
class IRRCurrency : public Currency {};
class ISKCurrency : public Currency {};
class ITLCurrency : public Currency {};
class JPYCurrency : public Currency {};
class KRWCurrency : public Currency {};
class KWDCurrency : public Currency {};
class LTLCurrency : public Currency {};
class LUFCurrency : public Currency {};
class LVLCurrency : public Currency {};
class MTLCurrency : public Currency {};
class MXNCurrency : public Currency {};
class NLGCurrency : public Currency {};
class NOKCurrency : public Currency {};
class NPRCurrency : public Currency {};
class NZDCurrency : public Currency {};
class PKRCurrency : public Currency {};
class PLNCurrency : public Currency {};
class PTECurrency : public Currency {};
class ROLCurrency : public Currency {};
class SARCurrency : public Currency {};
class SEKCurrency : public Currency {};
class SGDCurrency : public Currency {};
class SITCurrency : public Currency {};
class SKKCurrency : public Currency {};
class THBCurrency : public Currency {};
class TRLCurrency : public Currency {};
class TTDCurrency : public Currency {};
class TWDCurrency : public Currency {};
class USDCurrency : public Currency {};
class VEBCurrency : public Currency {};
class ZARCurrency : public Currency {};
}

// currency tags

%{
using QuantLib::CurrencyTag;
using QuantLib::CurrencyFormatter;
%}

%{
CurrencyTag currtagFromString(std::string s) {
    s = StringFormatter::toUppercase(s);
    if (s == "ARS")      return QuantLib::ARS;
    else if (s == "ATS") return QuantLib::ATS;
    else if (s == "AUD") return QuantLib::AUD;
    else if (s == "BDT") return QuantLib::BDT;
    else if (s == "BEF") return QuantLib::BEF;
    else if (s == "BGL") return QuantLib::BGL;
    else if (s == "BRL") return QuantLib::BRL;
    else if (s == "BYB") return QuantLib::BYB;
    else if (s == "CAD") return QuantLib::CAD;
    else if (s == "CHF") return QuantLib::CHF;
    else if (s == "CLP") return QuantLib::CLP;
    else if (s == "CNY") return QuantLib::CNY;
    else if (s == "COP") return QuantLib::COP;
    else if (s == "CYP") return QuantLib::CYP;
    else if (s == "CZK") return QuantLib::CZK;
    else if (s == "DEM") return QuantLib::DEM;
    else if (s == "DKK") return QuantLib::DKK;
    else if (s == "EEK") return QuantLib::EEK;
    else if (s == "EUR") return QuantLib::EUR;
    else if (s == "GBP") return QuantLib::GBP;
    else if (s == "GRD") return QuantLib::GRD;
    else if (s == "HKD") return QuantLib::HKD;
    else if (s == "HUF") return QuantLib::HUF;
    else if (s == "ILS") return QuantLib::ILS;
    else if (s == "INR") return QuantLib::INR;
    else if (s == "IQD") return QuantLib::IQD;
    else if (s == "IRR") return QuantLib::IRR;
    else if (s == "ISK") return QuantLib::ISK;
    else if (s == "ITL") return QuantLib::ITL;
    else if (s == "JPY") return QuantLib::JPY;
    else if (s == "KRW") return QuantLib::KRW;
    else if (s == "KWD") return QuantLib::KWD;
    else if (s == "LTL") return QuantLib::LTL;
    else if (s == "LVL") return QuantLib::LVL;
    else if (s == "MTL") return QuantLib::MTL;
    else if (s == "MXP") return QuantLib::MXP;
    else if (s == "NOK") return QuantLib::NOK;
    else if (s == "NPR") return QuantLib::NPR;
    else if (s == "NZD") return QuantLib::NZD;
    else if (s == "PKR") return QuantLib::PKR;
    else if (s == "PLN") return QuantLib::PLN;
    else if (s == "ROL") return QuantLib::ROL;
    else if (s == "SAR") return QuantLib::SAR;
    else if (s == "SEK") return QuantLib::SEK;
    else if (s == "SGD") return QuantLib::SGD;
    else if (s == "SIT") return QuantLib::SIT;
    else if (s == "SKK") return QuantLib::SKK;
    else if (s == "THB") return QuantLib::THB;
    else if (s == "TRL") return QuantLib::TRL;
    else if (s == "TTD") return QuantLib::TTD;
    else if (s == "TWD") return QuantLib::TWD;
    else if (s == "USD") return QuantLib::USD;
    else if (s == "VEB") return QuantLib::VEB;
    else if (s == "ZAR") return QuantLib::ZAR;
    else QL_FAIL("unknown currency: " + s);
}

std::string currtagToString(CurrencyTag c) {
    return CurrencyFormatter::toString(c);

}
%}

MapToString(CurrencyTag,currtagFromString,currtagToString);


#endif
