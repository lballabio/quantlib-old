
/*
 Copyright (C) 2000-2005 StatPro Italia srl
 Copyright (C) 2005 Johan Witters

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
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGJAVA)
        bool __eq__(const Currency& other) {
            return (*self) == other;
        }
        #if defined(SWIGPYTHON) || defined(SWIGJAVA)
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
class ARS : public Currency {};
class ARSCurrency : public Currency {};
class ATS : public Currency {};
class ATSCurrency : public Currency {};
class AUD : public Currency {};
class AUDCurrency : public Currency {};
class BDT : public Currency {};
class BDTCurrency : public Currency {};
class BEF : public Currency {};
class BEFCurrency : public Currency {};
class BGL : public Currency {};
class BGLCurrency : public Currency {};
class BRL : public Currency {};
class BRLCurrency : public Currency {};
class BYR : public Currency {};
class BYRCurrency : public Currency {};
class CAD : public Currency {};
class CADCurrency : public Currency {};
class CHF : public Currency {};
class CHFCurrency : public Currency {};
class CLP : public Currency {};
class CLPCurrency : public Currency {};
class CNY : public Currency {};
class CNYCurrency : public Currency {};
class COP : public Currency {};
class COPCurrency : public Currency {};
class CYP : public Currency {};
class CYPCurrency : public Currency {};
class CZK : public Currency {};
class CZKCurrency : public Currency {};
class DEM : public Currency {};
class DEMCurrency : public Currency {};
class DKK : public Currency {};
class DKKCurrency : public Currency {};
class EEK : public Currency {};
class EEKCurrency : public Currency {};
class ESP : public Currency {};
class ESPCurrency : public Currency {};
class EUR : public Currency {};
class EURCurrency : public Currency {};
class FIM : public Currency {};
class FIMCurrency : public Currency {};
class FRF : public Currency {};
class FRFCurrency : public Currency {};
class GBP : public Currency {};
class GBPCurrency : public Currency {};
class GRD : public Currency {};
class GRDCurrency : public Currency {};
class HKD : public Currency {};
class HKDCurrency : public Currency {};
class HUF : public Currency {};
class HUFCurrency : public Currency {};
class IEP : public Currency {};
class IEPCurrency : public Currency {};
class ILS : public Currency {};
class ILSCurrency : public Currency {};
class INR : public Currency {};
class INRCurrency : public Currency {};
class IQD : public Currency {};
class IQDCurrency : public Currency {};
class IRR : public Currency {};
class IRRCurrency : public Currency {};
class ISK : public Currency {};
class ISKCurrency : public Currency {};
class ITL : public Currency {};
class ITLCurrency : public Currency {};
class JPY : public Currency {};
class JPYCurrency : public Currency {};
class KRW : public Currency {};
class KRWCurrency : public Currency {};
class KWD : public Currency {};
class KWDCurrency : public Currency {};
class LTL : public Currency {};
class LTLCurrency : public Currency {};
class LUF : public Currency {};
class LUFCurrency : public Currency {};
class LVL : public Currency {};
class LVLCurrency : public Currency {};
class MTL : public Currency {};
class MTLCurrency : public Currency {};
class MXN : public Currency {};
class MXNCurrency : public Currency {};
class NLG : public Currency {};
class NLGCurrency : public Currency {};
class NOK : public Currency {};
class NOKCurrency : public Currency {};
class NPR : public Currency {};
class NPRCurrency : public Currency {};
class NZD : public Currency {};
class NZDCurrency : public Currency {};
class PKR : public Currency {};
class PKRCurrency : public Currency {};
class PLN : public Currency {};
class PLNCurrency : public Currency {};
class PTE : public Currency {};
class PTECurrency : public Currency {};
class ROL : public Currency {};
class ROLCurrency : public Currency {};
class SAR : public Currency {};
class SARCurrency : public Currency {};
class SEK : public Currency {};
class SEKCurrency : public Currency {};
class SGD : public Currency {};
class SGDCurrency : public Currency {};
class SIT : public Currency {};
class SITCurrency : public Currency {};
class SKK : public Currency {};
class SKKCurrency : public Currency {};
class THB : public Currency {};
class THBCurrency : public Currency {};
class TRL : public Currency {};
class TRLCurrency : public Currency {};
class TRY : public Currency {};
class TRYCurrency : public Currency {};
class TTD : public Currency {};
class TTDCurrency : public Currency {};
class TWD : public Currency {};
class TWDCurrency : public Currency {};
class USD : public Currency {};
class USDCurrency : public Currency {};
class VEB : public Currency {};
class VEBCurrency : public Currency {};
class ZAR : public Currency {};
class ZARCurrency : public Currency {};
}


#endif
