
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
class TRYCurrency : public Currency {};
class TTDCurrency : public Currency {};
class TWDCurrency : public Currency {};
class USDCurrency : public Currency {};
class VEBCurrency : public Currency {};
class ZARCurrency : public Currency {};
}


#endif
