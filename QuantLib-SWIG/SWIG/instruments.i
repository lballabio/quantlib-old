
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_instruments_i
#define quantlib_instruments_i

%include common.i
%include types.i
%include marketelements.i
%include observer.i
%include stl.i

// pricing engine

%{
using QuantLib::PricingEngine;
%}

%template(PricingEngine) Handle<PricingEngine>;

// instrument 

%{
using QuantLib::Instrument;
%}

%ignore Instrument;
class Instrument {
    #if defined(SWIGRUBY)
    %rename("isExpired?")   isExpired;
    %rename("recalculate!") recalculate;
    %rename("freeze!")      freeze;
    %rename("unfreeze!")    unfreeze;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("expired?")     isExpired;
    %rename("recalculate!") recalculate;
    %rename("freeze!")      freeze;
    %rename("unfreeze!")    unfreeze;
    #endif
  public:
    double NPV() const;
    bool isExpired() const;
    void setPricingEngine(const Handle<PricingEngine>&);
    void recalculate();
    void freeze();
    void unfreeze();
};

%template(Instrument) Handle<Instrument>;
IsObservable(Handle<Instrument>);

// actual instruments

%{
using QuantLib::Stock;
typedef Handle<Instrument> StockHandle;
%}

// Fake inheritance between Handles

%rename(Stock) StockHandle;
class StockHandle : public Handle<Instrument> {
  public:
    %extend {
        StockHandle(const RelinkableHandle<Quote>& quote) {
            return new StockHandle(new Stock(quote));
        }
    }
};


#endif
