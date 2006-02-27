
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

%template(PricingEngine) boost::shared_ptr<PricingEngine>;

// instrument

%{
using QuantLib::Instrument;
%}

%ignore Instrument;
class Instrument {
    #if defined(SWIGRUBY)
    %rename("isExpired?")     isExpired;
    %rename("pricingEngine=") setPricingEngine;
    %rename("recalculate!")   recalculate;
    %rename("freeze!")        freeze;
    %rename("unfreeze!")      unfreeze;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("error-estimate")      errorEstimate;
    %rename("expired?")            isExpired;
    %rename("pricing-engine-set!") setPricingEngine;
    %rename("recalculate!")        recalculate;
    %rename("freeze!")             freeze;
    %rename("unfreeze!")           unfreeze;
    #endif
  public:
    Real NPV() const;
    Real errorEstimate() const;
    bool isExpired() const;
    void setPricingEngine(const boost::shared_ptr<PricingEngine>&);
    void recalculate();
    void freeze();
    void unfreeze();
};

#if defined(SWIGR)
%Rruntime %{
setMethod("summary", "_p_boost__shared_ptrTInstrument_t",
function(object) c(value=object$NPV()))
print._p_boost__shared_ptrTInstrument_t <-
function(object) print(summary(object))
%}
#endif

%template(Instrument) boost::shared_ptr<Instrument>;
IsObservable(boost::shared_ptr<Instrument>);

// actual instruments

%{
using QuantLib::Stock;
typedef boost::shared_ptr<Instrument> StockPtr;
%}

%rename(Stock) StockPtr;
class StockPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        StockPtr(const Handle<Quote>& quote) {
            return new StockPtr(new Stock(quote));
        }
    }
};


#endif
