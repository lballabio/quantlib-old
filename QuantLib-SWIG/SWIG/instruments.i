
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

#ifndef quantlib_instruments_i
#define quantlib_instruments_i

%include types.i
%include marketelements.i
%include observer.i
%include stl.i

%{
using QuantLib::Instrument;
using QuantLib::Handle;
typedef Handle<Observable> InstrumentHandle;
%}

// Export Handle<Instrument>
%rename(Instrument) InstrumentHandle;
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("isin-code")    isinCode;
%rename(">string")      __str__;
%rename("expired?")     isExpired;
%rename("recalculate!") recalculate;
#elif defined(SWIGRUBY)
%rename("isExpired?")   isExpired;
%rename("recalculate!") recalculate;
#endif


class InstrumentHandle : public ObservableHandle {
  private:
    InstrumentHandle();
};

// replicate the Instrument interface
%addmethods InstrumentHandle {
	std::string isinCode() {
		return Handle<Instrument>(*self)->isinCode();
	}
	std::string description() {
		return Handle<Instrument>(*self)->description();
	}
	double NPV() {
		return Handle<Instrument>(*self)->NPV();
	}
	bool isExpired() {
		return Handle<Instrument>(*self)->isExpired();
	}
	void recalculate() {
		Handle<Instrument>(*self)->recalculate();
	}
	std::string __str__() {
        Handle<Instrument> h(*self);
	    if (h.isNull())
	        return "Null instrument";
    	std::string isin = h->isinCode();
    	if (isin == "")
    		isin = "unknown";
    	std::string desc = h->description();
    	if (desc == "")
    		desc = "no description available";
    	return ("Instrument: "+isin+" ("+desc+")");
	}
}


// actual instruments

%{
using QuantLib::Instruments::Stock;
typedef Handle<Observable> StockHandle;
std::string StockDefaultIsinCode = "unknown";
std::string StockDefaultDescription = "stock";
%}

// Fake inheritance between Handles

%rename(Stock) StockHandle;
class StockHandle : public InstrumentHandle {};

%addmethods StockHandle {
    StockHandle(const MarketElementRelinkableHandle& quote,
                const std::string& isinCode = StockDefaultIsinCode, 
                const std::string& description = StockDefaultDescription) {
        return new StockHandle(new Stock(quote,isinCode,description));
    }
}


#endif
