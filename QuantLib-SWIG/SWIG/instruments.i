
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

%include common.i
%include types.i
%include marketelements.i
%include observer.i
%include stl.i

%{
using QuantLib::Instrument;
%}

#if defined(SWIGRUBY)
%rename("isExpired?")   Handle<Instrument>::isExpired;
%rename("recalculate!") Handle<Instrument>::recalculate;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("isin-code")    Handle<Instrument>::isinCode;
%rename(">string")      Handle<Instrument>::__str__;
%rename("expired?")     Handle<Instrument>::isExpired;
%rename("recalculate!") Handle<Instrument>::recalculate;
#endif
%template(Instrument) Handle<Instrument>;
IsObservable(Handle<Instrument>);
%extend Handle<Instrument> {
	std::string isinCode() {
		return (*self)->isinCode();
	}
	std::string description() {
		return (*self)->description();
	}
	double NPV() {
		return (*self)->NPV();
	}
	bool isExpired() {
		return (*self)->isExpired();
	}
	void recalculate() {
		(*self)->recalculate();
	}
	std::string __str__() {
	    if (self->isNull())
	        return "Null instrument";
    	std::string isin = (*self)->isinCode();
    	if (isin == "")
    		isin = "unknown";
    	std::string desc = (*self)->description();
    	if (desc == "")
    		desc = "no description available";
    	return ("Instrument: "+isin+" ("+desc+")");
	}
}


// actual instruments

%{
using QuantLib::Instruments::Stock;
typedef Handle<Instrument> StockHandle;
%}

// Fake inheritance between Handles

%rename(Stock) StockHandle;
class StockHandle : public Handle<Instrument> {};
%extend StockHandle {
    StockHandle(const RelinkableHandle<MarketElement>& quote,
                const std::string& isinCode = "unknown", 
                const std::string& description = "stock") {
        return new StockHandle(new Stock(quote,isinCode,description));
    }
}


#endif
