
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
%rename("null?") isNull;
%rename("toObservable") asObservable;
%rename("isExpired?")   isExpired;
%rename("recalculate!") recalculate;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("null?") isNull;
%rename(">Observable") asObservable;
%rename("isin-code")    isinCode;
%rename(">string")      __str__;
%rename("expired?")     isExpired;
%rename("recalculate!") recalculate;
#endif
template <>
class Handle<Instrument> {
  public:
    #if defined(SWIGRUBY) || defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    bool isNull();
    #endif
    %addmethods {
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
            Handle<Instrument> h(*self);
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
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        Handle<Observable> asObservable() {
            return Handle<Observable>(*self);
        }
        #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        Handle<Observable>* asObservable() {
            return new Handle<Observable>(*self);
        }
        #endif
        #if defined(SWIGPYTHON) 
        bool __nonzero__() {
            return !(self->isNull());
        }
        #endif
    }
};
%template(Instrument) Handle<Instrument>;

/*
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("isin-code")    isinCode;
%rename(">string")      __str__;
%rename("expired?")     isExpired;
%rename("recalculate!") recalculate;
#elif defined(SWIGRUBY)
%rename("isExpired?")   isExpired;
%rename("recalculate!") recalculate;
#endif
%template(Instrument) Handle<Instrument>;
IsObservable(Handle<Instrument>);
%addmethods Handle<Instrument> {
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
        Handle<Instrument> h(*self);
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
*/

// actual instruments

%{
using QuantLib::Instruments::Stock;
typedef Handle<Instrument> StockHandle;
std::string StockDefaultIsinCode = "unknown";
std::string StockDefaultDescription = "stock";
%}

// Fake inheritance between Handles

%rename(Stock) StockHandle;
class StockHandle : public Handle<Instrument> {};
%addmethods StockHandle {
    StockHandle(const RelinkableHandle<MarketElement>& quote,
                const std::string& isinCode = StockDefaultIsinCode, 
                const std::string& description = StockDefaultDescription) {
        return new StockHandle(new Stock(quote,isinCode,description));
    }
}


#endif
