
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

#ifndef quantlib_market_elements_i
#define quantlib_market_elements_i

%include observer.i
%include functions.i

%{
using QuantLib::MarketElement;
using QuantLib::Handle;
using QuantLib::RelinkableHandle;
typedef Handle<MarketElement> MarketElementHandle;
typedef RelinkableHandle<MarketElement> MarketElementRelinkableHandle;
MarketElementHandle NullMarketElement;
%}

// Export handles
%rename(MarketElement) MarketElementHandle;
%rename(MarketElementHandle) MarketElementRelinkableHandle;
#if defined(SWIGPYTHON)
%rename(__nonzero__) isNull;
#elif defined(SWIGRUBY)
%rename("null?")   isNull;
%rename("linkTo!") linkTo;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("null?")    isNull;
%rename("link-to!") linkTo;
#endif

class MarketElementHandle : public ObservableHandle {
  private:
    MarketElementHandle();
    #if defined(SWIGRUBY) || defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    bool isNull() const;
    #endif
};

%addmethods MarketElementHandle {
	double value() {
		return (*self)->value();
	}
    #if defined(SWIGPYTHON)
    bool __nonzero__() {
        return !self->isNull();
    }
    #endif
}

class MarketElementRelinkableHandle {
  public:
    MarketElementRelinkableHandle(
        const MarketElementHandle& h = NullMarketElement);
	void linkTo(const MarketElementHandle&);
    #if defined(SWIGRUBY) || defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    bool isNull() const;
    #endif
};

%addmethods MarketElementRelinkableHandle {
    #if defined(SWIGPYTHON)
    bool __nonzero__() {
        return !self->isNull();
    }
    #endif
}


// actual market elements
%{
using QuantLib::SimpleMarketElement;
typedef Handle<SimpleMarketElement> SimpleMarketElementHandle;
%}

#if defined(SWIGRUBY)
%rename("value=") setValue;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("value-set!") setValue;
#endif

// Fake inheritance between Handles
%rename(SimpleMarketElement) SimpleMarketElementHandle;
class SimpleMarketElementHandle : public MarketElementHandle {};

%addmethods SimpleMarketElementHandle {
    SimpleMarketElementHandle(double value) {
        return new SimpleMarketElementHandle(
            new SimpleMarketElement(value));
    }
    void setValue(double value) {
        (*self)->setValue(value);
    }
}


#if defined(SWIGPYTHON) || defined(SWIGMZSCHEME)
%{
using QuantLib::DerivedMarketElement;
using QuantLib::CompositeMarketElement;
typedef Handle<DerivedMarketElement<UnaryFunction> >
    DerivedMarketElementHandle;
typedef Handle<CompositeMarketElement<BinaryFunction> > 
    CompositeMarketElementHandle;
%}

%rename(DerivedMarketElement) DerivedMarketElementHandle;
class DerivedMarketElementHandle : public MarketElementHandle {};
%rename(CompositeMarketElement) CompositeMarketElementHandle;
class CompositeMarketElementHandle : public MarketElementHandle {};

%addmethods DerivedMarketElementHandle {
    #if defined(SWIGPYTHON)
    DerivedMarketElementHandle(const MarketElementRelinkableHandle& h,
                               PyObject* function) {
        return new DerivedMarketElementHandle(
            new DerivedMarketElement<UnaryFunction>(
                h,UnaryFunction(function)));
    }
    #elif defined(SWIGMZSCHEME)
    DerivedMarketElementHandle(const MarketElementRelinkableHandle& h,
                               Scheme_Object* function) {
        return new DerivedMarketElementHandle(
            new DerivedMarketElement<UnaryFunction>(
                h,UnaryFunction(function)));
    }
    #endif
}

%addmethods CompositeMarketElementHandle {
    #if defined(SWIGPYTHON)
    CompositeMarketElementHandle(const MarketElementRelinkableHandle& h1,
                                 const MarketElementRelinkableHandle& h2,
                                 PyObject* function) {
        return new CompositeMarketElementHandle(
            new CompositeMarketElement<BinaryFunction>(
                h1,h2,BinaryFunction(function)));
    }
    #elif defined(SWIGMZSCHEME)
    CompositeMarketElementHandle(const MarketElementRelinkableHandle& h1,
                                 const MarketElementRelinkableHandle& h2,
                                 Scheme_Object* function) {
        return new CompositeMarketElementHandle(
            new CompositeMarketElement<BinaryFunction>(
                h1,h2,BinaryFunction(function)));
    }
    #endif
}
#endif


#endif
