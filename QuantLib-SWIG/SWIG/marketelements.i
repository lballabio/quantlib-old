
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

%include common.i
%include observer.i
%include functions.i

%{
using QuantLib::MarketElement;
%}

%ignore MarketElement;
class MarketElement {
  public:
    double value() const;
};

%template(MarketElement) Handle<MarketElement>;
IsObservable(Handle<MarketElement>);

%template(MarketElementHandle) RelinkableHandle<MarketElement>;
IsObservable(RelinkableHandle<MarketElement>);



// actual market elements
%{
using QuantLib::SimpleMarketElement;
typedef Handle<MarketElement> SimpleMarketElementHandle;
%}

// Fake inheritance between Handles
%rename(SimpleMarketElement) SimpleMarketElementHandle;
class SimpleMarketElementHandle : public Handle<MarketElement> {
    #if defined(SWIGRUBY)
    %rename("value=")     setValue;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-set!") setValue;
    #endif
  public:
    %extend {
        SimpleMarketElementHandle(double value) {
            return new SimpleMarketElementHandle(
                new SimpleMarketElement(value));
        }
        void setValue(double value) {
            Handle<SimpleMarketElement>(*self)->setValue(value);
        }
    }
};


#if defined(SWIGPYTHON) || defined(SWIGMZSCHEME)
%{
using QuantLib::DerivedMarketElement;
using QuantLib::CompositeMarketElement;
typedef Handle<MarketElement> DerivedMarketElementHandle;
typedef Handle<MarketElement> CompositeMarketElementHandle;
%}

%rename(DerivedMarketElement) DerivedMarketElementHandle;
class DerivedMarketElementHandle : public Handle<MarketElement> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        DerivedMarketElementHandle(const RelinkableHandle<MarketElement>& h,
                                   PyObject* function) {
            return new DerivedMarketElementHandle(
                new DerivedMarketElement<UnaryFunction>(
                    h,UnaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        DerivedMarketElementHandle(const RelinkableHandle<MarketElement>& h,
                                   Scheme_Object* function) {
            return new DerivedMarketElementHandle(
                new DerivedMarketElement<UnaryFunction>(
                    h,UnaryFunction(function)));
        }
        #endif
    }
};

%rename(CompositeMarketElement) CompositeMarketElementHandle;
class CompositeMarketElementHandle : public Handle<MarketElement> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        CompositeMarketElementHandle(const RelinkableHandle<MarketElement>& h1,
                                     const RelinkableHandle<MarketElement>& h2,
                                     PyObject* function) {
            return new CompositeMarketElementHandle(
                new CompositeMarketElement<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        CompositeMarketElementHandle(const RelinkableHandle<MarketElement>& h1,
                                     const RelinkableHandle<MarketElement>& h2,
                                     Scheme_Object* function) {
            return new CompositeMarketElementHandle(
                new CompositeMarketElement<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #endif
    }
};
#endif


#endif
