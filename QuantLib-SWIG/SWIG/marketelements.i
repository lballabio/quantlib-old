
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

#ifndef quantlib_market_elements_i
#define quantlib_market_elements_i

%include common.i
%include observer.i
%include functions.i

%{
using QuantLib::Quote;
using QuantLib::MarketElement;
%}

%ignore Quote;
class Quote {
  public:
    double value() const;
};

typedef Quote MarketElement;

%template(Quote) Handle<Quote>;
IsObservable(Handle<Quote>);

%template(QuoteHandle) RelinkableHandle<Quote>;
IsObservable(RelinkableHandle<Quote>);

#if defined(SWIGPYTHON)
%pythoncode %{
    MarketElement = Quote
    MarketElementHandle = QuoteHandle
%}
#endif


// actual market elements
%{
using QuantLib::SimpleQuote;
typedef Handle<Quote> SimpleQuoteHandle;
%}

// Fake inheritance between Handles
%rename(SimpleQuote) SimpleQuoteHandle;
class SimpleQuoteHandle : public Handle<Quote> {
    #if defined(SWIGRUBY)
    %rename("value=")     setValue;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-set!") setValue;
    #endif
  public:
    %extend {
        SimpleQuoteHandle(double value) {
            return new SimpleQuoteHandle(new SimpleQuote(value));
        }
        void setValue(double value) {
            %#if defined(HAVE_BOOST)
            boost::dynamic_pointer_cast<SimpleQuote>(*self)->setValue(value);
            %#else
            Handle<SimpleQuote>(*self)->setValue(value);
            %#endif
        }
    }
};

#if defined(SWIGPYTHON)
%pythoncode %{
    SimpleMarketElement = SimpleQuote
%}
#endif


#if defined(SWIGPYTHON) || defined(SWIGMZSCHEME)
%{
using QuantLib::DerivedQuote;
using QuantLib::CompositeQuote;
typedef Handle<Quote> DerivedQuoteHandle;
typedef Handle<Quote> CompositeQuoteHandle;
%}

%rename(DerivedQuote) DerivedQuoteHandle;
class DerivedQuoteHandle : public Handle<Quote> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        DerivedQuoteHandle(const RelinkableHandle<Quote>& h,
                           PyObject* function) {
            return new DerivedQuoteHandle(
                new DerivedQuote<UnaryFunction>(h,UnaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        DerivedQuoteHandle(const RelinkableHandle<Quote>& h,
                           Scheme_Object* function) {
            return new DerivedQuoteHandle(
                new DerivedQuote<UnaryFunction>(h,UnaryFunction(function)));
        }
        #endif
    }
};

#if defined(SWIGPYTHON)
%pythoncode %{
    DerivedMarketElement = DerivedQuote
%}
#endif


%rename(CompositeQuote) CompositeQuoteHandle;
class CompositeQuoteHandle : public Handle<Quote> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        CompositeQuoteHandle(const RelinkableHandle<Quote>& h1,
                             const RelinkableHandle<Quote>& h2,
                             PyObject* function) {
            return new CompositeQuoteHandle(
                new CompositeQuote<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        CompositeQuoteHandle(const RelinkableHandle<Quote>& h1,
                             const RelinkableHandle<Quote>& h2,
                             Scheme_Object* function) {
            return new CompositeQuoteHandle(
                new CompositeQuote<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #endif
    }
};

#if defined(SWIGPYTHON)
%pythoncode %{
    CompositeMarketElement = CompositeQuote
%}
#endif

#endif

namespace std {
    %template(QuoteVector) vector<Handle<Quote> >;
    %template(QuoteHandleVector) vector<RelinkableHandle<Quote> >;
}

#endif
