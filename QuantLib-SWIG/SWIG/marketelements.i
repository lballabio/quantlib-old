
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
%}

%ignore Quote;
class Quote {
  public:
    Real value() const;
};

%template(Quote) boost::shared_ptr<Quote>;
IsObservable(boost::shared_ptr<Quote>);

%template(QuoteHandle) RelinkableHandle<Quote>;
IsObservable(RelinkableHandle<Quote>);

// actual market elements
%{
using QuantLib::SimpleQuote;
typedef boost::shared_ptr<Quote> SimpleQuotePtr;
%}

%rename(SimpleQuote) SimpleQuotePtr;
class SimpleQuotePtr : public boost::shared_ptr<Quote> {
    #if defined(SWIGRUBY)
    %rename("value=")     setValue;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-set!") setValue;
    #endif
  public:
    %extend {
        SimpleQuotePtr(Real value) {
            return new SimpleQuotePtr(new SimpleQuote(value));
        }
        void setValue(Real value) {
            boost::dynamic_pointer_cast<SimpleQuote>(*self)->setValue(value);
        }
    }
};


#if defined(SWIGPYTHON) || defined(SWIGMZSCHEME)
%{
using QuantLib::DerivedQuote;
using QuantLib::CompositeQuote;
typedef boost::shared_ptr<Quote> DerivedQuotePtr;
typedef boost::shared_ptr<Quote> CompositeQuotePtr;
%}

%rename(DerivedQuote) DerivedQuotePtr;
class DerivedQuotePtr : public boost::shared_ptr<Quote> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        DerivedQuotePtr(const RelinkableHandle<Quote>& h,
                        PyObject* function) {
            return new DerivedQuotePtr(
                new DerivedQuote<UnaryFunction>(h,UnaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        DerivedQuotePtr(const RelinkableHandle<Quote>& h,
                        Scheme_Object* function) {
            return new DerivedQuotePtr(
                new DerivedQuote<UnaryFunction>(h,UnaryFunction(function)));
        }
        #endif
    }
};


%rename(CompositeQuote) CompositeQuotePtr;
class CompositeQuotePtr : public boost::shared_ptr<Quote> {
  public:
    %extend {
        #if defined(SWIGPYTHON)
        CompositeQuotePtr(const RelinkableHandle<Quote>& h1,
                          const RelinkableHandle<Quote>& h2,
                          PyObject* function) {
            return new CompositeQuotePtr(
                new CompositeQuote<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #elif defined(SWIGMZSCHEME)
        CompositeQuotePtr(const RelinkableHandle<Quote>& h1,
                          const RelinkableHandle<Quote>& h2,
                          Scheme_Object* function) {
            return new CompositeQuotePtr(
                new CompositeQuote<BinaryFunction>(
                    h1,h2,BinaryFunction(function)));
        }
        #endif
    }
};

#endif

namespace std {
    %template(QuoteVector) vector<boost::shared_ptr<Quote> >;
    %template(QuoteHandleVector) vector<RelinkableHandle<Quote> >;
}


#endif
