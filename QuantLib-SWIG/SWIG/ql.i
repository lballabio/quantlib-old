
/*
 Copyright (C) 2000-2006 StatPro Italia srl

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

#ifdef SWIGPERL
%{
// Undefine perl symbols that are also used in quantlib
#undef Null
#undef Stat
%}
#endif

%{
#include <ql/quantlib.hpp>

#if QL_HEX_VERSION < 0x000313f0
    #error using an old version of QuantLib, please update
#endif

// add here SWIG version check

#if defined(_MSC_VER)         // Microsoft Visual C++ 6.0
// disable Swig-dependent warnings

// 'identifier1' has C-linkage specified,
// but returns UDT 'identifier2' which is incompatible with C
#pragma warning(disable: 4190)

// 'int' : forcing value to bool 'true' or 'false' (performance warning)
#pragma warning(disable: 4800)

// debug info too long etc etc
#pragma warning(disable: 4786)
#endif
%}

#ifdef SWIGPERL
%{
// Redefine the symbol Null which was undefined so that we can load in
// perl headers
#define Null(s) ((s) NULL)
%}
#endif

#ifdef SWIGPYTHON
%{
#if PY_VERSION_HEX < 0x02010000
    #error Python version 2.1.0 or later is required
#endif
%}
#endif

#ifdef SWIGJAVA
%include "enumtypesafe.swg"
#endif

// common name mappings
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(">string")       __str__;
#elif defined(SWIGJAVA)
%rename(add)           operator+;
%rename(add)           __add__;
%rename(subtract)      operator-;
%rename(subtract)      __sub__;
%rename(multiply)      operator*;
%rename(multiply)      __mul__;
%rename(divide)        operator/;
%rename(divide)        __div__;
%rename(getValue)      operator();
%rename(equals)        __eq__;
%rename(unEquals)      __ne__;
%rename(toString)      __str__;
#endif


%include common.i
%include blackmodel.i
%include bonds.i
%include calendars.i
%include callability.i
%include capfloor.i
%include cashflows.i
%include compoundforward.i
%include convertiblebonds.i
%include currencies.i
%include date.i
%include daycounters.i
%include discountcurve.i
%include distributions.i
%include dividends.i
%include exchangerates.i
%include exercise.i
%include forwardcurve.i
%include functions.i
%include grid.i
%include history.i
%include indexes.i
%include instruments.i
%include integrals.i
%include interestrate.i
%include interpolation.i
%include linearalgebra.i
%include marketelements.i
%include money.i
%include montecarlo.i
%include null.i
%include observer.i
%include operators.i
%include optimizers.i
%include options.i
%include payoffs.i
%include piecewiseflatforward.i
%include piecewiseyieldcurve.i
%include randomnumbers.i
%include ratehelpers.i
%include rounding.i
%include scheduler.i
%include settings.i
%include shortratemodels.i
%include statistics.i
%include stochasticprocess.i
%include swap.i
%include swaption.i
%include termstructures.i
%include types.i
%include vectors.i
%include volatilities.i
%include zerocurve.i

// to be deprecated
%include old_pricers.i
%include old_volatility.i
