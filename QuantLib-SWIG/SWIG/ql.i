
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

%{
#include <ql/quantlib.hpp>

#if QL_HEX_VERSION < 0x000301a0
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
#endif
%}

#ifdef SWIGPYTHON
%{
#if PY_VERSION_HEX < 0x02010000
    #error using an unsupported Python version, please update
#endif
%}
#endif

#ifdef SWIGGUILE
%scheme %{
; macros for making it easier to free memory
; careful: they prevent tail-recursion!

(define-macro deleting-let
  (lambda (bindings . body)
    (let ((thunk (gensym))
          (result (gensym)))
      `(let ,(map (lambda (b) (list (car b) (cadr b))) bindings)
         (define ,thunk (lambda () ,@body))
         (let ((,result (,thunk)))
           ,@(map (lambda (b) (list (caddr b) (car b))) bindings)
           ,result)))))

(define-macro deleting-let*
  (lambda (bindings . body)
    (let ((thunk (gensym))
          (result (gensym)))
      `(let* ,(map (lambda (b) (list (car b) (cadr b))) bindings)
         (define ,thunk (lambda () ,@body))
         (let ((,result (,thunk)))
           ,@(map (lambda (b) (list (caddr b) (car b))) bindings)
           ,result)))))

(define (do-not-delete x) #f)

(export deleting-let
        deleting-let*
        do-not-delete)
%}
#endif


%include common.i
%include blackmodel.i
%include calendars.i
%include capfloor.i
%include cashflows.i
%include currencies.i
%include date.i
%include daycounters.i
%include distributions.i
%include functions.i
%include history.i
%include indexes.i
%include instruments.i
%include interpolation.i
%include linearalgebra.i
%include marketelements.i
%include montecarlo.i
%include null.i
%include observer.i
%include operators.i
%include optimizers.i
%include options.i
%include piecewiseflatforward.i
%include randomnumbers.i
%include riskstatistics.i
%include scheduler.i
%include segmentintegral.i
%include statistics.i
%include swap.i
%include swaption.i
%include termstructures.i
%include types.i
%include vectors.i

// to be deprecated
%include old_pricers.i
%include old_volatility.i
