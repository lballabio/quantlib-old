
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

#ifndef quantlib_segment_integral_i
#define quantlib_segment_integral_i

%include common.i
%include types.i
%include functions.i

%{
using QuantLib::SegmentIntegral;
using QuantLib::TrapezoidIntegral;
using QuantLib::SimpsonIntegral;
using QuantLib::KronrodIntegral;
%}

%define INTEGRATION_METHODS
    %extend {
        #if defined(SWIGPYTHON)
        double __call__(PyObject* pyFunction, double a, double b) {
            UnaryFunction f(pyFunction);
            return (*self)(f, a, b);
        }
        #elif defined(SWIGRUBY)
        double __call__(double a, double b) {
            UnaryFunction f;
            return (*self)(f, a, b);
        }
        #elif defined(SWIGMZSCHEME)
        double calculate(Scheme_Object* mzFunction, double a, double b) {
            UnaryFunction f(mzFunction);
            return (*self)(f, a, b);
        }
        #elif defined(SWIGGUILE)
        double calculate(SCM ghFunction, double a, double b) {
            UnaryFunction f(ghFunction);
            return (*self)(f, a, b);
        }
        #endif
    }
%enddef

class SegmentIntegral {
  public:
    SegmentIntegral(Size intervals);
    INTEGRATION_METHODS;
};


class TrapezoidIntegral {
  public:
    TrapezoidIntegral(double accuracy);
    INTEGRATION_METHODS;
};


class SimpsonIntegral {
  public:
    SimpsonIntegral(double accuracy);
    INTEGRATION_METHODS;
};


class KronrodIntegral {
  public:
    KronrodIntegral(double accuracy);
    INTEGRATION_METHODS;
};



#endif
