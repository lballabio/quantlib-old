
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
        Real __call__(PyObject* pyFunction, Real a, Real b) {
            UnaryFunction f(pyFunction);
            return (*self)(f, a, b);
        }
        #elif defined(SWIGRUBY)
        Real __call__(Real a, Real b) {
            UnaryFunction f;
            return (*self)(f, a, b);
        }
        #elif defined(SWIGMZSCHEME)
        Real calculate(Scheme_Object* mzFunction, Real a, Real b) {
            UnaryFunction f(mzFunction);
            return (*self)(f, a, b);
        }
        #elif defined(SWIGGUILE)
        Real calculate(SCM ghFunction, Real a, Real b) {
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
    TrapezoidIntegral(Real accuracy);
    INTEGRATION_METHODS;
};


class SimpsonIntegral {
  public:
    SimpsonIntegral(Real accuracy);
    INTEGRATION_METHODS;
};


class KronrodIntegral {
  public:
    KronrodIntegral(Real accuracy);
    INTEGRATION_METHODS;
};



#endif
