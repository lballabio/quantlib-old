
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

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
using QuantLib::Default;
using QuantLib::MidPoint;
using QuantLib::SimpsonIntegral;
using QuantLib::GaussKronrodAdaptive;
using QuantLib::GaussKronrodNonAdaptive;
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


template <class IntegrationPolicy>
class TrapezoidIntegral {
  public:
    TrapezoidIntegral(Real accuracy, Size maxIterations);
    INTEGRATION_METHODS;
};

%template(TrapezoidIntegralDefault) TrapezoidIntegral<Default>;
%template(TrapezoidIntegralMidPoint) TrapezoidIntegral<MidPoint>;

class SimpsonIntegral {
  public:
    SimpsonIntegral(Real accuracy, Size maxIterations);
    INTEGRATION_METHODS;
};


class GaussKronrodAdaptive {
  public:
    GaussKronrodAdaptive(Real tolerance,
                         Size maxFunctionEvaluations = Null<Size>());
    INTEGRATION_METHODS;
};

class GaussKronrodNonAdaptive {
  public:
    GaussKronrodNonAdaptive(Real absoluteAccuracy,
                            Size maxEvaluations,
                            Real relativeAccuracy);
    INTEGRATION_METHODS;
};



#endif
