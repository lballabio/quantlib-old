
/*
 Copyright (C) 2000, 2001, 2002, 2003 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier

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

#ifndef quantlib_distributions_i
#define quantlib_distributions_i

%{
using QuantLib::NormalDistribution;
using QuantLib::CumulativeNormalDistribution;
using QuantLib::MoroInverseCumulativeNormal;
using QuantLib::InverseCumulativeNormal;
%}

class NormalDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    NormalDistribution(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x);
    Real derivative(Real x);
};

class CumulativeNormalDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    CumulativeNormalDistribution(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x);
    Real derivative(Real x);
};

class InverseCumulativeNormal {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    InverseCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x);
};

class MoroInverseCumulativeNormal {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    MoroInverseCumulativeNormal(Real average = 0.0, Real sigma = 1.0);
    Real operator()(Real x);
};


#endif
