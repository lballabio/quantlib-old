
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

#ifndef quantlib_distributions_i
#define quantlib_distributions_i

%{
using QuantLib::NormalDistribution;
using QuantLib::CumulativeNormalDistribution;
using QuantLib::MoroInverseCumulativeNormal;
using QuantLib::InverseCumulativeNormal;
%}

class NormalDistribution {
    #if defined(SWIGRUBY)
    %rename(__call__) operator();
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    NormalDistribution(double average = 0.0, double sigma = 1.0);
    double operator()(double x);
    double derivative(double x);
};

class CumulativeNormalDistribution {
    #if defined(SWIGRUBY)
    %rename(__call__) operator();
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    CumulativeNormalDistribution(double average = 0.0, double sigma = 1.0);
    double operator()(double x);
    double derivative(double x);
};

class InverseCumulativeNormal {
    #if defined(SWIGRUBY)
    %rename(__call__) operator();
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    InverseCumulativeNormal(double average = 0.0, double sigma = 1.0);
    double operator()(double x);
};

class MoroInverseCumulativeNormal {
    #if defined(SWIGRUBY)
    %rename(__call__) operator();
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    MoroInverseCumulativeNormal(double average = 0.0, double sigma = 1.0);
    double operator()(double x);
};


#endif
