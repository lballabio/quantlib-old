
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2007 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2011 Tawanda Gwena

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

#ifndef quantlib_distributions_i
#define quantlib_distributions_i

%{
using QuantLib::NormalDistribution;
using QuantLib::CumulativeNormalDistribution;
using QuantLib::MoroInverseCumulativeNormal;
using QuantLib::InverseCumulativeNormal;
using QuantLib::BivariateCumulativeNormalDistribution;
using QuantLib::BinomialDistribution;
using QuantLib::CumulativeBinomialDistribution;
using QuantLib::BivariateCumulativeNormalDistributionDr78;
using QuantLib::BivariateCumulativeNormalDistributionWe04DP;
using QuantLib::ChiSquareDistribution;
using QuantLib::NonCentralChiSquareDistribution;
using QuantLib::InverseNonCentralChiSquareDistribution;
using QuantLib::GammaDistribution;
using QuantLib::GammaFunction;
using QuantLib::PoissonDistribution;
using QuantLib::CumulativePoissonDistribution;
using QuantLib::InverseCumulativePoisson;
using QuantLib::StudentDistribution;
using QuantLib::CumulativeStudentDistribution;
using QuantLib::InverseCumulativeStudent;
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


class BivariateCumulativeNormalDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    BivariateCumulativeNormalDistribution(Real rho);
    Real operator()(Real x, Real y);
};

class BinomialDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    BinomialDistribution(Real p, BigNatural n);
    Real operator()(BigNatural k);
};

class CumulativeBinomialDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    CumulativeBinomialDistribution(Real p, BigNatural n);
    Real operator()(BigNatural k);
};

class BivariateCumulativeNormalDistributionDr78 {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    BivariateCumulativeNormalDistributionDr78(Real rho);
    Real operator()(Real a, Real b);
};

class BivariateCumulativeNormalDistributionWe04DP {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    BivariateCumulativeNormalDistributionWe04DP(Real rho);
    Real operator()(Real a, Real b);
};

class ChiSquareDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    ChiSquareDistribution(Real df);
    Real operator()(Real x);
};

class NonCentralChiSquareDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    NonCentralChiSquareDistribution(Real df, Real ncp);
    Real operator()(Real x);
};

class InverseNonCentralChiSquareDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    InverseNonCentralChiSquareDistribution(Real df, Real ncp,
                                           Size maxEvaluations = 10,
                                           Real accuracy = 1e-8);
    Real operator()(Real x);
};

class GammaDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    GammaDistribution(Real a);
    Real operator()(Real x);
};

class GammaFunction {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    Real logValue(Real x);
};

class PoissonDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    PoissonDistribution(Real mu);
    Real operator()(BigNatural k);
};

class CumulativePoissonDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    CumulativePoissonDistribution(Real mu);
    Real operator()(BigNatural k);
};

class InverseCumulativePoisson {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    InverseCumulativePoisson(Real lambda);
    Real operator()(Real x);
};

class StudentDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    StudentDistribution(Integer n);
    Real operator()(Real x);
};

class CumulativeStudentDistribution {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    CumulativeStudentDistribution(Integer n);
    Real operator()(Real x);
};

class InverseCumulativeStudent {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #endif
  public:
    InverseCumulativeStudent(Integer n, Real accuracy = 1e-6,
                             Size maxIterations = 50);
    Real operator()(Real x);
};

#endif
