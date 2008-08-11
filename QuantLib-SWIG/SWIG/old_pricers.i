
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

#ifndef quantlib_old_pricers_i
#define quantlib_old_pricers_i

%include date.i
%include options.i
%include types.i
%include linearalgebra.i
%include termstructures.i
%include volatilities.i
%include vectors.i


// Analytic pricers

%{
using QuantLib::DiscreteGeometricASO;
%}

class DiscreteGeometricASO {
  public:
    DiscreteGeometricASO(Option::Type type, Real underlying,
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<Time>& timeDelays,
                         Volatility volatility);
    Real value() const;
};


// MonteCarlo pricers

// Define so that the vectors get created
%include stl.i

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_SPECIALIZE( YieldTermStructureHandle,
                            Handle<YieldTermStructure> )
#endif
%template(YieldTermStructureVector)
    std::vector<Handle<YieldTermStructure> >;

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_SPECIALIZE( BlackVolTermStructureHandle,
                            Handle<BlackVolTermStructure> )
#endif
%template(BlackVolTermStructureVector)
    std::vector<Handle<BlackVolTermStructure> >;


#endif
