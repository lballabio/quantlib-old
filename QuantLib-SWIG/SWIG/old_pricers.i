
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
	DiscreteGeometricASO(OptionType type, Real underlying,
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<Time>& timeDelays,
                         Volatility volatility);
	Real value() const;
};


// MonteCarlo pricers

%{
// single asset
using QuantLib::McDiscreteArithmeticASO;

// multi asset
using QuantLib::McMaxBasket;
using QuantLib::McEverest;
using QuantLib::McHimalaya;
using QuantLib::McPagoda;
%}

class McDiscreteArithmeticASO {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McDiscreteArithmeticASO(OptionType type, Real underlying,
                            const Handle<YieldTermStructure>& dividendYield,
                            const Handle<YieldTermStructure>& riskFreeRate,
                            const Handle<BlackVolTermStructure>& volatility,
                            const std::vector<Time>& timeDelays,
                            bool controlVariate, BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INTEGER) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McMaxBasket {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McMaxBasket(const std::vector<Real>& underlying,
                const std::vector<Handle<YieldTermStructure> >& dividendYields,
                const Handle<YieldTermStructure>& riskFreeRate,
                const std::vector<Handle<BlackVolTermStructure> >&
                                                             volatilities,
                const Matrix& correlation,
                Time residualTime,
                BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INTEGER) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McHimalaya {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McHimalaya(const std::vector<Real>& underlying,
               const std::vector<Handle<YieldTermStructure> >& dividendYields,
               const Handle<YieldTermStructure>& riskFreeRate,
               const std::vector<Handle<BlackVolTermStructure> >& volatilities,
               const Matrix& correlation,
			   Real strike,
               const std::vector<Time>& timeDelays,
		       BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INTEGER) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McEverest {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McEverest(const std::vector<Handle<YieldTermStructure> >& dividendYield,
              const Handle<YieldTermStructure>& riskFreeRate,
              const std::vector<Handle<BlackVolTermStructure> >& volatilities,
              const Matrix& correlation,
              Time residualTime,
			  BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INTEGER) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McPagoda {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McPagoda(const std::vector<Real>& portfolio,
             Real fraction,
             Real roof,
             const std::vector<Handle<YieldTermStructure> >& dividendYields,
             const Handle<YieldTermStructure>& riskFreeRate,
             const std::vector<Handle<BlackVolTermStructure> >& volatilities,
             const Matrix& correlation,
		     const std::vector<Time>& timeDelays,
		     BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INTEGER) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};


#endif
