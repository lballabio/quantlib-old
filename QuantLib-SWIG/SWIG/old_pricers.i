
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
using QuantLib::Barrier;
typedef Barrier::Type BarrierType;

BarrierType barrierTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "downin")
        return Barrier::DownIn;
    else if (s == "downout")
        return Barrier::DownOut;
    else if (s == "upin")
        return Barrier::UpIn;
    else if (s == "upout")
        return Barrier::UpOut;
    else
        QL_FAIL("unknown barrier type: "+s);
}

std::string barrierTypeToString(BarrierType t) {
    switch (t) {
      case Barrier::DownIn:
        return "DownIn";
      case Barrier::DownOut:
        return "DownOut";
      case Barrier::UpIn:
        return "UpIn";
      case Barrier::UpOut:
        return "UpOut";
      default:
        QL_FAIL("unknown barrier type");
    }
}
%}

MapToString(BarrierType,barrierTypeFromString,barrierTypeToString);


%{
using QuantLib::CliquetOptionPricer;
%}

%rename(CliquetOption) CliquetOptionPricer;
class CliquetOptionPricer {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho") dividendRho;
    #endif
  public:
	CliquetOptionPricer(OptionType type, Real underlying, Real moneyness,
                        const std::vector<Spread>& dividendYield,
                        const std::vector<Rate>& riskFreeRate,
                        const std::vector<Time>& times,
                        const std::vector<Volatility>& volatility);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
};


%{
using QuantLib::ContinuousGeometricAPO;
using QuantLib::DiscreteGeometricAPO;
using QuantLib::DiscreteGeometricASO;
%}

class ContinuousGeometricAPO {
  public:
	ContinuousGeometricAPO(OptionType type, Real underlying, Real strike,
                           Spread dividendYield, Rate riskFreeRate, 
                           Time residualTime, Volatility volatility);
	Real value() const;
};

class DiscreteGeometricAPO {
  public:
	DiscreteGeometricAPO(OptionType type, Real underlying, Real strike,
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<Time>& timeDelays,
                         Volatility volatility);
	Real value() const;
};

class DiscreteGeometricASO {
  public:
	DiscreteGeometricASO(OptionType type, Real underlying, 
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<Time>& timeDelays,
                         Volatility volatility);
	Real value() const;
};



%{
using QuantLib::DividendEuropeanOption;
%}

class DividendEuropeanOption {
  public:
	DividendEuropeanOption(OptionType type, Real underlying, Real strike,
                           Spread dividendYield, Rate riskFreeRate, 
                           Time residualTime, Volatility volatility,
                           const std::vector<Spread>& dividends,
                           const std::vector<Time>& times);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
};


// Finite-difference pricers

%{
using QuantLib::FdEuropean;
%}

class FdEuropean {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdEuropean(OptionType type, Real underlying,
               Real strike, Spread dividendYield, Rate riskFreeRate,
               Time residualTime, Volatility volatility, 
               Size timeSteps = 200, Size gridPoints = 800);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};


%{
using QuantLib::FdAmericanOption;
using QuantLib::FdDividendAmericanOption;
%}

class FdAmericanOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdAmericanOption(OptionType type, Real underlying, Real strike,
                     Spread dividendYield, Rate riskFreeRate, 
                     Time residualTime, Volatility volatility, 
                     Size timeSteps = 100, Size gridPoints = 100);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};

class FdDividendAmericanOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdDividendAmericanOption(OptionType type, Real underlying, Real strike,
                             Spread dividendYield, Rate riskFreeRate, 
                             Time residualTime, Volatility volatility,
                             const std::vector<Spread>& dividends,
                             const std::vector<Time>& times,
                             Size timeSteps = 100, Size gridPoints = 100);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};


%{
using QuantLib::FdShoutOption;
using QuantLib::FdDividendShoutOption;
%}

class FdShoutOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdShoutOption(OptionType type, Real underlying, Real strike,
                  Spread dividendYield, Rate riskFreeRate, 
                  Time residualTime, Volatility volatility, 
                  Size timeSteps = 100, Size gridPoints = 100);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};

class FdDividendShoutOption{
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdDividendShoutOption(OptionType type, Real underlying, Real strike,
                          Spread dividendYield, Rate riskFreeRate, 
                          Time residualTime, Volatility volatility,
                          const std::vector<Spread>& dividends,
                          const std::vector<Time>& times,
                          Size timeSteps = 100, Size gridPoints = 100);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};


%{
using QuantLib::FdBermudanOption;
%}

class FdBermudanOption {
  public:
	FdBermudanOption(OptionType type, Real underlying, Real strike,
                     Spread dividendYield, Rate riskFreeRate, 
                     Time residualTime, Volatility volatility, 
                     const std::vector<Time>& exTimes,
                     Size timeSteps = 100, Size gridPoints = 100);
	Real value() const;
	Real delta() const;
	Real gamma() const;
	Real theta() const;
	Real vega() const;
	Real rho() const;
	Real dividendRho() const;
	Volatility impliedVolatility(Real targetValue, Real accuracy = 1e-4,
                                 Size maxEvaluations = 100) const;
};


// MonteCarlo pricers

%{
// single asset
using QuantLib::McDiscreteArithmeticAPO;
using QuantLib::McDiscreteArithmeticASO;

// multi asset
using QuantLib::McBasket;
using QuantLib::McMaxBasket;
using QuantLib::McEverest;
using QuantLib::McHimalaya;
using QuantLib::McPagoda;
%}

class McDiscreteArithmeticAPO {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McDiscreteArithmeticAPO(
                    OptionType type, Real underlying, Real strike,
                    const RelinkableHandle<TermStructure>& dividendYield,
                    const RelinkableHandle<TermStructure>& riskFreeRate,
                    const RelinkableHandle<BlackVolTermStructure>& volatility,
                    const std::vector<Time>& timeDelays,
                    bool controlVariate, BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McDiscreteArithmeticASO {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McDiscreteArithmeticASO(
                    OptionType type, Real underlying,
                    const RelinkableHandle<TermStructure>& dividendYield,
                    const RelinkableHandle<TermStructure>& riskFreeRate,
                    const RelinkableHandle<BlackVolTermStructure>& volatility,
                    const std::vector<Time>& timeDelays,
                    bool controlVariate, BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McBasket {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McBasket(OptionType type, 
             const std::vector<Real>& underlying, 
             Real strike, 
             const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
             const RelinkableHandle<TermStructure>& riskFreeRate,
             const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
             const Matrix& correlation,
             Time residualTime, 
             BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
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
                const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
                const RelinkableHandle<TermStructure>& riskFreeRate,
                const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
                const Matrix& correlation,
                Time residualTime, 
                BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
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
               const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
               const RelinkableHandle<TermStructure>& riskFreeRate,
               const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
               const Matrix& correlation,
			   Real strike, 
               const std::vector<Time>& timeDelays,
		       BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};

class McEverest {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McEverest(const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYield,
              const RelinkableHandle<TermStructure>& riskFreeRate,
              const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
              const Matrix& correlation,
              Time residualTime,
			  BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
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
             const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
             const RelinkableHandle<TermStructure>& riskFreeRate,
             const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
             const Matrix& correlation,
		     const std::vector<Time>& timeDelays,
		     BigInteger seed = 0);
    Real value(Real tolerance,
               Size maxSample = QL_MAX_INT) const;
    Real valueWithSamples(Size samples) const;
    Real errorEstimate() const;
};


#endif
