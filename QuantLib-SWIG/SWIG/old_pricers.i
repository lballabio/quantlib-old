
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
        throw Error("unknown barrier type: "+s);
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
        throw Error("unknown barrier type");
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
	CliquetOptionPricer(OptionType type, double underlying, double moneyness,
                        const std::vector<double>& dividendYield,
                        const std::vector<double>& riskFreeRate,
                        const std::vector<double>& times,
                        const std::vector<double>& volatility);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
};


%{
using QuantLib::ContinuousGeometricAPO;
using QuantLib::DiscreteGeometricAPO;
using QuantLib::DiscreteGeometricASO;
%}

class ContinuousGeometricAPO {
  public:
	ContinuousGeometricAPO(OptionType type, double underlying, double strike,
                           Spread dividendYield, Rate riskFreeRate, 
                           double residualTime, double volatility);
	double value() const;
};

class DiscreteGeometricAPO {
  public:
	DiscreteGeometricAPO(OptionType type, double underlying, double strike,
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<double>& timeDelays,
                         double volatility);
	double value() const;
};

class DiscreteGeometricASO {
  public:
	DiscreteGeometricASO(OptionType type, double underlying, 
                         Spread dividendYield, Rate riskFreeRate,
                         const std::vector<double>& timeDelays,
                         double volatility);
	double value() const;
};



%{
using QuantLib::DividendEuropeanOption;
%}

class DividendEuropeanOption {
  public:
	DividendEuropeanOption(OptionType type, double underlying, double strike,
                           Spread dividendYield, Rate riskFreeRate, 
                           Time residualTime, double volatility,
                           const std::vector<double>& dividends,
                           const std::vector<double>& times);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
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
	FdEuropean(OptionType type, double underlying,
               double strike, Spread dividendYield, Rate riskFreeRate,
               Time residualTime, double volatility, 
               Size timeSteps = 200, Size gridPoints = 800);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
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
	FdAmericanOption(OptionType type, double underlying, double strike,
                     Spread dividendYield, Rate riskFreeRate, 
                     Time residualTime, double volatility, 
                     Size timeSteps = 100, Size gridPoints = 100);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const;
};

class FdDividendAmericanOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdDividendAmericanOption(OptionType type, double underlying, double strike,
                             Spread dividendYield, Rate riskFreeRate, 
                             Time residualTime, double volatility,
                             const std::vector<double>& dividends,
                             const std::vector<double>& times,
                             Size timeSteps = 100, Size gridPoints = 100);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
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
	FdShoutOption(OptionType type, double underlying, double strike,
                  Spread dividendYield, Rate riskFreeRate, 
                  Time residualTime, double volatility, 
                  Size timeSteps = 100, Size gridPoints = 100);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const;
};

class FdDividendShoutOption{
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdDividendShoutOption(OptionType type, double underlying, double strike,
                          Spread dividendYield, Rate riskFreeRate, 
                          Time residualTime, double volatility,
                          const std::vector<double>& dividends,
                          const std::vector<double>& times,
                          Size timeSteps = 100, Size gridPoints = 100);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const;
};


%{
using QuantLib::FdBermudanOption;
%}

class FdBermudanOption {
  public:
	FdBermudanOption(OptionType type, double underlying, double strike,
                     Spread dividendYield, Rate riskFreeRate, 
                     Time residualTime, double volatility, 
                     const std::vector<double>& exTimes,
                     Size timeSteps = 100, Size gridPoints = 100);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
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
                    OptionType type, double underlying, double strike,
                    const RelinkableHandle<TermStructure>& dividendYield,
                    const RelinkableHandle<TermStructure>& riskFreeRate,
                    const RelinkableHandle<BlackVolTermStructure>& volatility,
                    const std::vector<double>& timeDelays,
                    bool controlVariate, long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McDiscreteArithmeticASO {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McDiscreteArithmeticASO(
                    OptionType type, double underlying,
                    const RelinkableHandle<TermStructure>& dividendYield,
                    const RelinkableHandle<TermStructure>& riskFreeRate,
                    const RelinkableHandle<BlackVolTermStructure>& volatility,
                    const std::vector<double>& timeDelays,
                    bool controlVariate, long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McBasket {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McBasket(OptionType type, 
             const std::vector<double>& underlying, 
             double strike, 
             const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
             const RelinkableHandle<TermStructure>& riskFreeRate,
             const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
             const Matrix& correlation,
             double residualTime, 
             long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McMaxBasket {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McMaxBasket(const std::vector<double>& underlying, 
                const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
                const RelinkableHandle<TermStructure>& riskFreeRate,
                const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
                const Matrix& correlation,
                double residualTime, 
                long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McHimalaya {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McHimalaya(const std::vector<double>& underlying, 
               const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
               const RelinkableHandle<TermStructure>& riskFreeRate,
               const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
               const Matrix& correlation,
			   double strike, 
               const std::vector<Time>& timeDelays,
		       long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
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
			  long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McPagoda {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
    McPagoda(const std::vector<double>& portfolio, 
             double fraction, 
             double roof,
             const std::vector<RelinkableHandle<TermStructure> >& 
                                                             dividendYields,
             const RelinkableHandle<TermStructure>& riskFreeRate,
             const std::vector<RelinkableHandle<BlackVolTermStructure> >& 
                                                             volatilities,
             const Matrix& correlation,
		     const std::vector<Time>& timeDelays,
		     long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};


#endif
