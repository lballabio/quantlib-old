
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_old_pricers_i
#define quantlib_old_pricers_i

%include date.i
%include options.i
%include types.i
%include linearalgebra.i
%include vectors.i


// Analytic pricers

%{
using QuantLib::Pricers::EuropeanOption;
%}

class EuropeanOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	EuropeanOption(OptionType type, double underlying, double strike,
                   Spread dividendYield, Rate riskFreeRate, Time residualTime,
                   double volatility);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const ;
};


%{
using QuantLib::Pricers::BarrierOption;
typedef BarrierOption::BarrierType BarrierType;

BarrierType barrierTypeFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "downin")
        return BarrierOption::DownIn;
    else if (s == "downout")
        return BarrierOption::DownOut;
    else if (s == "upin")
        return BarrierOption::UpIn;
    else if (s == "upout")
        return BarrierOption::UpOut;
    else
        throw Error("unknown barrier type: "+s);
}

std::string barrierTypeToString(BarrierType t) {
    switch (t) {
      case BarrierOption::DownIn:
        return "DownIn";
      case BarrierOption::DownOut:
        return "DownOut";
      case BarrierOption::UpIn:
        return "UpIn";
      case BarrierOption::UpOut:
        return "UpOut";
      default:
        throw Error("unknown barrier type");
    }
}
%}

MapToString(BarrierType,barrierTypeFromString,barrierTypeToString);

class BarrierOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    #endif
  public:
    BarrierOption(BarrierType barrType, OptionType type, double underlying,
                  double strike, Spread dividendYield, Rate riskFreeRate,
                  Time residualTime, double volatility, double barrier,
                  double rebate = 0.0);
    double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
};


%{
using QuantLib::Pricers::BinaryOption;
%}

class BinaryOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	BinaryOption(OptionType type, double underlying, double strike,
                 Spread dividendYield, Rate riskFreeRate, Time residualTime,
                 double volatility, double cashPayoff = 1.0);
	double value() const;
	double delta() const;
	double gamma() const;
	double theta() const;
	double vega() const;
	double rho() const;
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const ;
};


%{
using QuantLib::Pricers::CliquetOption;
%}

class CliquetOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho") dividendRho;
    #endif
  public:
	CliquetOption(OptionType type, double underlying, double moneyness,
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
using QuantLib::Pricers::ContinuousGeometricAPO;
using QuantLib::Pricers::DiscreteGeometricAPO;
using QuantLib::Pricers::DiscreteGeometricASO;
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



// Finite-difference pricers

%{
using QuantLib::Pricers::FdEuropean;
using QuantLib::Pricers::FdDividendEuropeanOption;
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

class FdDividendEuropeanOption {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("dividend-rho")       dividendRho;
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
	FdDividendEuropeanOption(OptionType type, double underlying, double strike,
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
	double dividendRho() const;
	double impliedVolatility(double targetValue, double accuracy = 1e-4,
                             Size maxEvaluations = 100) const;
};


%{
using QuantLib::Pricers::FdAmericanOption;
using QuantLib::Pricers::FdDividendAmericanOption;
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
using QuantLib::Pricers::FdShoutOption;
using QuantLib::Pricers::FdDividendShoutOption;
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
using QuantLib::Pricers::FdBermudanOption;
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
using QuantLib::Pricers::McDiscreteArithmeticAPO;
using QuantLib::Pricers::McDiscreteArithmeticASO;
using QuantLib::Pricers::McEuropean;

// multi asset
using QuantLib::Pricers::McBasket;
using QuantLib::Pricers::McMaxBasket;
using QuantLib::Pricers::McEverest;
using QuantLib::Pricers::McHimalaya;
using QuantLib::Pricers::McPagoda;
%}

class McEuropean {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McEuropean(OptionType type, double underlying, double strike,
			   Spread dividendYield, Rate riskFreeRate,
			   double residualTime, double volatility,
			   bool antitheticVariance, long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

class McDiscreteArithmeticAPO {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
  public:
	McDiscreteArithmeticAPO(OptionType type, double underlying, double strike,
                            Spread dividendYield, Rate riskFreeRate,
                            const std::vector<double>& timeDelays,
                            double volatility, bool antitheticVariance,
                            bool controlVariate, long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};

    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("value-with-samples") valueWithSamples;
    %rename("error-estimate")     errorEstimate;
    #endif
class McDiscreteArithmeticASO {
  public:
	McDiscreteArithmeticASO(OptionType type, double underlying,
                            Spread dividendYield, Rate riskFreeRate,
                            const std::vector<double>& timeDelays,
                            double volatility, bool antitheticVariance,
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
    McBasket(OptionType type, const Array& underlying, double strike,
   		     const Array& dividendYield, const Matrix& covariance,
		     Rate riskFreeRate, double residualTime,
		     bool antitheticVariance, long seed = 0);
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
    McMaxBasket(const Array& underlying, const Array& dividendYield,
                const Matrix& covariance, Rate riskFreeRate,
                double residualTime, bool antitheticVariance,
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
    McHimalaya(const Array& underlying, const Array& dividendYield,
               const Matrix& covariance, Rate riskFreeRate,
			   double strike, const std::vector<double>& timeDelays,
		       bool antitheticVariance, long seed = 0);
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
    McEverest(const Array& dividendYield, const Matrix& covariance,
              Rate riskFreeRate, Time residualTime,
			  bool antitheticVariance, long seed = 0);
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
    McPagoda(const Array& portfolio, double fraction, double roof,
		     const Array& dividendYield, const Matrix& covariance,
		     Rate riskFreeRate, const std::vector<double>& timeDelays,
		     bool antithetic, long seed = 0);
    double value(double tolerance,
                 Size maxSample = QL_MAX_INT) const;
    double valueWithSamples(Size samples) const;
    double errorEstimate() const;
};


#endif
