
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

#ifndef quantlib_statistics_i
#define quantlib_statistics_i

%include types.i
%include linearalgebra.i
%include vectors.i
%include stl.i

%{
using QuantLib::Math::Statistics;
using QuantLib::Math::RiskStatistics;
using QuantLib::Math::SequenceStatistics;
%}

class Statistics {
    #if defined(SWIGRUBY)
    %rename("reset!")                reset;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("weight-sum")            weightSum;
    %rename("standard-deviation")    standardDeviation;
    %rename("error-estimate")        errorEstimate;
    %rename("reset!")                reset;
    #endif
  public:
    Size samples() const;
    double weightSum() const;
    double mean() const;
    double variance() const;
    double standardDeviation() const;
    double errorEstimate() const;
    double skewness() const;
    double kurtosis() const;
    double min() const;
    double max() const;
    // Modifiers
    void reset();
    void add(double value, double weight = 1.0);
    %extend {
        void add(const std::vector<double>& values) {
            self->addSequence(values.begin(), values.end());
        }
        void add(const std::vector<double>& values, 
                 const std::vector<double>& weights) {
            self->addSequence(values.begin(), values.end(), weights.begin());
        }
    }
};

class RiskStatistics : public Statistics {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("semi-variance")         semiVariance;
    %rename("semi-deviation")        semiDeviation;
    %rename("downside-variance")     downsideVariance;
    %rename("downside-deviation")    downsideDeviation;
    %rename("potential-upside")      potentialUpside;
    %rename("value-at-risk")         valueAtRisk;
    %rename("expected-shortfall")    expectedShortfall;
    %rename("average-shortfall")     averageShortfall;
    #endif
  public:
    double semiVariance() const;
    double semiDeviation() const;
    double downsideVariance() const;
    double downsideDeviation() const;
    double regret(double target) const;
    double potentialUpside(double percentile) const;
    double valueAtRisk(double percentile) const;
    double expectedShortfall(double percentile) const;
    double shortfall(double target) const;
    double averageShortfall(double target) const;
};

template <class S>
class SequenceStatistics {
    #if defined(SWIGRUBY)
    %rename("reset!")                reset;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("weight-sum")            weightSum;
    %rename("standard-deviation")    standardDeviation;
    %rename("error-estimate")        errorEstimate;
    %rename("reset!")                reset;
    #endif
  public:
    SequenceStatistics(Size dimension);
    Size samples() const;
    double weightSum() const;
    std::vector<double> mean() const;
    std::vector<double> variance() const;
    std::vector<double> standardDeviation() const;
    std::vector<double> errorEstimate() const;
    std::vector<double> skewness() const;
    std::vector<double> kurtosis() const;
    std::vector<double> min() const;
    std::vector<double> max() const;
    // Modifiers
    void reset();
    void add(const std::vector<double>& value, double weight = 1.0);
    void add(const Array& value, double weight = 1.0);
};

%template(MultipleStatistics) SequenceStatistics<Statistics>;

%{
using QuantLib::Math::MultivariateAccumulator;
%}

class MultivariateAccumulator {
  public:
    Size size() const;
    Size samples() const;
    double weightSum() const;
    Array mean() const;
    Matrix covariance() const;
	Matrix correlation() const;
    void add(const Array& a, double weight = 1.0);
    void reset();
};


#endif
