
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

#ifndef quantlib_risk_statistics_i
#define quantlib_risk_statistics_i

%include types.i
%include vectors.i

%{
using QuantLib::RiskStatistics;
using QuantLib::Math::RiskMeasures;
%}

#if defined(SWIGRUBY)
%rename("reset!")                RiskStatistics::reset;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("add-sequence")          RiskStatistics::addSequence;
%rename("add-weighted-sequence") RiskStatistics::addWeightedSequence;
%rename("standard-deviation")    RiskStatistics::standardDeviation;
%rename("error-estimate")        RiskStatistics::errorEstimate;
%rename("potential-upside")      RiskStatistics::potentialUpside;
%rename("value-at-risk")         RiskStatistics::valueAtRisk;
%rename("average-shortfall")     RiskStatistics::averageShortfall;
%rename("expected-shortfall")    RiskStatistics::expectedShortfall;
%rename("weight-sum")            RiskStatistics::weightSum;
%rename("reset!")                RiskStatistics::reset;
#endif


class RiskMeasures {
  public:
    RiskMeasures();
    double potentialUpside(double percentile, double mean, double std) const;
    double valueAtRisk(double percentile, double mean, double std) const;
    double shortfall(double target, double mean, double std) const;
    double averageShortfall(double target, double mean, double std) const;
};


class RiskStatistics {
  public:
    RiskStatistics();
    // Accessors
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
    double potentialUpside(double percentile) const;
    double valueAtRisk(double percentile) const;
    double expectedShortfall(double percentile) const;
    double shortfall(double target) const;
    double averageShortfall(double target) const;
    // Modifiers
    void add(double value, double weight = 1.0);
    void reset();
};

%extend RiskStatistics {
    void addSequence(const std::vector<double>& values) {
        self->addSequence(values.begin(), values.end());
    }
    void addWeightedSequence(const std::vector<double>& values, 
                             const std::vector<double>& weights) {
        self->addSequence(values.begin(), values.end(), weights.begin());
    }
}


#endif
