
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

class RiskMeasures {
  public:
    RiskMeasures();
    double potentialUpside(double percentile, double mean, double std) const;
    double valueAtRisk(double percentile, double mean, double std) const;
    double shortfall(double target, double mean, double std) const;
    double averageShortfall(double target, double mean, double std) const;
};

class RiskStatistics {
    #if defined(SWIGRUBY)
    %rename("reset!")                reset;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("standard-deviation")    standardDeviation;
    %rename("error-estimate")        errorEstimate;
    %rename("potential-upside")      potentialUpside;
    %rename("value-at-risk")         valueAtRisk;
    %rename("average-shortfall")     averageShortfall;
    %rename("expected-shortfall")    expectedShortfall;
    %rename("weight-sum")            weightSum;
    %rename("reset!")                reset;
    // resolve overloading
    %rename("add_single")            add(double,double);
    %rename("add_sequence")          add(const std::vector<double>&);
    %rename("add_weighted_sequence") add(const std::vector<double>&, 
                                         const std::vector<double>&);
    #if defined(SWIGGUILE)
    %scheme %{
        (define (RiskStatistics-add stats value . weight)
          (let ((method (cond ((number? value) RiskStatistics-add-single)
                              ((null? weight) RiskStatistics-add-sequence)
                              (else RiskStatistics-add-weighted-sequence))))
            (apply method stats value weight)))
        (export RiskStatistics-add)
    %}
    #endif
    #endif
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



#endif
