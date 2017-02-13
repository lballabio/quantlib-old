
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

#ifndef quantlib_statistics_i
#define quantlib_statistics_i

%include types.i
%include linearalgebra.i
%include vectors.i
%include stl.i

%{
using QuantLib::Statistics;
using QuantLib::IncrementalStatistics;
using QuantLib::RiskStatistics;
using QuantLib::GenericSequenceStatistics;
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
    Real weightSum() const;
    Real mean() const;
    Real variance() const;
    Real standardDeviation() const;
    Real errorEstimate() const;
    Real skewness() const;
    Real kurtosis() const;
    Real min() const;
    Real max() const;
    // Modifiers
    void reset();
    void add(Real value, Real weight = 1.0);
    %extend {
        void add(const std::vector<Real>& values) {
            self->addSequence(values.begin(), values.end());
        }
        void add(const std::vector<Real>& values, 
                 const std::vector<Real>& weights) {
            self->addSequence(values.begin(), values.end(), weights.begin());
        }
    }
};


class IncrementalStatistics {
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
    Real weightSum() const;
    Real mean() const;
    Real variance() const;
    Real standardDeviation() const;
    Real errorEstimate() const;
    Real skewness() const;
    Real kurtosis() const;
    Real min() const;
    Real max() const;
    // Modifiers
    void reset();
    void add(Real value, Real weight = 1.0);
    %extend {
        void add(const std::vector<Real>& values) {
            self->addSequence(values.begin(), values.end());
        }
        void add(const std::vector<Real>& values, 
                 const std::vector<Real>& weights) {
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
    Real semiVariance() const;
    Real semiDeviation() const;
    Real downsideVariance() const;
    Real downsideDeviation() const;
    Real regret(Real target) const;
    Real potentialUpside(Real percentile) const;
    Real valueAtRisk(Real percentile) const;
    Real expectedShortfall(Real percentile) const;
    Real shortfall(Real target) const;
    Real averageShortfall(Real target) const;
};

template <class S>
class GenericSequenceStatistics {
    #if defined(SWIGRUBY)
    %rename("reset!")                reset;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("weight-sum")            weightSum;
    %rename("standard-deviation")    standardDeviation;
    %rename("error-estimate")        errorEstimate;
    %rename("reset!")                reset;
    #endif
  public:
    GenericSequenceStatistics(Size dimension);
    Size size() const;
    Size samples() const;
    Real weightSum() const;
    std::vector<Real> mean() const;
    std::vector<Real> variance() const;
    std::vector<Real> standardDeviation() const;
    std::vector<Real> errorEstimate() const;
    std::vector<Real> skewness() const;
    std::vector<Real> kurtosis() const;
    std::vector<Real> min() const;
    std::vector<Real> max() const;
    Matrix covariance() const;
    Matrix correlation() const;
    // Modifiers
    void reset();
    void add(const std::vector<Real>& value, Real weight = 1.0);
    void add(const Array& value, Real weight = 1.0);
};

%template(MultipleStatistics) GenericSequenceStatistics<Statistics>;
%template(SequenceStatistics) GenericSequenceStatistics<RiskStatistics>;

%template(MultipleIncrementalStatistics)
						GenericSequenceStatistics<IncrementalStatistics>;


#endif
