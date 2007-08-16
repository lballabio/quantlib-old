
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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

#ifndef qla_riskstatistics_hpp
#define qla_riskstatistics_hpp

#include <oh/libraryobject.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class GeneralStatistics;

    template<class Stat>
    class GenericGaussianStatistics;

    typedef GenericGaussianStatistics<GeneralStatistics> GaussianStatistics;

    template <class S>
    class GenericRiskStatistics;

    typedef GenericRiskStatistics<GaussianStatistics> RiskStatistics;

    typedef RiskStatistics Statistics;

    class StatsHolder;
}

namespace QuantLibAddin {

    class Statistics : 
                public ObjectHandler::LibraryObject<QuantLib::Statistics> {
      public:
        Statistics(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                   const std::vector<QuantLib::Real>& values, 
                   const std::vector<QuantLib::Real>& weights,
                   bool permanent);
    };


    #define DECLARE_TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(NAME) \
    QuantLib::Real NAME(QuantLib::Real, QuantLib::Real);

    DECLARE_TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideVariance)
    DECLARE_TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideDeviation)



    #define DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(NAME) \
    QuantLib::Real NAME(QuantLib::Real, QuantLib::Real, QuantLib::Real);

    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianRegret)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianPercentile)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianTopPercentile)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianPotentialUpside)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianValueAtRisk)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianExpectedShortfall)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianShortfall)
    DECLARE_TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianAverageShortfall)

}

#endif

