
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Duminuco Cristina

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

#ifndef qla_riskstatistics_hpp
#define qla_riskstatistics_hpp

#include <oh/objhandler.hpp>
#include <ql/math/statistics.hpp>

namespace QuantLibAddin {

    class Statistics : 
        public ObjHandler::LibraryObject<QuantLib::Statistics> {
    public:
        Statistics(std::vector<QuantLib::Real> values, 
                   std::vector<QuantLib::Real> weights);
    };


    #define TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(METHOD) \
    inline double METHOD(double mean, double stdDev) { \
        QuantLib::StatsHolder h(mean, stdDev); \
        QuantLib::GenericGaussianStatistics< QuantLib::StatsHolder > s(h); \
        return s.METHOD(); \
    }

    TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideVariance)
    TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideDeviation)



    #define TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(METHOD) \
    inline double METHOD(double x, double mean, double stdDev) { \
        QuantLib::StatsHolder h(mean, stdDev); \
        QuantLib::GenericGaussianStatistics< QuantLib::StatsHolder > s(h); \
        return s.METHOD(x); \
    }

    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianRegret)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianPercentile)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianTopPercentile)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianPotentialUpside)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianValueAtRisk)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianExpectedShortfall)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianShortfall)
    TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(gaussianAverageShortfall)

}

#endif
