/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/statistics.hpp>
#include <ql/math/statistics/statistics.hpp>
#include <ql/math/statistics/incrementalstatistics.hpp>

namespace QuantLibAddin {

    Statistics::Statistics(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Real>& values, 
        const std::vector<QuantLib::Real>& w,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::Statistics>(properties, permanent)
    {               
        libraryObject_ = boost::shared_ptr<QuantLib::Statistics>(new
            QuantLib::Statistics());

        QL_REQUIRE(w.empty() || values.size()==w.size(),
                   "Mismatch between number of samples (" <<
                   values.size() << ") and number of weights (" <<
                   w.size() << ")");

        if (!values.empty()) {
            if (!w.empty())
                libraryObject_->addSequence(values.begin(),
                                            values.end(),
                                            w.begin());
            else
                libraryObject_->addSequence(values.begin(),
                                            values.end());
        }
    }

    IncrementalStatistics::IncrementalStatistics(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Real>& values, 
        const std::vector<QuantLib::Real>& w,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::IncrementalStatistics>(properties, permanent)
    {               
        libraryObject_ = boost::shared_ptr<QuantLib::IncrementalStatistics>(new
            QuantLib::IncrementalStatistics());

        QL_REQUIRE(w.empty() || values.size()==w.size(),
                   "Mismatch between number of samples (" <<
                   values.size() << ") and number of weights (" <<
                   w.size() << ")");

        if (!values.empty()) {
            if (!w.empty())
                libraryObject_->addSequence(values.begin(),
                                            values.end(),
                                            w.begin());
            else
                libraryObject_->addSequence(values.begin(),
                                            values.end());
        }
    }

    #define TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(NAME) \
    QuantLib::Real NAME(QuantLib::Real mean, QuantLib::Real stdDev) { \
        QuantLib::StatsHolder h(mean, stdDev); \
        QuantLib::GenericGaussianStatistics< QuantLib::StatsHolder > s(h); \
        return s.NAME(); \
    }

    TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideVariance)
    TYPICAL_GAUSSIAN_2DOUBLE_STAT_FUNCTION(gaussianDownsideDeviation)



    #define TYPICAL_GAUSSIAN_3DOUBLE_STAT_FUNCTION(NAME) \
    QuantLib::Real NAME(QuantLib::Real x, QuantLib::Real mean, QuantLib::Real stdDev) { \
        QuantLib::StatsHolder h(mean, stdDev); \
        QuantLib::GenericGaussianStatistics< QuantLib::StatsHolder > s(h); \
        return s.NAME(x); \
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
