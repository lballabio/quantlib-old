
/*
 Copyright (C) 2007 François du Vignaud

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

#ifndef qla_historicalforwardratesanalysisfactory_hpp
#define qla_historicalforwardratesanalysisfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/types.hpp>
#include <ql/models/marketmodels/historicalforwardratesanalysis.hpp>

namespace QuantLib {
    class Calendar;
    class DayCounter;
    class RateHelper;
}

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis>(*HistoricalForwardRatesAnalysisConstructor)(
                const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
                const QuantLib::Date& startDate,
                const QuantLib::Date& endDate,
                const QuantLib::Period& step,
                const boost::shared_ptr<QuantLib::InterestRateIndex>& fwdIndex,
                const QuantLib::Period& initialGap,
                const QuantLib::Period& horizon,
                const std::vector<boost::shared_ptr<QuantLib::IborIndex> >& iborIndexes,
                const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >& swapIndexes,
                const QuantLib::DayCounter& yieldCurveDayCounter,
                QuantLib::Real yieldCurveAccuracy);

    template<>
    class Create<boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis> > :
        private RegistryManager<QuantLib::HistoricalForwardRatesAnalysis,
                                EnumPairRegistry> {
    public:
        boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis> operator() (
                const std::string& traitsID,
                const std::string& interpolatorID,
                const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
                const QuantLib::Date& startDate,
                const QuantLib::Date& endDate,
                const QuantLib::Period& step,
                const boost::shared_ptr<QuantLib::InterestRateIndex>& fwdIndex,
                const QuantLib::Period& initialGap,
                const QuantLib::Period& horizon,
                const std::vector<boost::shared_ptr<QuantLib::IborIndex> >& iborIndexes,
                const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >& swapIndexes,
                const QuantLib::DayCounter& yieldCurveDayCounter,
                QuantLib::Real yieldCurveAccuracy) {
            KeyPair key(traitsID, interpolatorID);
            HistoricalForwardRatesAnalysisConstructor historicalForwardRatesAnalysisConstructor =
                reinterpret_cast<HistoricalForwardRatesAnalysisConstructor>(getType(key));
            return historicalForwardRatesAnalysisConstructor(
                stats,
                startDate,
                endDate,
                step,
                fwdIndex,
                initialGap,
                horizon,
                iborIndexes,
                swapIndexes,
                yieldCurveDayCounter,
                yieldCurveAccuracy);
        }
        using RegistryManager<QuantLib::HistoricalForwardRatesAnalysis,
                              EnumPairRegistry>::registerType;
    };

 }

#endif
