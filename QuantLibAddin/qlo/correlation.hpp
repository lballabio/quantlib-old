
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti
 Copyright (C) 2007 Katiuscia Manzoni

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

#ifndef qla_correlation_hpp
#define qla_correlation_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class LmCorrelationModel;
    class PiecewiseConstantCorrelation;
    class Matrix;
    class CurveState;
    class EvolutionDescription;
    class Date;
    class Period;
    class InterestRateIndex;
    class IborIndex;
    class SwapIndex;
    class DayCounter;
    class HistoricalForwardRatesAnalysis;
    class HistoricalRatesAnalysis;

    class GeneralStatistics;

    template<class Stat>
    class GenericGaussianStatistics;

    typedef GenericGaussianStatistics<GeneralStatistics> GaussianStatistics;

    template <class S>
    class GenericRiskStatistics;

    typedef GenericRiskStatistics<GaussianStatistics> RiskStatistics;

    typedef RiskStatistics Statistics;

    template <class StatisticsType>
    class GenericSequenceStatistics;

    typedef GenericSequenceStatistics<Statistics> SequenceStatistics;
}

namespace QuantLibAddin {

    OH_LIB_CLASS(LmCorrelationModel, QuantLib::LmCorrelationModel)

    class LmLinearExponentialCorrelationModel : public LmCorrelationModel {
     public:
        LmLinearExponentialCorrelationModel(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                            QuantLib::Size size,
                                            QuantLib::Real rho,
                                            QuantLib::Real beta,
                                            QuantLib::Size factors,
                                            bool permanent);

    };

    OH_LIB_CLASS(PiecewiseConstantCorrelation, QuantLib::PiecewiseConstantCorrelation)

    class TimeHomogeneousForwardCorrelation : public PiecewiseConstantCorrelation {
      public:
        TimeHomogeneousForwardCorrelation(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                const QuantLib::Matrix& fwdCorrelation,
                const std::vector<QuantLib::Time>& rateTimes,
                bool permanent);
    };

    class ExponentialForwardCorrelation : public PiecewiseConstantCorrelation {
      public:
        ExponentialForwardCorrelation(
                                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                const std::vector<QuantLib::Time>& rateTimes,
                                QuantLib::Real longTermCorr,
                                QuantLib::Real beta,
                                QuantLib::Real gamma,
                                const std::vector<QuantLib::Time>& times,
                                bool permanent);
    };

    class CotSwapFromFwdCorrelation : public PiecewiseConstantCorrelation {
      public:
        CotSwapFromFwdCorrelation(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>&,
            const QuantLib::CurveState& curveState,
            QuantLib::Real displacement,
            bool permanent);
    };
  
    class HistoricalForwardRatesAnalysis : public
        ObjectHandler::LibraryObject<QuantLib::HistoricalForwardRatesAnalysis> {
      public:
        HistoricalForwardRatesAnalysis(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
                const QuantLib::Date& startDate,
                const QuantLib::Date& endDate,
                const QuantLib::Period& step,
                const boost::shared_ptr<QuantLib::InterestRateIndex>&,
                const QuantLib::Period& initialGap,
                const QuantLib::Period& horizon,
                const std::vector<boost::shared_ptr<QuantLib::IborIndex> >&,
                const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >&,
                const QuantLib::DayCounter& yieldCurveDayCounter,
                const std::string& traitsID, 
                const std::string& interpolatorID,
                QuantLib::Real yieldCurveAccuracy,
                bool permanent);
    };

    class HistoricalRatesAnalysis : public
        ObjectHandler::LibraryObject<QuantLib::HistoricalRatesAnalysis> {
      public:
        HistoricalRatesAnalysis(
                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
                const QuantLib::Date& startDate,
                const QuantLib::Date& endDate,
                const QuantLib::Period& step,
                const std::vector<boost::shared_ptr<QuantLib::InterestRateIndex> >&,
                bool permanent);
    };


}

#endif

