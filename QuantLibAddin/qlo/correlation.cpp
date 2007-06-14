
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/correlation.hpp>
#include <ql/legacy/libormarketmodels/lmlinexpcorrmodel.hpp>
#include <ql/models/marketmodels/correlations/cotswapfromfwdcorrelation.hpp>
#include <ql/models/marketmodels/correlations/timehomogeneousforwardcorrelation.hpp>
#include <ql/models/marketmodels/correlations/expcorrelations.hpp>
#include <ql/models/marketmodels/historicalcorrelation.hpp>
#include <ql/math/statistics/sequencestatistics.hpp>

namespace QuantLibAddin {
      
    //Correlation model
    LmLinearExponentialCorrelationModel::LmLinearExponentialCorrelationModel(
        QuantLib::Size size,
        QuantLib::Real rho,
        QuantLib::Real beta,
        QuantLib::Size factors) {

            libraryObject_ = boost::shared_ptr<QuantLib::LmLinearExponentialCorrelationModel>(
                new QuantLib::LmLinearExponentialCorrelationModel(size,rho,beta,factors));

    }
    
    TimeHomogeneousForwardCorrelation::TimeHomogeneousForwardCorrelation(
           const QuantLib::Matrix& fwdCorrelation,
           const std::vector<QuantLib::Time>& rateTimes)
    {
        QL_REQUIRE(!rateTimes.empty(), "rate times vector is empty!");
        libraryObject_ =
            boost::shared_ptr<QuantLib::TimeHomogeneousForwardCorrelation>(
                new QuantLib::TimeHomogeneousForwardCorrelation(
                    fwdCorrelation, rateTimes));
    }

    ExponentialForwardCorrelation::ExponentialForwardCorrelation(
                                const std::vector<QuantLib::Time>& rateTimes,
                                QuantLib::Real longTermCorr,
                                QuantLib::Real beta,
                                QuantLib::Real gamma,
                                const std::vector<QuantLib::Time>& times) {
        QL_REQUIRE(!rateTimes.empty(), "rate times vector is empty!");
        libraryObject_ =
            boost::shared_ptr<QuantLib::ExponentialForwardCorrelation>(new
                QuantLib::ExponentialForwardCorrelation(rateTimes,
                                                        longTermCorr,
                                                        beta,
                                                        gamma,
                                                        times));
    }

    CotSwapFromFwdCorrelation::CotSwapFromFwdCorrelation(
            const QuantLib::Matrix& correlations,
            const QuantLib::CurveState& curveState,
            QuantLib::Real displacement,
            const QuantLib::EvolutionDescription& evolution) {
        libraryObject_ =
            boost::shared_ptr<QuantLib::CotSwapFromFwdCorrelation>(new
                QuantLib::CotSwapFromFwdCorrelation(correlations,
                                                          curveState,
                                                          displacement,
                                                          evolution));
    }
        
    HistoricalCorrelation::HistoricalCorrelation(
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
        libraryObject_ = boost::shared_ptr<QuantLib::HistoricalCorrelation>(new
            QuantLib::HistoricalCorrelation(startDate,
                                            endDate,
                                            step,
                                            fwdIndex,
                                            initialGap,
                                            horizon,
                                            iborIndexes,
                                            swapIndexes,
                                            yieldCurveDayCounter,
                                            yieldCurveAccuracy));
    }

    const QuantLib::Matrix HistoricalCorrelation::correlation() const {
        return libraryObject_->stats().correlation();
    }

}
