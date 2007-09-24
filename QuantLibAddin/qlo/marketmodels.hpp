
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#ifndef qla_market_models_hpp
#define qla_market_models_hpp

#include <oh/libraryobject.hpp>

#include <ql/handle.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class MarketModel;
    class PiecewiseConstantCorrelation;
    class EvolutionDescription;
    class CTSMMCapletCalibration;
    class YieldTermStructure;
    class MarketModelFactory;
}

namespace QuantLibAddin {

    // MarketModels
    OH_LIB_CLASS(MarketModel, QuantLib::MarketModel);

    class FlatVol : public MarketModel {
    public:
        FlatVol(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Volatility>& volatilities,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements,
            bool permanent);
    };


    class AbcdVol : public MarketModel {
    public:
        AbcdVol(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Real a,
            QuantLib::Real b,
            QuantLib::Real c,
            QuantLib::Real d,
            const std::vector<QuantLib::Real>& ks,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements,
            bool permanent);
    };

    class PseudoRootFacade : public MarketModel {
      public:
        PseudoRootFacade(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::CTSMMCapletCalibration> calibrator,
            bool permanent);
    };

    class CotSwapToFwdAdapter : public MarketModel {
      public:
        CotSwapToFwdAdapter(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::MarketModel>& coterminalModel,
            bool permanent);
    };

    class FwdPeriodAdapter : public MarketModel {
      public:
        FwdPeriodAdapter(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::MarketModel>& largeModel,
            QuantLib::Size period,
            QuantLib::Size offset,
            const std::vector<QuantLib::Spread>& newDisplacements_,
            bool permanent);
    };

    class FwdToCotSwapAdapter : public MarketModel {
      public:
        FwdToCotSwapAdapter(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::MarketModel>& forwardModel,
            bool permanent);
    };

    // MarketModelFactories
    OH_LIB_CLASS(MarketModelFactory, QuantLib::MarketModelFactory);

    class FlatVolFactory : public MarketModelFactory {
    public:
        FlatVolFactory(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                       QuantLib::Real longTermCorr,
                       QuantLib::Real beta,
                       const std::vector<QuantLib::Time>& times,
                       const std::vector<QuantLib::Volatility>& vols,
                       const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldCurve,
                       QuantLib::Spread displacement,
                       bool permanent);
    };


    //class SwapCovarianceApproximator : public ObjectHandler::LibraryObject<
    //    QuantLib::SwapCovarianceApproximator> {
    //  public:
    //    SwapCovarianceApproximator(const QuantLib::CurveState& initialCurveState,
    //                               QuantLib::Size expiry,
    //                               QuantLib::Size maturity,
    //                               QuantLib::Spread displacement,
    //                               const QuantLib::Matrix& forwardCovarianceMatrix);
    //    QuantLib::Disposable<QuantLib::Matrix> swapCovarianceMatrix();
    //};

     std::vector<QuantLib::Real> qlRateVolDifferences(
                            const QuantLib::MarketModel&,
                            const QuantLib::MarketModel&);

     std::vector<QuantLib::Real> qlRateInstVolDifferences(
                            const QuantLib::MarketModel&,
                            const QuantLib::MarketModel&,
                            QuantLib::Size);  
}

#endif

