
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

#include <oh/objecthandler.hpp>
#include <ql/models/marketmodels/marketmodel.hpp>
#include <ql/legacy/libormarketmodels/lmextlinexpvolmodel.hpp>
#include <ql/models/marketmodels/accountingengine.hpp>
#include <ql/models/marketmodels/browniangenerator.hpp>
#include <ql/models/marketmodels/models/abcdvol.hpp>
#include <ql/models/marketmodels/models/ctsmmcapletcalibration.hpp>
#include <ql/models/marketmodels/models/alphaform.hpp>

namespace QuantLibAddin {


    class CTSMMCapletCalibration : public
        ObjectHandler::LibraryObject<QuantLib::CTSMMCapletCalibration> {
    };

    class CTSMMCapletOriginalCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletOriginalCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            const std::vector<QuantLib::Real>& alpha,
            bool lowestRoot,
			bool useFullApprox);
    };

    class CTSMMCapletAlphaFormCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletAlphaFormCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            const std::vector<QuantLib::Real>& alphaInitial,
            const std::vector<QuantLib::Real>& alphaMax,
            const std::vector<QuantLib::Real>& alphaMin,
            bool maximizeHomogeneity,
            boost::shared_ptr<QuantLib::AlphaForm>& parametricForm);
    };

    class CTSMMCapletMaxHomogeneityCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletMaxHomogeneityCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            QuantLib::Real caplet0Swaption1Priority);
    };


    // MarketModels
    class MarketModel : public ObjectHandler::LibraryObject<QuantLib::MarketModel> {
    };

    class FlatVol : public MarketModel {
    public:
        FlatVol(
            const std::vector<QuantLib::Volatility>& volatilities,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements);
    };


    class AbcdVol : public MarketModel {
    public:
        AbcdVol(
            QuantLib::Real a,
            QuantLib::Real b,
            QuantLib::Real c,
            QuantLib::Real d,
            const std::vector<QuantLib::Real>& ks,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements);
    };

    class PseudoRootFacade : public MarketModel {
      public:
        PseudoRootFacade(
              const boost::shared_ptr<QuantLib::CTSMMCapletCalibration> calibrator);
    };

    class CotSwapToFwdAdapter : public MarketModel {
      public:
        FwdToCotSwapAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& coterminalModel);
    };

    class FwdPeriodAdapter : public MarketModel {
      public:
        FwdPeriodAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& largeModel,
              QuantLib::Size period,
              QuantLib::Size offset,
              const std::vector<QuantLib::Spread>& newDisplacements_ );
    };

    class FwdToCotSwapAdapter : public MarketModel {
      public:
        FwdToCotSwapAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& forwardModel);
    };

    // MarketModelFactories
    class MarketModelFactory : public ObjectHandler::LibraryObject<QuantLib::MarketModelFactory> {
    };

    class FlatVolFactory : public MarketModelFactory {
    public:
        FlatVolFactory(QuantLib::Real longTermCorr,
                              QuantLib::Real beta,
                              const std::vector<QuantLib::Time>& times,
                              const std::vector<QuantLib::Volatility>& vols,
                              const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldCurve,
                              QuantLib::Spread displacement);
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

    class BrownianGeneratorFactory : public ObjectHandler::LibraryObject<
        QuantLib::BrownianGeneratorFactory> {
    };

    class MTBrownianGeneratorFactory : public BrownianGeneratorFactory {
    public:
        MTBrownianGeneratorFactory(unsigned long seed);
    };


    class AccountingEngine : public ObjectHandler::LibraryObject<
        QuantLib::AccountingEngine> {
    public:
        AccountingEngine(
            const boost::shared_ptr<QuantLib::MarketModelEvolver>& evolver,
            const QuantLib::Clone<QuantLib::MarketModelMultiProduct>& product,
            double initialNumeraireValue);
    };
    
     std::vector<QuantLib::Real> qlRateVolDifferences(
                            const QuantLib::MarketModel&,
                            const QuantLib::MarketModel&);

     std::vector<QuantLib::Real> qlRateInstVolDifferences(
                            const QuantLib::MarketModel&,
                            const QuantLib::MarketModel&,
                            QuantLib::Size);  
}

#endif
