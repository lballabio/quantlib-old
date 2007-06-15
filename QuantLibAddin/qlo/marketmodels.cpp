
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/marketmodels.hpp>
#include <ql/models/marketmodels/models/fwdperiodadapter.hpp>
#include <ql/models/marketmodels/models/fwdtocotswapadapter.hpp>
#include <ql/models/marketmodels/models/pseudorootfacade.hpp>
#include <ql/models/marketmodels/models/flatvol.hpp>
#include <ql/models/marketmodels/browniangenerators/mtbrowniangenerator.hpp>
#include <ql/models/marketmodels/marketmodeldifferences.hpp>
#include <ql/models/marketmodels/models/capletcoterminalswaptioncalibration.hpp>
#include <ql/models/marketmodels/models/capletcoterminalalphacalibration.hpp>
#include <ql/models/marketmodels/models/capletcoterminalmaxhomogeneity.hpp>

namespace QuantLibAddin {

    CTSMMCapletOriginalCalibration::CTSMMCapletOriginalCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            const std::vector<QuantLib::Real>& alpha,
            bool lowestRoot,
			bool useFullApprox)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::CTSMMCapletCalibration>(new
                QuantLib::CTSMMCapletOriginalCalibration(
                    evolution, corr, swapVariances, capletVols, cs,
                    displacement, alpha, lowestRoot, useFullApprox));
    }

    CTSMMCapletAlphaFormCalibration::CTSMMCapletAlphaFormCalibration(
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
            boost::shared_ptr<QuantLib::AlphaForm>& parametricForm)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::CTSMMCapletCalibration>(new
                QuantLib::CTSMMCapletAlphaFormCalibration(
                    evolution, corr, swapVariances, capletVols, cs,
                    displacement, alphaInitial, alphaMax, alphaMin,
                    maximizeHomogeneity, parametricForm));
    }

    CTSMMCapletMaxHomogeneityCalibration::CTSMMCapletMaxHomogeneityCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            QuantLib::Real caplet0Swaption1Priority)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::CTSMMCapletCalibration>(new
                QuantLib::CTSMMCapletMaxHomogeneityCalibration(
                    evolution, corr, swapVariances, capletVols, cs,
                    displacement, caplet0Swaption1Priority));
    }

   

    FlatVol::FlatVol(
            const std::vector<QuantLib::Volatility>& volatilities,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements) {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::FlatVol(volatilities,
                                     corr,
                                     evolution,
                                     numberOfFactors,
                                     initialRates,
                                     displacements));
    }

    AbcdVol::AbcdVol(
            QuantLib::Real a,
            QuantLib::Real b,
            QuantLib::Real c,
            QuantLib::Real d,
            const std::vector<QuantLib::Real>& ks,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements) {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::AbcdVol(a, b, c, d, ks,
                                     corr,
                                     evolution,
                                     numberOfFactors,
                                     initialRates,
                                     displacements));
    }

    PseudoRootFacade::PseudoRootFacade(
              const boost::shared_ptr<QuantLib::CTSMMCapletCalibration> calibrator) {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::PseudoRootFacade(calibrator));
    }

    CotSwapToFwdAdapter::CotSwapToFwdAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& coterminalModel) {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::CotSwapToFwdAdapter(coterminalModel));
    }

    FwdPeriodAdapter::FwdPeriodAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& largeModel,
              QuantLib::Size period,
              QuantLib::Size offset,
              const std::vector<QuantLib::Spread>& newDisplacements){
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::FwdPeriodAdapter(largeModel,
                                       period,
                                       offset,
                                       newDisplacements));
    }

    FwdToCotSwapAdapter::FwdToCotSwapAdapter(
              const boost::shared_ptr<QuantLib::MarketModel>& forwardModel) {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(new
            QuantLib::FwdToCotSwapAdapter(forwardModel));
    }

    FlatVolFactory::FlatVolFactory(
        QuantLib::Real longTermCorr,
        QuantLib::Real beta,
        const std::vector<QuantLib::Time>& times,
        const std::vector<QuantLib::Volatility>& vols,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldCurve,
        QuantLib::Spread displacement)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelFactory>(
            new QuantLib::FlatVolFactory(longTermCorr,
                                         beta,
                                         times,
                                         vols,
                                         yieldCurve,
                                         displacement));
    }

    //SwapCovarianceApproximator::SwapCovarianceApproximator(
    //                            const QuantLib::CurveState& initialCurveState,
    //                            QuantLib::Size expiry,
    //                            QuantLib::Size maturity,
    //                            QuantLib::Spread displacement,
    //                            const QuantLib::Matrix& forwardCovarianceMatrix)
    //{
    //    libraryObject_ = boost::shared_ptr<QuantLib::SwapCovarianceApproximator>(
    //        new QuantLib::SwapCovarianceApproximator(
    //                        initialCurveState, expiry,maturity, displacement,
    //                        forwardCovarianceMatrix));
    //}

    //QuantLib::Disposable<QuantLib::Matrix> SwapCovarianceApproximator::swapCovarianceMatrix()
    //    {
    //    QuantLib::Matrix result;
    //    libraryObject_->swapCovarianceMatrix();
    //    return result;
    //}

    MTBrownianGeneratorFactory::MTBrownianGeneratorFactory(unsigned long seed)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::BrownianGeneratorFactory>(
            new QuantLib::MTBrownianGeneratorFactory(seed));
    }

    
    AccountingEngine::AccountingEngine(
        const boost::shared_ptr<QuantLib::MarketModelEvolver>& evolver,
        const QuantLib::Clone<QuantLib::MarketModelMultiProduct>& product,
        double initialNumeraireValue)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::AccountingEngine>(
            new QuantLib::AccountingEngine(evolver,
                                           //*(product.get()),
                                           product,
                                           initialNumeraireValue));
    }
       
    std::vector<QuantLib::Real> qlRateVolDifferences(
       const QuantLib::MarketModel& marketModel1,
       const QuantLib::MarketModel& marketModel2){
           return QuantLib::rateVolDifferences(marketModel1, marketModel2);
   }

   std::vector<QuantLib::Real> qlRateInstVolDifferences(
                            const QuantLib::MarketModel& marketModel1,
                            const QuantLib::MarketModel& marketModel2, 
                            QuantLib::Size index){
        return QuantLib::rateInstVolDifferences(marketModel1, marketModel2, index);
   }
    

}
