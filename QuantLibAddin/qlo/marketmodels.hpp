
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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

#ifndef qla_market_models_hpp
#define qla_market_models_hpp

#include <oh/objhandler.hpp>

#include <ql/instruments/payoffs.hpp>
#include <ql/models/marketmodels/accountingengine.hpp>
#include <ql/models/marketmodels/marketmodel.hpp>
#include <ql/models/marketmodels/evolutiondescription.hpp>
#include <ql/models/marketmodels/curvestate.hpp>
#include <ql/models/marketmodels/driftcomputation/lmmdriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/cmsmmdriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/lmmnormaldriftcalculator.hpp>
#include <ql/models/marketmodels/driftcomputation/smmdriftcalculator.hpp>
#include <ql/models/marketmodels/browniangenerator.hpp>
#include <ql/models/marketmodels/marketmodelevolver.hpp>
#include <ql/models/marketmodels/models/abcdvol.hpp>
#include <ql/models/marketmodels/curvestates/cmswapcurvestate.hpp>
#include <ql/models/marketmodels/curvestates/coterminalswapcurvestate.hpp>
#include <ql/models/marketmodels/curvestates/lmmcurvestate.hpp>
#include <ql/models/marketmodels/models/piecewiseconstantabcdvariance.hpp>
#include <ql/models/marketmodels/models/timedependantcorrelationstructures/cotswapfromfwdcorrelation.hpp>
#include <ql/models/marketmodels/models/capletcoterminalswaptioncalibration.hpp>
#include <ql/models/marketmodels/products/multiproductcomposite.hpp>
#include <ql/legacy/libormarketmodels/lmextlinexpvolmodel.hpp>
#include <ql/legacy/libormarketmodels/lmlinexpcorrmodel.hpp>
#include <ql/termstructures/volatilities/abcd.hpp>
#include <ql/models/marketmodels/models/timedependantcorrelationstructures/timehomogeneousforwardcorrelation.hpp>

namespace QuantLibAddin {

    class TimeDependantCorrelationStructure :
        public ObjHandler::LibraryObject<QuantLib::TimeDependantCorrelationStructure>{};


    class TimeHomogeneousForwardCorrelation : public TimeDependantCorrelationStructure {
        public:
            TimeHomogeneousForwardCorrelation(
                const QuantLib::Matrix& fwdCorrelation,
                const std::vector<QuantLib::Time>& rateTimes,
                QuantLib::Size numberOfFactors);
            const QuantLib::Matrix& pseudoRoot(QuantLib::Size i) const;
    };

    class CotSwapFromFwdCorrelation : public TimeDependantCorrelationStructure {
        public:
            CotSwapFromFwdCorrelation(
            const QuantLib::Matrix& correlations,
            const QuantLib::CurveState& curveState,
            QuantLib::Real displacement,
            const QuantLib::EvolutionDescription& evolution,
            QuantLib::Size numberOfFactors);
    };

    class PiecewiseConstantVariance: public ObjHandler::LibraryObject<QuantLib::PiecewiseConstantVariance>{};

    class PiecewiseConstantAbcdVariance : public PiecewiseConstantVariance {
      public:
        PiecewiseConstantAbcdVariance(QuantLib::Real a, QuantLib::Real b,
                                      QuantLib::Real c, QuantLib::Real d,
                                      const QuantLib::Size resetIndex,
                                      const std::vector<QuantLib::Time>& rateTimes);

    };

    //QuantLib::Matrix capletCoterminalSwaptionCalibrationFunction(
    //        const QuantLib::EvolutionDescription& evolution,
    //        const QuantLib::TimeDependantCorrelationStructure& corr,
    //        const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
    //        const std::vector<QuantLib::Volatility>& capletVols,
    //        const QuantLib::CurveState& cs,
    //        const QuantLib::Spread displacement,
    //        const std::vector<QuantLib::Real>& alpha,
    //        bool lowestRoot,
    //        QuantLib::Size timeIndex);

    class CapletCoterminalSwaptionCalibration : public
        ObjHandler::LibraryObject<QuantLib::CapletCoterminalSwaptionCalibration> {
      public:
        CapletCoterminalSwaptionCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::TimeDependantCorrelationStructure>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement);
    };

    class EvolutionDescription : public ObjHandler::LibraryObject<QuantLib::EvolutionDescription> {
    public:
        EvolutionDescription(
            const std::vector<QuantLib::Time>& rateTimes,
            const std::vector<QuantLib::Time>& evolutionTimes);
        EvolutionDescription(
            const QuantLib::EvolutionDescription& ev);
        EvolutionDescription(
            const QuantLib::MarketModelMultiProduct& product);
    };

    class Abcd : public ObjHandler::LibraryObject<QuantLib::Abcd> {
    public:
        Abcd(QuantLib::Real a,
             QuantLib::Real b,
             QuantLib::Real c,
             QuantLib::Real d,
             bool aIsFixed,
             bool bIsFixed,
             bool cIsFixed,
             bool dIsFixed);
    };

    // MarketModels
    class MarketModel : public ObjHandler::LibraryObject<QuantLib::MarketModel> {
    };

    class FlatVol : public MarketModel {
    public:
        FlatVol(
            const std::vector<QuantLib::Volatility>& volatilities,
            const boost::shared_ptr<QuantLib::TimeDependantCorrelationStructure>& corr,
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
            const boost::shared_ptr<QuantLib::TimeDependantCorrelationStructure>& corr,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements);
    };
    // MarketModelFactories
    class MarketModelFactory : public ObjHandler::LibraryObject<QuantLib::MarketModelFactory> {
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

    // CurveStates
    class CurveState : public ObjHandler::LibraryObject<QuantLib::CurveState> {};

    class CMSwapCurveState : public CurveState {
      public:
        CMSwapCurveState(std::vector<QuantLib::Time>& rateTimes, QuantLib::Size spanningForwards) {
            libraryObject_ = boost::shared_ptr<QuantLib::CMSwapCurveState>(new
                QuantLib::CMSwapCurveState(rateTimes, spanningForwards));
        }
    };

    class CoterminalSwapCurveState : public CurveState {
      public:
        CoterminalSwapCurveState(std::vector<QuantLib::Time>& rateTimes) {
            libraryObject_ = boost::shared_ptr<QuantLib::CoterminalSwapCurveState>(new
                QuantLib::CoterminalSwapCurveState(rateTimes));
        }
    };

    class LMMCurveState : public CurveState {
      public:
        LMMCurveState(std::vector<QuantLib::Time>& rateTimes) {
            libraryObject_ = boost::shared_ptr<QuantLib::LMMCurveState>(new
                QuantLib::LMMCurveState(rateTimes));
        }
    };

    class LMMDriftCalculator : public ObjHandler::LibraryObject<QuantLib::LMMDriftCalculator> {
      public:
        LMMDriftCalculator(const QuantLib::Matrix& pseudo,
                           const std::vector<QuantLib::Rate>& displacements,
                           const std::vector<QuantLib::Time>& taus,
                           QuantLib::Size numeraire,
                           QuantLib::Size alive);
        std::vector<QuantLib::Real> compute(
            const QuantLib::LMMCurveState& cs) const;
        std::vector<QuantLib::Real> computePlain(
            const QuantLib::LMMCurveState& cs) const;
        std::vector<QuantLib::Real> computeReduced(
            const QuantLib::LMMCurveState& cs) const;
      private:
        mutable std::vector<QuantLib::Real> drifts_;
    };

    class LMMNormalDriftCalculator : public ObjHandler::LibraryObject<QuantLib::LMMNormalDriftCalculator> {
      public:
        LMMNormalDriftCalculator(const QuantLib::Matrix& pseudo,
                        const std::vector<QuantLib::Time>& taus,
                        QuantLib::Size numeraire,
                        QuantLib::Size alive);
        std::vector<QuantLib::Real> compute(
            const QuantLib::LMMCurveState& cs) const;
        std::vector<QuantLib::Real> computePlain(
            const QuantLib::LMMCurveState& cs) const;
        std::vector<QuantLib::Real> computeReduced(
            const QuantLib::LMMCurveState& cs) const;
      private:
        mutable std::vector<QuantLib::Real> drifts_;
    };

    class CMSMMDriftCalculator : public ObjHandler::LibraryObject<QuantLib::CMSMMDriftCalculator> {
      public:
        CMSMMDriftCalculator(const QuantLib::Matrix& pseudo,
                             const std::vector<QuantLib::Rate>& displacements,
                             const std::vector<QuantLib::Time>& taus,
                             QuantLib::Size numeraire,
                             QuantLib::Size alive,
                             QuantLib::Size spanningFwds);
        std::vector<QuantLib::Real> compute(
            const QuantLib::CMSwapCurveState& cs) const;
      private:
        mutable std::vector<QuantLib::Real> drifts_;
    };

    class SMMDriftCalculator : public ObjHandler::LibraryObject<QuantLib::SMMDriftCalculator> {
      public:
        SMMDriftCalculator(const QuantLib::Matrix& pseudo,
                           const std::vector<QuantLib::Rate>& displacements,
                           const std::vector<QuantLib::Time>& taus,
                           QuantLib::Size numeraire,
                           QuantLib::Size alive);
        std::vector<QuantLib::Real> compute(
            const QuantLib::CoterminalSwapCurveState& cs) const;
      private:
        mutable std::vector<QuantLib::Real> drifts_;
    };

    //class SwapCovarianceApproximator : public ObjHandler::LibraryObject<
    //    QuantLib::SwapCovarianceApproximator> {
    //  public:
    //    SwapCovarianceApproximator(const QuantLib::CurveState& initialCurveState,
    //                               QuantLib::Size expiry,
    //                               QuantLib::Size maturity,
    //                               QuantLib::Spread displacement,
    //                               const QuantLib::Matrix& forwardCovarianceMatrix);
    //    QuantLib::Disposable<QuantLib::Matrix> swapCovarianceMatrix();
    //};

    class MarketModelMultiProduct : public ObjHandler::LibraryObject<
        QuantLib::MarketModelMultiProduct> {
      public:
          std::string evolution() const;
    };

    class MarketModelComposite : public MarketModelMultiProduct {};

    class MultiProductComposite : public MarketModelComposite {
        public:
            MultiProductComposite(){
                libraryObject_ = boost::shared_ptr<QuantLib::MultiProductComposite>(
                    new QuantLib::MultiProductComposite());};
    };

    class OneStepForwards : public MarketModelMultiProduct {
    public:
        OneStepForwards(const std::vector<QuantLib::Time>& rateTimes,
                            const std::vector<QuantLib::Real>& accruals,
                            const std::vector<QuantLib::Time>& paymentTimes,
                            const std::vector<QuantLib::Rate>& strikes);
    };

    class OneStepOptionlets : public MarketModelMultiProduct {
      public:
        OneStepOptionlets(
                const std::vector<QuantLib::Time>& rateTimes,
                const std::vector<QuantLib::Real>& accruals,
                const std::vector<QuantLib::Time>& paymentTimes,
                const std::vector<boost::shared_ptr<QuantLib::Payoff> >&);
    };

    class MultiStepRatchet : public MarketModelMultiProduct {
      public:
        MultiStepRatchet(const std::vector<QuantLib::Time>& rateTimes,
                         const std::vector<QuantLib::Real>& accruals,
                         const std::vector<QuantLib::Time>& paymentTimes,
                         QuantLib::Real gearingOfFloor,
                         QuantLib::Real gearingOfFixing,
                         QuantLib::Rate spreadOfFloor,
                         QuantLib::Rate spreadOfFixing,
                         QuantLib::Real initialFloor,
                         bool payer);
    };

    class BrownianGeneratorFactory : public ObjHandler::LibraryObject<
        QuantLib::BrownianGeneratorFactory> {
    };

    class MTBrownianGeneratorFactory : public BrownianGeneratorFactory {
    public:
        MTBrownianGeneratorFactory(unsigned long seed);
    };

    class MarketModelEvolver : public ObjHandler::LibraryObject<
        QuantLib::MarketModelEvolver> {
    };

    class ForwardRatePcEvolver : public MarketModelEvolver {
    public:
        ForwardRatePcEvolver(const boost::shared_ptr<QuantLib::MarketModel>&,
                             const QuantLib::BrownianGeneratorFactory&,
                             const std::vector<QuantLib::Size>& numeraires);
    };

    class ForwardRateIpcEvolver : public MarketModelEvolver {
     public:
        ForwardRateIpcEvolver(const boost::shared_ptr<QuantLib::MarketModel>&,
                              const QuantLib::BrownianGeneratorFactory&,
                              const std::vector<QuantLib::Size>& numeraires);
    };
    class ForwardRateNormalPcEvolver : public MarketModelEvolver {
     public:
    ForwardRateNormalPcEvolver(const boost::shared_ptr<QuantLib::MarketModel>&,
                              const QuantLib::BrownianGeneratorFactory&,
                              const std::vector<QuantLib::Size>& numeraires);
    };

    class AccountingEngine : public ObjHandler::LibraryObject<
        QuantLib::AccountingEngine> {
    public:
        AccountingEngine(
            const boost::shared_ptr<QuantLib::MarketModelEvolver>& evolver,
            const QuantLib::Clone<QuantLib::MarketModelMultiProduct>& product,
            double initialNumeraireValue);
    };

    // Volatility Model
    class LmVolatilityModel : public ObjHandler::LibraryObject<
        QuantLib::LmVolatilityModel> { };
    class LmLinearExponentialVolatilityModel : public LmVolatilityModel { };
    class LmExtLinearExponentialVolModel : public LmLinearExponentialVolatilityModel {
    public:
        LmExtLinearExponentialVolModel(const std::vector<QuantLib::Time>& fixingTimes,
                                       QuantLib::Real a,
                                       QuantLib::Real b,
                                       QuantLib::Real c,
                                       QuantLib::Real d);

    };
    // Correlation Model
    class LmCorrelationModel : public ObjHandler::LibraryObject<
        QuantLib::LmCorrelationModel> { };
    class LmLinearExponentialCorrelationModel : public LmCorrelationModel {
     public:
        LmLinearExponentialCorrelationModel(QuantLib::Size size,
                                            QuantLib::Real rho,
                                            QuantLib::Real beta,
                                            QuantLib::Size factors);

    };
    /////
    std::vector<QuantLib::Rate> qlForwardsFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Rate> qlCoterminalSwapRatesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);
    std::vector<QuantLib::Real> qlCoterminalSwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Rate> qlConstantMaturitySwapRatesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);
    std::vector<QuantLib::Real> qlConstantMaturitySwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);



}

#endif
