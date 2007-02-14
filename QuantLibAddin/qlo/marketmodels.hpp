
/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#include <ql/Instruments/payoffs.hpp>
#include <ql/MarketModels/accountingengine.hpp>
#include <ql/MarketModels/marketmodel.hpp>
#include <ql/MarketModels/evolutiondescription.hpp>
#include <ql/MarketModels/curvestate.hpp>
#include <ql/MarketModels/DriftComputation/lmmdriftcalculator.hpp>
#include <ql/MarketModels/swapforwardconversionmatrix.hpp>
#include <ql/MarketModels/browniangenerator.hpp>
#include <ql/MarketModels/marketmodelevolver.hpp>
#include <ql/MarketModels/Models/expcorrabcdvol.hpp>
#include <ql/MarketModels/CurveStates/cmswapcurvestate.hpp>
#include <ql/MarketModels/CurveStates/coterminalswapcurvestate.hpp>
#include <ql/MarketModels/CurveStates/lmmcurvestate.hpp>

namespace QuantLibAddin {

    class EvolutionDescription : public ObjHandler::LibraryObject<QuantLib::EvolutionDescription> {
    public:
        EvolutionDescription(
            const std::vector<QuantLib::Time>& rateTimes,
            const std::vector<QuantLib::Time>& evolutionTimes);
        EvolutionDescription(
            const QuantLib::EvolutionDescription& ev);
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

    class MarketModel : public ObjHandler::LibraryObject<QuantLib::MarketModel> {
    };

    class ExpCorrFlatVol : public MarketModel {
    public:
        ExpCorrFlatVol(
            double longTermCorr,
            double beta,
            const std::vector<double>& volatilities,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements);
    };

    class ExpCorrAbcdVol : public MarketModel {
    public:
        ExpCorrAbcdVol(
            double a,
            double b,
            double c,
            double d,
            const std::vector<double>& ks,
            double longTermCorr,
            double beta,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements);
    };

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
        std::vector<QuantLib::Real> computePlain(
            const std::vector<QuantLib::Rate>& forwards) const;
        std::vector<QuantLib::Real> computeReduced(
            const std::vector<QuantLib::Rate>& forwards) const;
      private:
        QuantLib::Size size_;
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

    class AccountingEngine : public ObjHandler::LibraryObject<
        QuantLib::AccountingEngine> {
    public:
        AccountingEngine(
            const boost::shared_ptr<QuantLib::MarketModelEvolver>& evolver,
            const QuantLib::Clone<QuantLib::MarketModelMultiProduct>& product,
            double initialNumeraireValue);
    };


}

#endif
