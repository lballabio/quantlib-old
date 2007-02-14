
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/marketmodels.hpp>
#include <qlo/ValueObjects/vo_marketmodels.hpp>
#include <ql/MarketModels/Models/expcorrflatvol.hpp>
#include <ql/MarketModels/Models/expcorrabcdvol.hpp>
#include <ql/MarketModels/BrownianGenerators/mtbrowniangenerator.hpp>
#include <ql/MarketModels/Evolvers/forwardratepcevolver.hpp>
#include <ql/MarketModels/Evolvers/forwardrateipcevolver.hpp>
#include <ql/MarketModels/Products/OneStep/onestepforwards.hpp>
#include <ql/MarketModels/Products/OneStep/onestepoptionlets.hpp>
#include <ql/MarketModels/Products/MultiStep/multistepratchet.hpp>

namespace QuantLibAddin {

    EvolutionDescription::EvolutionDescription(
            const std::vector<QuantLib::Time>& rateTimes,
            const std::vector<QuantLib::Time>& evolutionTimes)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::EvolutionDescription>(
            new QuantLib::EvolutionDescription(rateTimes, evolutionTimes));
    }

    EvolutionDescription::EvolutionDescription(
            const QuantLib::EvolutionDescription& ev)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::EvolutionDescription>(
            new QuantLib::EvolutionDescription(ev));
    }

    ExpCorrFlatVol::ExpCorrFlatVol(
            double longTermCorr,
            double beta,
            const std::vector<double>& volatilities,
            const QuantLib::EvolutionDescription& evolution,
            const QuantLib::Size numberOfFactors,
            const std::vector<QuantLib::Rate>& initialRates,
            const std::vector<QuantLib::Rate>& displacements)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(
            new QuantLib::ExpCorrFlatVol(longTermCorr,
                                         beta,
                                         volatilities,
                                         evolution,
                                         numberOfFactors,
                                         initialRates,
                                         displacements));
    }

    ExpCorrAbcdVol::ExpCorrAbcdVol(
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
            const std::vector<QuantLib::Rate>& displacements)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModel>(
            new QuantLib::ExpCorrAbcdVol(a, b, c, d, ks,
                                         longTermCorr,
                                         beta,
                                         evolution,
                                         numberOfFactors,
                                         initialRates,
                                         displacements));
    }

    Abcd::Abcd(QuantLib::Real a, QuantLib::Real b,
               QuantLib::Real c, QuantLib::Real d,
               bool aIsFixed, bool bIsFixed,
               bool cIsFixed, bool dIsFixed) {
        libraryObject_ = boost::shared_ptr<QuantLib::Abcd>(
            new QuantLib::Abcd(a, b, c, d, 
                               aIsFixed, bIsFixed, cIsFixed, dIsFixed));
    }

    LMMDriftCalculator::LMMDriftCalculator(
                                    const QuantLib::Matrix& pseudo,
                                    const std::vector<QuantLib::Rate>& displ,
                                    const std::vector<QuantLib::Time>& taus,
                                    QuantLib::Size numeraire,
                                    QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_ = boost::shared_ptr<QuantLib::LMMDriftCalculator>(new
            QuantLib::LMMDriftCalculator(pseudo, displ,
                                         taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::compute(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::computePlain(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computePlain(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMDriftCalculator::computeReduced(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computeReduced(cs, drifts_);
        return drifts_;
    }

    LMMNormalDriftCalculator::LMMNormalDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Rate>& displ,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_ = boost::shared_ptr<QuantLib::LMMNormalDriftCalculator>(
            new QuantLib::LMMNormalDriftCalculator(pseudo, displ,
                                                   taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::compute(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::computePlain(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computePlain(cs, drifts_);
        return drifts_;
    }

    std::vector<QuantLib::Real> LMMNormalDriftCalculator::computeReduced(
                                const QuantLib::LMMCurveState& cs) const {
        libraryObject_->computeReduced(cs, drifts_);
        return drifts_;
    }

    CMSMMDriftCalculator::CMSMMDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Rate>& displ,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive,
                                QuantLib::Size spanningFwds)
    : drifts_(taus.size()) {
        libraryObject_= boost::shared_ptr<QuantLib::CMSMMDriftCalculator>(new
            QuantLib::CMSMMDriftCalculator(pseudo, displ,
                                           taus, numeraire, alive,
                                           spanningFwds));
    }

    std::vector<QuantLib::Real> CMSMMDriftCalculator::compute(
                                const QuantLib::CMSwapCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
    }

    SMMDriftCalculator::SMMDriftCalculator(
                                const QuantLib::Matrix& pseudo,
                                const std::vector<QuantLib::Rate>& displ,
                                const std::vector<QuantLib::Time>& taus,
                                QuantLib::Size numeraire,
                                QuantLib::Size alive)
    : drifts_(taus.size()) {
        libraryObject_= boost::shared_ptr<QuantLib::SMMDriftCalculator>(new
            QuantLib::SMMDriftCalculator(pseudo, displ,
                                         taus, numeraire, alive));
    }

    std::vector<QuantLib::Real> SMMDriftCalculator::compute(
                                const QuantLib::CoterminalSwapCurveState& cs) const {
        libraryObject_->compute(cs, drifts_);
        return drifts_;
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

    OneStepForwards::OneStepForwards(
        const std::vector<QuantLib::Time>& rateTimes,
        const std::vector<QuantLib::Real>& accruals,
        const std::vector<QuantLib::Time>& paymentTimes,
        const std::vector<QuantLib::Rate>& strikes)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelMultiProduct>(
            new QuantLib::OneStepForwards(rateTimes, accruals,
                                          paymentTimes, strikes));
    }

    MultiStepRatchet::MultiStepRatchet(
        const std::vector<QuantLib::Time>& rateTimes,
        const std::vector<QuantLib::Real>& accruals,
        const std::vector<QuantLib::Time>& paymentTimes,
        QuantLib::Real gearingOfFloor,
        QuantLib::Real gearingOfFixing,
        QuantLib::Rate spreadOfFloor,
        QuantLib::Rate spreadOfFixing,
        QuantLib::Real initialFloor,
        bool payer)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelMultiProduct>(
            new QuantLib::MultiStepRatchet(rateTimes, accruals,
                                          paymentTimes, 
                                          gearingOfFloor, gearingOfFixing,
                                          spreadOfFloor, spreadOfFixing,
                                          initialFloor, payer));
    }

    std::string MarketModelMultiProduct::evolution() const
    {
        const QuantLib::EvolutionDescription& ev = libraryObject_->evolution();
        boost::shared_ptr<ObjHandler::Object> objectPointer(
            new QuantLibAddin::EvolutionDescription(ev));
        std::string anonymousID =
            ObjHandler::ObjectHandler::instance().storeObject("", objectPointer);
        objectPointer->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(
            new ValueObjects::qlEvolutionDescription(
                anonymousID,
                ev.rateTimes(),
                ev.evolutionTimes())));
        return anonymousID;
    }

    OneStepOptionlets::OneStepOptionlets(
            const std::vector<QuantLib::Time>& rateTimes,
            const std::vector<QuantLib::Real>& accruals,
            const std::vector<QuantLib::Time>& paymentTimes,
            const std::vector<boost::shared_ptr<QuantLib::Payoff> >& payoffs)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::MarketModelMultiProduct>(new
                QuantLib::OneStepOptionlets(
                    rateTimes, accruals, paymentTimes, payoffs));
    }

    MTBrownianGeneratorFactory::MTBrownianGeneratorFactory(unsigned long seed)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::BrownianGeneratorFactory>(
            new QuantLib::MTBrownianGeneratorFactory(seed));
    }

    ForwardRatePcEvolver::ForwardRatePcEvolver(
        const boost::shared_ptr<QuantLib::MarketModel>& pseudoRoot,
        const QuantLib::BrownianGeneratorFactory& generatorFactory,
        const std::vector<QuantLib::Size>& numeraires)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelEvolver>(
            new QuantLib::ForwardRatePcEvolver(pseudoRoot,
                                               generatorFactory,
                                               numeraires));
    }

    ForwardRateIpcEvolver::ForwardRateIpcEvolver(
        const boost::shared_ptr<QuantLib::MarketModel>& pseudoRoot,
        const QuantLib::BrownianGeneratorFactory& generatorFactory,
        const std::vector<QuantLib::Size>& numeraires)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelEvolver>(
            new QuantLib::ForwardRateIpcEvolver(pseudoRoot,
                                                generatorFactory,
                                                numeraires));
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

}
