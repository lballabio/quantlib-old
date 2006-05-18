
/*
 Copyright (C) 2005 Plamen Neykov

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

#include <qla/typeregistry.hpp>
#include <qla/typefactory.hpp>
#include <ql/option.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Instruments/quantoforwardvanillaoption.hpp>
#include <ql/PricingEngines/all.hpp>

#define REG_ENUM(Type, Body) \
    { \
        TypeMapPtr typeMap(new TypeMap); \
        Body \
        allTypesMap[typeid(Type).name()] = typeMap; \
    }

#define MAP(str_id, constructor) \
    (*typeMap)[str_id] = (void*) constructor

namespace QuantLibAddin {

    /* *** StrikedTypePayoff *** */
    boost::shared_ptr<QuantLib::StrikedTypePayoff> SUPERSHARE_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::SuperSharePayoff(type, input1, input2));
    }
    boost::shared_ptr<QuantLib::StrikedTypePayoff> VANILLA_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::PlainVanillaPayoff(type, input1));
    }
    boost::shared_ptr<QuantLib::StrikedTypePayoff> PERCENTAGESTRIKE_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::PercentageStrikePayoff(type, input1));
    }
    boost::shared_ptr<QuantLib::StrikedTypePayoff> ASSETORNOTHING_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::AssetOrNothingPayoff(type, input1));
    }
    boost::shared_ptr<QuantLib::StrikedTypePayoff> CASHORNOTHING_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::CashOrNothingPayoff(type, input1, input2));
    }

    boost::shared_ptr<QuantLib::StrikedTypePayoff> GAP_Payoff(const std::string& optionTypeID,
        const double &input1,
        const double &input2) {
        QuantLib::Option::Type type = 
            Create<QuantLib::Option::Type>()(optionTypeID);
        return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
            new QuantLib::GapPayoff(type, input1, input2));
    }

    /* *** PricingEngine *** */
    boost::shared_ptr<QuantLib::PricingEngine> AB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticBarrierEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AC_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticCliquetEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ACGAPA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticContinuousGeometricAveragePriceAsianEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDigitalAmericanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADGAPA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDiscreteGeometricAveragePriceAsianEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> ADE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticDividendEuropeanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticEuropeanEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AP_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::AnalyticPerformanceEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> BAWA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BaroneAdesiWhaleyApproximationEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> AEQPB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::AdditiveEQPBinomialTree>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> CRR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::CoxRossRubinstein>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> I_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::IntegralEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDAmericanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDEuropeanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FDB_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::FDBermudanEngine(timeSteps, timeSteps-1));
    }
    boost::shared_ptr<QuantLib::PricingEngine> JR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::JarrowRudd>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> LR_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::LeisenReimer>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TIAN_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Tian>(timeSteps));
    }
    boost::shared_ptr<QuantLib::PricingEngine> TRI_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BinomialVanillaEngine<QuantLib::Trigeorgis>(timeSteps));       
    }
    boost::shared_ptr<QuantLib::PricingEngine> BSA_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::BjerksundStenslandApproximationEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> PE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine>();
    }
    boost::shared_ptr<QuantLib::PricingEngine> SE_Engine(const long& timeSteps) {
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::StulzEngine);
    }
    boost::shared_ptr<QuantLib::PricingEngine> FE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine> 
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> FPE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine> 
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::ForwardPerformanceEngine
                <QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> QE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine> 
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::QuantoEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
    }
    boost::shared_ptr<QuantLib::PricingEngine> QFE_Engine(const long& timeSteps) {
        boost::shared_ptr<QuantLib::VanillaOption::engine> 
            underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
        boost::shared_ptr<QuantLib::ForwardVanillaOption::engine> forwardEngine(
                new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
                QuantLib::VanillaOption::results>(underlyingEngine));
        return boost::shared_ptr<QuantLib::PricingEngine> (
            new QuantLib::QuantoEngine<QuantLib::ForwardVanillaOption::arguments,
                QuantLib::ForwardVanillaOption::results>(forwardEngine));
    }

    ComplexTypeRegistry::ComplexTypeRegistry() {
        REG_ENUM(QuantLib::StrikedTypePayoff,
            MAP("AssetOrNothing", ASSETORNOTHING_Payoff);
            MAP("CashOrNothing", CASHORNOTHING_Payoff);
            MAP("Gap", GAP_Payoff);
            MAP("PercentageStrike", PERCENTAGESTRIKE_Payoff);
            MAP("Vanilla", VANILLA_Payoff);
            MAP("SuperShare", SUPERSHARE_Payoff);
        );

        REG_ENUM(QuantLib::PricingEngine,
            MAP("AB" ,AB_Engine);
            MAP("AC", AC_Engine);
            MAP("ACGAPA", ACGAPA_Engine);
            MAP("ADA", ADA_Engine);
            MAP("ADGAPA", ADGAPA_Engine);
            MAP("ADE", ADE_Engine);
            MAP("AE", AE_Engine);
            MAP("AP", AP_Engine);
            MAP("BAWA", BAWA_Engine);
            MAP("AEQPB", AEQPB_Engine);
            MAP("CRR", CRR_Engine);
            MAP("I", I_Engine);
            MAP("FDA", FDA_Engine);
            MAP("FDE", FDE_Engine);
            MAP("FDB", FDB_Engine);
            MAP("JR", JR_Engine);
            MAP("LR", LR_Engine);
            MAP("TIAN", TIAN_Engine);
            MAP("TRI", TRI_Engine);
            MAP("BSA", BSA_Engine);
            MAP("PE", PE_Engine);
            MAP("SE", SE_Engine);
            MAP("FE", FE_Engine);
            MAP("FPE", FPE_Engine);
            MAP("QE", QE_Engine);
            MAP("QFE", QFE_Engine);
        );
    }
}

