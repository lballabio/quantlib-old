
/*
 Copyright (C) 2005 Eric Ehlers

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
    #include <qla/config.hpp>
#endif
#include <oh/objhandler.hpp>
#include <qla/objects/optionutils.hpp>
#include <ql/PricingEngines/all.hpp>

namespace QuantLibAddin {

    QuantLib::Option::Type IDtoOptionType(const std::string &optionTypeID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(optionTypeID);
        if (idUpper.compare("PUT") ==0)
            return QuantLib::Option::Put;
        else if (idUpper.compare("CALL") == 0)
            return QuantLib::Option::Call;
        else
            QL_FAIL("IDtoOptionType: unrecognized optionTypeID: " + optionTypeID);
    }

    boost::shared_ptr<QuantLib::StrikedTypePayoff> IDtoPayoff(
            const std::string &optionTypeID,
            const std::string &payoffID,
            const double &input1,
            const double &input2) {
        QuantLib::Option::Type type = IDtoOptionType(optionTypeID);
        std::string idUpper = QuantLib::StringFormatter::toUppercase(payoffID);
        if (idUpper.compare("AON") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::AssetOrNothingPayoff(type, input1));
        else if (idUpper.compare("CON") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::CashOrNothingPayoff(type, input1, input2));
        else if (idUpper.compare("GAP") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::GapPayoff(type, input1, input2));
        else if (idUpper.compare("PSP") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::PercentageStrikePayoff(type, input1));
        else if (idUpper.compare("VAN") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::PlainVanillaPayoff(type, input1));
        else if (idUpper.compare("SSP") == 0)
            return boost::shared_ptr<QuantLib::StrikedTypePayoff> (
                new QuantLib::SuperSharePayoff(type, input1, input2));
        else
            QL_FAIL("IDtoPayoff: unrecognized payoffID: " + payoffID);
    }

    boost::shared_ptr<QuantLib::Exercise> IDtoExercise(
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(exerciseID);
        if (idUpper.compare("AM") == 0)
            return boost::shared_ptr<QuantLib::Exercise> (
                new QuantLib::AmericanExercise(QuantLib::Date(settlementDate), 
                QuantLib::Date(exerciseDate)));
        else if (idUpper.compare("EU") == 0)
            return boost::shared_ptr<QuantLib::Exercise> (
                new QuantLib::EuropeanExercise(QuantLib::Date(exerciseDate)));
        else
            QL_FAIL("IDtoExercise: unrecognized exerciseTypeID: " + exerciseID);
    }

    boost::shared_ptr<QuantLib::PricingEngine> IDtoEngine(
            const std::string &engineID,
            const long &timeSteps) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(engineID);
        if (idUpper.compare("AB") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticBarrierEngine);
        else if (idUpper.compare("AC") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticCliquetEngine);
        else if (idUpper.compare("ACGAPA") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticContinuousGeometricAveragePriceAsianEngine);
        else if (idUpper.compare("ADA") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticDigitalAmericanEngine);
        else if (idUpper.compare("ADGAPA") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticDiscreteGeometricAveragePriceAsianEngine);
        else if (idUpper.compare("ADE") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticDividendEuropeanEngine);
        else if (idUpper.compare("AE") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticEuropeanEngine);
        else if (idUpper.compare("AP") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::AnalyticPerformanceEngine);
        else if (idUpper.compare("BAWA") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BaroneAdesiWhaleyApproximationEngine);
        else if (idUpper.compare("AEQPB") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::AdditiveEQPBinomialTree>(timeSteps));
        else if (idUpper.compare("CRR") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::CoxRossRubinstein>(timeSteps));
        else if (idUpper.compare("JR") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::JarrowRudd>(timeSteps));
        else if (idUpper.compare("LR") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::LeisenReimer>(timeSteps));
        else if (idUpper.compare("TIAN") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::Tian>(timeSteps));
        else if (idUpper.compare("TRI") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BinomialVanillaEngine<QuantLib::Trigeorgis>(timeSteps));       
        else if (idUpper.compare("BSA") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::BjerksundStenslandApproximationEngine);
//        else if (idUpper.compare("JQA") == 0)
//            return boost::shared_ptr<QuantLib::PricingEngine> (
//                new QuantLib::JuQuadraticApproximationEngine);
        else if (idUpper.compare("PE") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine>();
        else if (idUpper.compare("SE") == 0)
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::StulzEngine);
        else if (idUpper.compare("FE") == 0) {
            boost::shared_ptr<QuantLib::VanillaOption::engine> 
                underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::ForwardEngine<QuantLib::VanillaOption::arguments,
                    QuantLib::VanillaOption::results>(underlyingEngine));
        } else if (idUpper.compare("FPE") == 0) {
            boost::shared_ptr<QuantLib::VanillaOption::engine> 
                underlyingEngine(new QuantLib::AnalyticEuropeanEngine);    
            return boost::shared_ptr<QuantLib::PricingEngine> (
                new QuantLib::ForwardPerformanceEngine
                    <QuantLib::VanillaOption::arguments,
                    QuantLib::VanillaOption::results>(underlyingEngine));
        } else
            QL_FAIL("IDtoEngine: unrecognized engineID: " + engineID);
    }

    QuantLib::Matrix vectorVectorToMatrix(
        const std::vector < std::vector < double > > &vv) {
        if (vv.size() == 0) {
            QuantLib::Matrix m(0, 0);
            return m;
        } else {
            const std::vector < double > v = vv[0];
            if (v.size() == 0) {
                QuantLib::Matrix m(0, 0);
                return m;
            }
            QuantLib::Matrix m(vv.size(), v.size());
            for (unsigned int i = 0; i < vv.size(); i++) {
                const std::vector < double > v = vv[i];
                for (unsigned int j = 0; j < v.size(); j++)
                    m[i][j] = v[j];
            }
            return m;
        }
    }

    std::vector<QuantLib::Date> longVectorToDateVector(
            const std::vector < long > &v) {
        std::vector<QuantLib::Date> ret;
        std::vector<long>::const_iterator i;
        for (i=v.begin(); i!=v.end(); i++)
            ret.push_back(QuantLib::Date(*i));
        return ret;
    }

}

