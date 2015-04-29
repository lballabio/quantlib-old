/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Jose Aparicio

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

#include <qlo/basketlossmodels.hpp>

#include <ql/experimental/credit/gaussianlhplossmodel.hpp>
#include <ql/experimental/credit/constantlosslatentmodel.hpp>
#include <ql/experimental/credit/spotlosslatentmodel.hpp>
#include <ql/experimental/credit/binomiallossmodel.hpp>
#include <ql/experimental/credit/basecorrelationstructure.hpp>
#include <ql/experimental/credit/basecorrelationlossmodel.hpp>
#include <ql/experimental/credit/inhomogeneouspooldef.hpp>
#include <ql/experimental/credit/randomdefaultlatentmodel.hpp>
#include <ql/experimental/credit/randomlosslatentmodel.hpp>
#include <ql/experimental/credit/saddlepointlossmodel.hpp>
#include <ql/experimental/credit/recursivelossmodel.hpp>

namespace QuantLibAddin {

    GaussianLHPLossModel::GaussianLHPLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        QuantLib::Real correl,
        const std::vector<QuantLib::Real>& recoveryRates,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::GaussianLHPLossModel>(new 
            QuantLib::GaussianLHPLossModel(correl, recoveryRates));
    }

    IHGaussPoolLossModel::IHGaussPoolLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Real correlation,
        const std::vector<QuantLib::Real>& recoveryRates,
        const QuantLib::Size numBuckets,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
            usedFactors.push_back(std::vector<QuantLib::Real>(1, 
               std::sqrt(correlation)));

        boost::shared_ptr<QuantLib::GaussianConstantLossLM> model(new 
            QuantLib::GaussianConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::IHGaussPoolLossModel>(new 
                QuantLib::IHGaussPoolLossModel(model, numBuckets));
    }

    IHStudentPoolLossModel::IHStudentPoolLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Real correlation,
        const std::vector<QuantLib::Real>& recoveryRates,
        const std::vector<QuantLib::Real>& copulaInitVals,
        const QuantLib::Size numBuckets,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
            usedFactors.push_back(std::vector<QuantLib::Real>(1, 
               std::sqrt(correlation)));

        QuantLib::TCopulaPolicy::initTraits  initTT;
        for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
            initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                copulaInitVals[i]));

        boost::shared_ptr<QuantLib::TConstantLossLM> model(new 
            QuantLib::TConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::Trapezoid,
                initTT));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::IHStudentPoolLossModel>(new 
                QuantLib::IHStudentPoolLossModel(model, numBuckets));
    }

    GaussianBinomialLossModel::GaussianBinomialLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {
        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        boost::shared_ptr<QuantLib::GaussianConstantLossLM> model(new 
            QuantLib::GaussianConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::GaussianBinomialLossModel>(new 
                QuantLib::GaussianBinomialLossModel(model));
    }

    TBinomialLossModel::TBinomialLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        //! \to do Implement a type. By now only initialization traits which 
        //   can be defined by a vector can be used this way
        const std::vector<QuantLib::Real>& copulaInitVals,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {
        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        QuantLib::TCopulaPolicy::initTraits  initTT;
        for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
            initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                copulaInitVals[i]));

        boost::shared_ptr<QuantLib::TConstantLossLM> model(new 
            QuantLib::TConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::Trapezoid,
                initTT));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::TBinomialLossModel>(new 
                QuantLib::TBinomialLossModel(model));
    }

    BaseCorrelationLossModel::BaseCorrelationLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::string& baseModelId,
        const boost::shared_ptr<QuantLib::CorrelationTermStructure>& addinBC,
        const std::vector<QuantLib::Real>& recoveryRates,
        //! \to do Implement a type. By now only initialization traits which 
        //   can be defined by a vector can be used this way
        const std::vector<QuantLib::Real>& copulaInitVals,
        bool permanent)
    : DefaultLossModel(properties, permanent) {
        // eventually write a factory
        if(baseModelId == std::string("GLHP")) {
            boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> > bilin_BCTS = 
               boost::dynamic_pointer_cast<
                    QuantLib::BaseCorrelationTermStructure<
                        QuantLib::BilinearInterpolation> >(addinBC);
            if(bilin_BCTS) {
            libraryObject_ = 
                boost::shared_ptr<QuantLib::BaseCorrelationLossModel<
                    QuantLib::GaussianLHPLossModel, 
                    QuantLib::BilinearInterpolation> >(new 
                        QuantLib::BaseCorrelationLossModel<
                            QuantLib::GaussianLHPLossModel, 
                            QuantLib::BilinearInterpolation>(QuantLib::Handle<
                                QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> >(bilin_BCTS), 
                                recoveryRates));
            }else{
                QL_FAIL("Can't determine base correlation interpolation.");
            }
           
        }else if(baseModelId == std::string("GBINOMKR")){
            boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> > bilin_BCTS = 
               boost::dynamic_pointer_cast<
                    QuantLib::BaseCorrelationTermStructure<
                        QuantLib::BilinearInterpolation> >(addinBC);
            if(bilin_BCTS) {
            libraryObject_ = 
                boost::shared_ptr<QuantLib::BaseCorrelationLossModel<
                    QuantLib::GaussianBinomialLossModel, 
                    QuantLib::BilinearInterpolation> >(new 
                        QuantLib::BaseCorrelationLossModel<
                            QuantLib::GaussianBinomialLossModel, 
                            QuantLib::BilinearInterpolation>(QuantLib::Handle<
                                QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> >(bilin_BCTS), 
                                recoveryRates));
            }else{
                QL_FAIL("Can't determine base correlation interpolation.");
            }
 
        // of questionable interest, but there it goes.
        }else if(baseModelId == std::string("TBINOMKR")){
            boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<
             QuantLib::BilinearInterpolation> > bilin_BCTS = 
             boost::dynamic_pointer_cast<QuantLib::BaseCorrelationTermStructure<
                QuantLib::BilinearInterpolation> >(addinBC);
            if(bilin_BCTS) {
                QuantLib::TCopulaPolicy::initTraits  initTT;
                for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
                    initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                        copulaInitVals[i]));
            libraryObject_ = 
                boost::shared_ptr<QuantLib::BaseCorrelationLossModel<
                    QuantLib::TBinomialLossModel, 
                    QuantLib::BilinearInterpolation> >(new 
                        QuantLib::BaseCorrelationLossModel<
                            QuantLib::TBinomialLossModel, 
                            QuantLib::BilinearInterpolation>(QuantLib::Handle<
                                QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> >(bilin_BCTS), 
                                recoveryRates, initTT));
            }else{
                QL_FAIL("Can't determine base correlation interpolation.");
            }
        }else if(baseModelId == std::string("NONHOMOGKR")){
            boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> > bilin_BCTS = 
               boost::dynamic_pointer_cast<
                    QuantLib::BaseCorrelationTermStructure<
                        QuantLib::BilinearInterpolation> >(addinBC);
            if(bilin_BCTS) {
            libraryObject_ = 
                boost::shared_ptr<QuantLib::BaseCorrelationLossModel<
                    QuantLib::IHGaussPoolLossModel, 
                    QuantLib::BilinearInterpolation> >(new 
                        QuantLib::BaseCorrelationLossModel<
                            QuantLib::IHGaussPoolLossModel, 
                            QuantLib::BilinearInterpolation>(QuantLib::Handle<
                                QuantLib::BaseCorrelationTermStructure<
               QuantLib::BilinearInterpolation> >(bilin_BCTS), 
                                recoveryRates));
            }else{
                QL_FAIL("Can't determine base correlation interpolation.");
            }
        // }else if(){ADD OTHER MODELS HERE}
        }else{
            QL_FAIL("Can't determine base correlation model.");
        }    
    }

    GaussianRandomDefaultLM::GaussianRandomDefaultLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        const QuantLib::Size numSims,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        boost::shared_ptr<QuantLib::GaussianConstantLossLM> model(new 
            QuantLib::GaussianConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::GaussianRandomDefaultLM>(new 
                QuantLib::GaussianRandomDefaultLM(model, numSims));
    }

    GaussianRandomLossLM::GaussianRandomLossLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        const QuantLib::Real modelA,
        const QuantLib::Size numSims,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<2*recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        boost::shared_ptr<QuantLib::GaussianSpotLossLM> model(new 
            QuantLib::GaussianSpotLossLM(usedFactors, recoveryRates, 
                modelA, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::GaussianRandomLossLM>(new 
                QuantLib::GaussianRandomLossLM(model, numSims));
    }


    TRandomDefaultLM::TRandomDefaultLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        const std::vector<QuantLib::Real>& copulaInitVals,
        const QuantLib::Size numSims,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        QuantLib::TCopulaPolicy::initTraits  initTT;
        for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
            initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                copulaInitVals[i]));

        boost::shared_ptr<QuantLib::TConstantLossLM> model(new 
            QuantLib::TConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::Trapezoid,
                initTT));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::TRandomDefaultLM>(new 
                QuantLib::TRandomDefaultLM(model, numSims));
    }

    TRandomLossLM::TRandomLossLM(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        const std::vector<QuantLib::Real>& copulaInitVals,
        const QuantLib::Real modelA,
        const QuantLib::Size numSims,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        QuantLib::TCopulaPolicy::initTraits  initTT;
        for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
            initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                copulaInitVals[i]));

        boost::shared_ptr<QuantLib::TSpotLossLM> model(new 
            QuantLib::TSpotLossLM(usedFactors, recoveryRates, 
                modelA, 
                QuantLib::LatentModelIntegrationType::Trapezoid,
                initTT));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::TRandomLossLM>(new 
                QuantLib::TRandomLossLM(model, numSims));
    }

    SaddlePointLossModel::SaddlePointLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        boost::shared_ptr<QuantLib::GaussianConstantLossLM> model(new 
            QuantLib::GaussianConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = boost::shared_ptr<
            QuantLib::SaddlePointLossModel<QuantLib::GaussianCopulaPolicy> >(new
                QuantLib::SaddlePointLossModel<QuantLib::GaussianCopulaPolicy>(
                    model));
    }


    TSaddlePointLossModel::TSaddlePointLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        const std::vector<QuantLib::Real>& copulaInitVals,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        QuantLib::TCopulaPolicy::initTraits  initTT;
        for(QuantLib::Size i=0; i< copulaInitVals.size(); i++) 
            initTT.tOrders.push_back(static_cast<QuantLib::Integer>(
                copulaInitVals[i]));

        boost::shared_ptr<QuantLib::TConstantLossLM> model(new 
            QuantLib::TConstantLossLM(usedFactors, recoveryRates, 
            /* Incorrect integration but the saddle point search algorithm
            is having trouble with the domain given (ironically at the points
            of least contribution....), as a result the model
            will not be matching the MC simulation as it does in the gaussian
            set up.
            */
            QuantLib::LatentModelIntegrationType::GaussianQuadrature,
                //QuantLib::LatentModelIntegrationType::Trapezoid,
                initTT));

        libraryObject_ = boost::shared_ptr<
            QuantLib::SaddlePointLossModel<QuantLib::TCopulaPolicy> >(new
                QuantLib::SaddlePointLossModel<QuantLib::TCopulaPolicy>(
                    model));
    }

    RecursiveGaussLossModel::RecursiveGaussLossModel(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<std::vector<QuantLib::Real> >& factorWeights,
        const std::vector<QuantLib::Real>& recoveryRates,
        bool permanent
        )
    : DefaultLossModel(properties, permanent) {

        std::vector<std::vector<QuantLib::Real> > usedFactors;
        //This allow to have just one number without having an extra constructor
        if(factorWeights.size() == 1 && factorWeights[0].size()== 1 && 
            recoveryRates.size() > 1) {
                for(QuantLib::Size i=0; i<recoveryRates.size(); i++)
                    usedFactors.push_back(std::vector<QuantLib::Real>(1, 
                       factorWeights[0][0]));
        }else{
            usedFactors = factorWeights;
        }

        boost::shared_ptr<QuantLib::GaussianConstantLossLM> model(new 
            QuantLib::GaussianConstantLossLM(usedFactors, recoveryRates, 
                QuantLib::LatentModelIntegrationType::GaussianQuadrature));

        libraryObject_ = 
            boost::shared_ptr<QuantLib::RecursiveGaussLossModel>(new 
                QuantLib::RecursiveGaussLossModel(model));
    }



}
