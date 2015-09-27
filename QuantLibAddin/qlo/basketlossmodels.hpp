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

#ifndef qla_basketlossmodels_hpp
#define qla_basketlossmodels_hpp

#include <ql/types.hpp>

#include <qlo/baseinstruments.hpp>

namespace QuantLib {
    class DefaultLossModel;
    class CorrelationTermStructure;

    template <class T>
    class Handle;
}


namespace QuantLibAddin {

    /* Default Loss Models */
    OH_LIB_CLASS(DefaultLossModel, QuantLib::DefaultLossModel);

    class GaussianLHPLossModel : public DefaultLossModel {
    public:
        GaussianLHPLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Real correl,
            const std::vector<QuantLib::Real>& recoveryRates,
            bool permanent
            );
    };

    class IHGaussPoolLossModel : public DefaultLossModel {
    public:
        IHGaussPoolLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Real correlation,
            const std::vector<QuantLib::Real>& recoveryRates,
            const QuantLib::Size numBuckets,
            bool permanent
            );
    };

    class IHStudentPoolLossModel : public DefaultLossModel {
    public:
        IHStudentPoolLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Real correlation,
            const std::vector<QuantLib::Real>& recoveryRates,
            //! \to do Implement a type. By now only initialization traits which
            //  can be defined by a vector can be used this way
            const std::vector<QuantLib::Real>& copulaInitVals,
            const QuantLib::Size numBuckets,
            bool permanent
            );
    };

    class GaussianBinomialLossModel : public DefaultLossModel {
    public:
        GaussianBinomialLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            bool permanent
            );
    };

    class TBinomialLossModel : public DefaultLossModel {
    public:
        TBinomialLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            //! \to do Implement a type. By now only initialization traits which
            //  can be defined by a vector can be used this way
            const std::vector<QuantLib::Real>& copulaInitVals,
            bool permanent
            );
    };

    class BaseCorrelationLossModel : public DefaultLossModel {
    public:
        BaseCorrelationLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            
            const std::string& baseModelId,
            const boost::shared_ptr<QuantLib::CorrelationTermStructure>& addinBC,
            const std::vector<QuantLib::Real>& recoveryRates,
            //! \to do Implement a type. By now only initialization traits which
            //  can be defined by a vector can be used this way
            const std::vector<QuantLib::Real>& copulaInitVals,

            bool permanent
            );
    };

    class GaussianRandomDefaultLM : public DefaultLossModel {
    public:
        GaussianRandomDefaultLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            const QuantLib::Size numSims,
            bool permanent
            );
    };

    class GaussianRandomLossLM : public DefaultLossModel {
    public:
        GaussianRandomLossLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            const QuantLib::Real modelA,
            const QuantLib::Size numSims,
            bool permanent
            );
    };

    class TRandomDefaultLM : public DefaultLossModel {
    public:
        TRandomDefaultLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            const std::vector<QuantLib::Real>& copulaInitVals,
            const QuantLib::Size numSims,
            bool permanent
            );
    };

    class TRandomLossLM : public DefaultLossModel {
    public:
        TRandomLossLM(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            const std::vector<QuantLib::Real>& copulaInitVals,
            const QuantLib::Real modelA,
            const QuantLib::Size numSims,
            bool permanent
            );
    };

    class SaddlePointLossModel : public DefaultLossModel {
    public:
        SaddlePointLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            bool permanent
            );
    };

    class TSaddlePointLossModel : public DefaultLossModel {
    public:
        TSaddlePointLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            const std::vector<QuantLib::Real>& copulaInitVals,
            bool permanent
            );
    };

    class RecursiveGaussLossModel : public DefaultLossModel {
    public:
        RecursiveGaussLossModel(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<std::vector<QuantLib::Real> >& factorWeights,
            const std::vector<QuantLib::Real>& recoveryRates,
            bool permanent
            );
    };

}

#endif
