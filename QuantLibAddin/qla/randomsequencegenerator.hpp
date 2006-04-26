
/*
 Copyright (C) 2006 Aurelien Chanudet

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

#ifndef qla_randomsequencegenerator_hpp
#define qla_randomsequencegenerator_hpp

#include <oh/objhandler.hpp>
#include <ql/RandomNumbers/all.hpp>
#include <vector>

namespace QuantLibAddin {

    // Common Denominator

    class RandomSequenceGenerator : public ObjHandler::Object {
      public:
        RandomSequenceGenerator(const boost::shared_ptr < InstanceName > &instanceName) : ObjHandler::Object(instanceName) {}
        std::vector<std::vector<double> > variates(long samples);

        virtual std::vector<double> nextSequence() const = 0;

        virtual boost::shared_ptr<void> getReference() const {
            return boost::shared_ptr<void>();
        }
    };

    // Pseudo Random Sequences

    template <class URNG>
    class PseudoRandomSequenceGenerator : public RandomSequenceGenerator {
      public:
        PseudoRandomSequenceGenerator(const boost::shared_ptr < InstanceName > &instanceName,long dimension, const URNG& urng)
      : RandomSequenceGenerator(instanceName), 
      ursg_(QuantLib::RandomSequenceGenerator<URNG>(dimension, urng)) { }

        virtual std::vector<double> nextSequence() const {
            QuantLib::Array sample = ursg_.nextSequence().value;

            std::vector<double> v;
            for (std::size_t j=0 ; j < sample.size() ; j++) {
                v.push_back(sample[j]);
            }

            return v;
        }

      private:
        typename QuantLib::GenericPseudoRandom<URNG, QuantLib::InverseCumulativeNormal>::ursg_type ursg_;
    };

    class MersenneTwisterRsg : public PseudoRandomSequenceGenerator<QuantLib::MersenneTwisterUniformRng> {
      public:
        typedef QuantLib::MersenneTwisterUniformRng urng_type;

        MersenneTwisterRsg(const boost::shared_ptr < InstanceName > &instanceName,long dimension, long seed);
    };

    // Low Discrepancy Sequences

    template <class URSG>
    class LowDiscrepancySequenceGenerator : public RandomSequenceGenerator {
      public:
        LowDiscrepancySequenceGenerator(const boost::shared_ptr < InstanceName > &instanceName,const URSG& ursg) 
            : RandomSequenceGenerator(instanceName), ursg_(ursg) { }

        virtual std::vector<double> nextSequence() const {
            QuantLib::Array sample = ursg_.nextSequence().value;

            std::vector<double> v;
            for (std::size_t j=0 ; j < sample.size() ; j++) {
                v.push_back(sample[j]);
            }

            return v;
        }

      private:
        typename QuantLib::GenericLowDiscrepancy<URSG, QuantLib::InverseCumulativeNormal>::ursg_type ursg_;
    };

    class FaureRsg : public LowDiscrepancySequenceGenerator<QuantLib::FaureRsg> {
      public:
        typedef QuantLib::FaureRsg rsg_type;
        FaureRsg(const boost::shared_ptr < InstanceName > &instanceName, long dimension);
    };

    class HaltonRsg : public LowDiscrepancySequenceGenerator<QuantLib::HaltonRsg> {
      public:
        typedef QuantLib::HaltonRsg rsg_type;
        HaltonRsg(const boost::shared_ptr < InstanceName > &instanceName, long dimension, long seed);
    };

    class SobolRsg : public LowDiscrepancySequenceGenerator<QuantLib::SobolRsg> {
      public:
        typedef QuantLib::SobolRsg rsg_type;
        SobolRsg(const boost::shared_ptr < InstanceName > &instanceName, long dimension, long seed);
    };

}

#endif

