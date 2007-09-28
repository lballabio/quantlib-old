/*
 Copyright (C) 2006 François du Vignaud

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

#ifndef qla_smilesection_hpp
#define qla_smilesection_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class Date;
    class SmileSection;
    class DayCounter;
    class EndCriteria;
    class Quote;
    class OptimizationMethod;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {
    
    OH_LIB_CLASS(SmileSection, QuantLib::SmileSection);

    class FlatSmileSection : public SmileSection {
      public:
        FlatSmileSection(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                         const QuantLib::Date& optionDate,
                         QuantLib::Volatility v,
                         const QuantLib::DayCounter& dc,
                         const QuantLib::Date& refDate,
                         QuantLib::Real atmValue,
                         bool permanent);
    };

    class InterpolatedSmileSection : public SmileSection {
      public:
        InterpolatedSmileSection(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Date& optionDate,
              const std::vector<QuantLib::Rate>& strikes,
              const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
              const QuantLib::Handle<QuantLib::Quote>& atmLevel,
              const QuantLib::DayCounter& dc,
              bool permanent);
    };

    class SabrSmileSection: public SmileSection{
      public:
        SabrSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Time expiry,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            const QuantLib::Handle<QuantLib::Quote>& forward,
            QuantLib::Real alpha, 
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted = false,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria
                = boost::shared_ptr<QuantLib::EndCriteria>(),
            const boost::shared_ptr<QuantLib::OptimizationMethod> method
                = boost::shared_ptr<QuantLib::OptimizationMethod>(),
            bool permanent = false);
        
    };

    class SabrInterpolatedSmileSection: public SmileSection {
      public:
        SabrInterpolatedSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& optionDate,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            QuantLib::Handle<QuantLib::Quote> forward, 
            QuantLib::Real alpha, 
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method,
            const QuantLib::DayCounter& dc,
            bool permanent);
    };
       
    class SabrInterpolatedSmileSection2: public SabrInterpolatedSmileSection {
      public:
        SabrInterpolatedSmileSection2(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& optionDate,
            const std::vector<QuantLib::Rate>& strikeSpreads,
            QuantLib::Handle<QuantLib::Quote> atmVolatility,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& volSpreads,
            QuantLib::Handle<QuantLib::Quote> forward, 
            QuantLib::Real alpha, 
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method,
            const QuantLib::DayCounter& dc,
            bool permanent);
    };
}

#endif

