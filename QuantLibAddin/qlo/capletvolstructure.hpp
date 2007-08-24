
/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#ifndef qla_capletvolstructure_hpp
#define qla_capletvolstructure_hpp

#include <qlo/termstructures.hpp>

#include <ql/termstructures/volatilities/interestrate/caplet/capletvolatilitiesstructures.hpp>
//#include <ql/termstructures/volatilities/interestrate/caplet/genericcapletvolatilitiesstructures.hpp>
#include <ql/termstructures/volatilities/interestrate/cap/capflatvolvector.hpp>
#include <ql/termstructures/volatilities/interestrate/cap/capvolsurface.hpp>


namespace QuantLib {
    class CapletVolatilityStructure;
    class Period;
    class SmileSection;
    class IborIndex;
}

namespace QuantLibAddin {
    
    OH_OBJ_CLASS(CapletVolatilityStructure, TermStructure)

    class CapletConstantVolatility : public CapletVolatilityStructure {
      public:
      CapletConstantVolatility(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                               const QuantLib::Handle<QuantLib::Quote>& volatility,
                               const QuantLib::DayCounter& dayCounter,
                               bool permanent);
    };

    class CapsStripper : public CapletVolatilityStructure {
        public:
        CapsStripper(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                     const std::vector<QuantLib::Period>& tenors,
                     const std::vector<QuantLib::Rate>& strikes,
                     const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volatilities,
                     const boost::shared_ptr<QuantLib::IborIndex>& index,
                     const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
                     const QuantLib::DayCounter& dayCounter,
                     QuantLib::Real impliedVolatilityAccuracy,
                     QuantLib::Size maxEvaluations,
                     bool decoupledInterpolation,
                     bool permanent);

     CapsStripper(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  const std::vector<QuantLib::Period>& tenors,
                  const std::vector<QuantLib::Rate>& strikes,
                  const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volatilities,
                  const boost::shared_ptr<QuantLib::IborIndex>& index,
                  const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
                  const QuantLib::DayCounter& dayCounter,
                  QuantLib::Real impliedVolatilityAccuracy,
                  QuantLib::Size maxEvaluations,
                  const std::vector<boost::shared_ptr<QuantLib::SmileSection> >& 
                    smileSectionInterfaces,
                  bool decoupledInterpolation,
                  bool permanent);
    };

    class SmileSectionsVolStructure: public CapletVolatilityStructure {
      public:
        SmileSectionsVolStructure(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Date& referenceDate,
        const QuantLib::DayCounter& dayCounter,
        const std::vector<boost::shared_ptr<QuantLib::SmileSection> >& smileSections,
        bool permanent);
        
        };

    class SpreadedCapletVolatilityStructure : public CapletVolatilityStructure {
      public:
        SpreadedCapletVolatilityStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& underlyingVolStructure,
            const QuantLib::Handle<QuantLib::Quote>&,
            bool permanent);

    };

      class GenericCapletVolStructure : public CapletVolatilityStructure {
      public:
          GenericCapletVolStructure(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                    std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& capletVols,
                                    const std::vector<QuantLib::Rate>& strikes,
                                    const std::vector<QuantLib::Time>& tenors,
                                    bool permanent);
    };

    OH_OBJ_CLASS(CapVolatilityStructure, TermStructure)

    class CapVolatilityVector : public CapVolatilityStructure {
      public:
      CapVolatilityVector(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionTenors,
          const std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> >& volatilities,
          const QuantLib::DayCounter& dayCounter,
          bool permanent);
    };

    class CapVolatilitySurface : public CapVolatilityStructure {
      public:
      CapVolatilitySurface(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionLengths,
          const std::vector<QuantLib::Rate>& strikes,
          const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volatilities,
          bool permanent);
    };
}

#endif

