
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

#ifndef qla_capletvolstructure_hpp
#define qla_capletvolstructure_hpp

#include <oh/objhandler.hpp>
#include <ql/Volatilities/capletvolatilitiesstructures.hpp>
#include <ql/capvolstructures.hpp>
#include <qlo/termstructures.hpp>
#include <ql/Volatilities/smilesection.hpp>

namespace QuantLibAddin {
    
    class CapletVolatilityStructure : public TermStructure {};

    class CapletConstantVolatility : public CapletVolatilityStructure {
      public:
      CapletConstantVolatility(const QuantLib::Handle<QuantLib::Quote>& volatility,
                               const QuantLib::DayCounter& dayCounter);
    };

    class CapsStripper : public CapletVolatilityStructure {
        public:
        CapsStripper(const std::vector<QuantLib::Period>& tenors,
                     const std::vector<QuantLib::Rate>& strikes,
                     const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volatilities,
                     const boost::shared_ptr<QuantLib::IborIndex>& index,
                     const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
                     const QuantLib::DayCounter& dayCounter,
                     QuantLib::Real impliedVolatilityAccuracy,
                     QuantLib::Size maxEvaluations,
                     bool decoupledInterpolation);

     CapsStripper(const std::vector<QuantLib::Period>& tenors,
                  const std::vector<QuantLib::Rate>& strikes,
                  const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volatilities,
                  const boost::shared_ptr<QuantLib::IborIndex>& index,
                  const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
                  const QuantLib::DayCounter& dayCounter,
                  QuantLib::Real impliedVolatilityAccuracy,
                  QuantLib::Size maxEvaluations,
                  const std::vector<boost::shared_ptr<QuantLib::SmileSection> >& 
                    smileSectionInterfaces,
                  bool decoupledInterpolation);
    };

    class SmileSectionsVolStructure: public CapletVolatilityStructure {
      public:
        SmileSectionsVolStructure(
        const QuantLib::Date& referenceDate,
        const QuantLib::DayCounter& dayCounter,
        const QuantLib::SmileSectionInterfaceVector& smileSections);
        
        };

}

#endif
