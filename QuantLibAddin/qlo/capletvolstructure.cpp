
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/capletvolstructure.hpp>
#include <ql/Volatilities/capletconstantvol.hpp>
#include <ql/Volatilities/capstripper.hpp>

namespace QuantLibAddin {

    CapletConstantVolatility::CapletConstantVolatility(
        const QuantLib::Handle<QuantLib::Quote>& volatility,
        const QuantLib::DayCounter& dayCounter)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapletConstantVolatility(volatility, dayCounter));
    }

    CapsStripper::CapsStripper(
         const std::vector<QuantLib::Period>& tenors,
         const std::vector<QuantLib::Rate>& strikes,
         const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& 
         volatilities,
         const boost::shared_ptr<QuantLib::IborIndex>& index,
         const QuantLib::Handle<QuantLib::YieldTermStructure> yieldTermStructure,
         const QuantLib::DayCounter& dayCounter,
         QuantLib::Real impliedVolatilityAccuracy,
         QuantLib::Size maxEvaluations,
         bool decoupledInterpolations)
    {
        std::vector<boost::shared_ptr<QuantLib::SmileSection> > dummySmileSections;
        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > temp(volatilities.size());
        QuantLib::Size nbColumns  = volatilities.front().size();
        for(QuantLib::Size i = 0; i<temp.size(); ++i){
            temp[i].resize(nbColumns);
            for (QuantLib::Size j = 0; j<nbColumns; ++j)
                temp[i][j]=  volatilities[i][j];
        }
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapsStripper(tenors, strikes, temp, 
            index, yieldTermStructure, dayCounter, impliedVolatilityAccuracy, 
                maxEvaluations, dummySmileSections, true, 
                decoupledInterpolations));
    }

    
 CapsStripper::CapsStripper(
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
      bool decoupledInterpolation)
    {
        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > temp(volatilities.size());
        QuantLib::Size nbColumns  = volatilities.front().size();
        for(QuantLib::Size i = 0; i<temp.size(); ++i){
            temp[i].resize(nbColumns);
            for (QuantLib::Size j = 0; j<nbColumns; ++j)
                temp[i][j]=  volatilities[i][j];
        }
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapsStripper(tenors, strikes, temp, 
            index, yieldTermStructure, dayCounter, impliedVolatilityAccuracy, 
            maxEvaluations, smileSectionInterfaces, decoupledInterpolation));
    }

    SmileSectionsVolStructure::SmileSectionsVolStructure(
        const QuantLib::Date& referenceDate,
        const QuantLib::DayCounter& dayCounter,
        const QuantLib::SmileSectionInterfaceVector& smileSections){
        libraryObject_ = 
            boost::shared_ptr<QuantLib::SmileSectionsVolStructure>(new
            QuantLib::SmileSectionsVolStructure(referenceDate,
            dayCounter, smileSections));
   }

}
