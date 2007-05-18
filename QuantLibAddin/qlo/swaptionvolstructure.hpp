
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Silvia Frasson
 Copyright (C) 2006 Mario Pucci
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#ifndef qla_swaptionvolstructure_hpp
#define qla_swaptionvolstructure_hpp

#include <oh/objecthandler.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/smilesection.hpp>

#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/volatilities/swaptionvolcube.hpp>
#include <ql/termstructures/volatilities/spreadedswaptionvolstructure.hpp>

namespace QuantLib {
    class QuantLib::EndCriteria;
}

namespace QuantLibAddin {

    class SwaptionVolatilityStructure : public TermStructure {};

    class SwaptionConstantVolatility : public SwaptionVolatilityStructure {
      public:
        SwaptionConstantVolatility(const QuantLib::Date& referenceDate,
                                   const QuantLib::Handle<QuantLib::Quote>&,
                                   const QuantLib::DayCounter& dayCounter);
    };

    class SwaptionVolatilityDiscrete : public SwaptionVolatilityStructure {};

    class SwaptionVolatilityMatrix : public SwaptionVolatilityDiscrete {
      public:
        SwaptionVolatilityMatrix(const QuantLib::Calendar& calendar,
                                 const std::vector<QuantLib::Period>& optionTenors,
                                 const std::vector<QuantLib::Period>& tenors,
                                 const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& vols,
                                 const QuantLib::DayCounter& dayCounter,
                                 const QuantLib::BusinessDayConvention bdc);
        std::vector<long> locate(const QuantLib::Date& d,
                                 const QuantLib::Period& p);
    };
    

    class SwaptionVolatilityCube : public SwaptionVolatilityDiscrete {
    };
    
    class SwaptionVolCube2 : public SwaptionVolatilityCube {
    public:
        SwaptionVolCube2(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& atmVol,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::Period>& swapTenors,
            const std::vector<QuantLib::Spread>& strikeSpreads,
            const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volSpreads,
            const boost::shared_ptr<QuantLib::SwapIndex>& swapIndexBase,
            bool vegaWeightedSmileFit);
    };
    
    // FIXME Clients of these functions pass in a temporary object as input which can
    // result in a crash if the param is declared as a reference so change to pass-by-value
    // instead.  Please confirm this change is OK then delete these comments :-)
    //std::vector<std::vector<boost::any> > getSabrParameters(QuantLib::Matrix & sabrParameters);
    std::vector<std::vector<boost::any> > getSabrParameters(QuantLib::Matrix sabrParameters);
    //std::vector<std::vector<boost::any> > getVolCube(QuantLib::Matrix & volCube);
    std::vector<std::vector<boost::any> > getVolCube(QuantLib::Matrix volCube);
    
    class SwaptionVolCube1 : public SwaptionVolatilityCube {
      public:
        SwaptionVolCube1(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& atmVol,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::Period>& swapTenors,
            const std::vector<QuantLib::Spread>& strikeSpreads,
            const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& volSpreads,
            const boost::shared_ptr<QuantLib::SwapIndex>& swapIndexBase,
            bool vegaWeightedSmileFit,
            const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& parametersGuess,
            const std::vector<bool>& isParameterFixed,
            bool isAtmCalibrated,
            const boost::shared_ptr<QuantLib::EndCriteria>& endCriteria,
            QuantLib::Real maxErrorTolerance,
            const boost::shared_ptr<QuantLib::OptimizationMethod>& optMethod);
        std::vector<std::vector<boost::any> > getSparseSabrParameters();
        std::vector<std::vector<boost::any> > getDenseSabrParameters();
        std::vector<std::vector<boost::any> > getMarketVolCube();
        std::vector<std::vector<boost::any> > getVolCubeAtmCalibrated();
    };

    class SmileSectionByCube : public SmileSection {
      public:
        SmileSectionByCube(
            const boost::shared_ptr<QuantLib::SwaptionVolatilityCube>& cube,
            const QuantLib::Period& optionTenors,
            const QuantLib::Period& swapTenors);
        SmileSectionByCube(
            const boost::shared_ptr<QuantLib::SwaptionVolatilityCube>& cube,
            const QuantLib::Date& optionDate,
            const QuantLib::Period& swapTenors);
    };
    
    class SpreadedSwaptionVolatilityStructure : public SwaptionVolatilityStructure {
      public:
        SpreadedSwaptionVolatilityStructure(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& underlyingVolStructure,
            QuantLib::Spread spread);

    };
        
}

#endif
