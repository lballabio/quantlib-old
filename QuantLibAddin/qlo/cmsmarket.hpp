
/*
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_cmsmarket_hpp
#define qla_cmsmarket_hpp

#include <oh/objecthandler.hpp>
#include <ql/termstructures/swaptionvolstructure.hpp>
#include <ql/termstructures/volatilities/swaption/cmsmarket.hpp>
#include <ql/termstructure.hpp>
#include <ql/cashflows/conundrumpricer.hpp>
#include <qlo/swaptionvolstructure.hpp>


namespace QuantLibAddin {
    
    std::vector<std::vector<boost::any> > browseCmsMarket(QuantLib::Matrix & cmsMarket);
    
    class CmsMarket: public ObjectHandler::LibraryObject<QuantLib::CmsMarket>{
      public:
        CmsMarket(
            const std::vector<QuantLib::Period>& expiries,
            const std::vector< boost::shared_ptr<QuantLib::SwapIndex> >& swapIndices,
            const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& bidAskSpreads,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTermStructure,
            const std::vector< boost::shared_ptr<QuantLib::CmsCouponPricer> >& pricers);
        
        const std::vector<std::vector<boost::any> > getCmsMarket()
        {
            QuantLib::Matrix cmsMarket = libraryObject_->browse();
            return browseCmsMarket(cmsMarket);
        }

    };  

    class CmsMarketCalibration: public ObjectHandler::LibraryObject<QuantLib::CmsMarketCalibration>{
      public:
        CmsMarketCalibration(
            QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& volCube,
            boost::shared_ptr<QuantLib::CmsMarket>& cmsMarket,
            const QuantLib::Matrix& weights,
            QuantLib::CmsMarketCalibration::CalibrationType calibrationType);
        
        std::vector<std::vector<boost::any> > getSparseSabrParameters();
        std::vector<std::vector<boost::any> > getDenseSabrParameters();
        std::vector<std::vector<boost::any> > getCmsMarket();
        QuantLib::Real elapsed() {return elapsed_ ; }
        QuantLib::Array compute(const boost::shared_ptr<QuantLib::EndCriteria>& endCriteria,
                                const boost::shared_ptr<QuantLib::OptimizationMethod>& method,
                                const QuantLib::Array& guess,
                                bool isMeanReversionFixed);
      private:
        QuantLib::Real elapsed_;
    }; 
}

#endif

