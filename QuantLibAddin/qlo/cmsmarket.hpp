
/*
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_cmsmarket_hpp
#define qla_cmsmarket_hpp

#include <oh/objhandler.hpp>
#include <ql/swaptionvolstructure.hpp>
//#include <ql/Volatilities/swaptionvolcubebysabr.hpp>
#include <ql/Volatilities/cmsmarket.hpp>
#include <ql/termstructure.hpp>
#include <ql/CashFlows/conundrumpricer.hpp>


namespace QuantLibAddin {
    
    std::vector<std::vector<boost::any> > browseCmsMarket(QuantLib::Matrix & cmsMarket);
    
    class CmsMarket: public ObjHandler::LibraryObject<QuantLib::CmsMarket>{
      public:
        CmsMarket(
            const std::vector<QuantLib::Period>& expiries,
            const std::vector< boost::shared_ptr<QuantLib::SwapIndex> >& swapIndices,
            const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& bidAskSpreads,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTermStructure,
            const std::vector< boost::shared_ptr<QuantLib::CmsCouponPricer> >& pricers);
        
        const std::vector<std::vector<boost::any> > getCmsMarket()
        {
            return browseCmsMarket(cmsMarket_);
        }

        protected:
        QuantLib::Matrix cmsMarket_;
    };  

    class SmileAndCmsCalibrationBySabr: public ObjHandler::LibraryObject<QuantLib::SmileAndCmsCalibrationBySabr>{
     public:
        SmileAndCmsCalibrationBySabr(
            QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& volCube,
            boost::shared_ptr<QuantLib::CmsMarket>& cmsMarket,
            const QuantLib::Matrix& weights,
            QuantLib::SmileAndCmsCalibrationBySabr::CalibrationType calibrationType);
        
    }; 
}

#endif

