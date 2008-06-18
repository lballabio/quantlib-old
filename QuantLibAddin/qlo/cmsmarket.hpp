
/*
 Copyright (C) 2007 Ferdinando Ametrano
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

#include <oh/libraryobject.hpp>
#include <oh/property.hpp>
#include <ql/termstructures/volatility/swaption/cmsmarket.hpp>

namespace QuantLibAddin {
    
    std::vector<std::vector<ObjectHandler::property_t> > browseCmsMarket(
        const QuantLib::Matrix& cmsMarket);
    
    class CmsMarket: public ObjectHandler::LibraryObject<QuantLib::CmsMarket>{
      public:
        CmsMarket(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Period>& expiries,
            const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >& swapIndices,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& bidAskSpreads,
            const std::vector<boost::shared_ptr<QuantLib::CmsCouponPricer> >& pricers,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discountingTS,
            bool permanent);
        
        std::vector<std::vector<ObjectHandler::property_t> > getCmsMarket();

    };  

}

#endif
