
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/cmsmarket.hpp>
#include <qlo/swaptionvolstructure.hpp>
#include <ql/termstructures/volatility/interestrate/swaption/cmsmarket.hpp>

namespace QuantLibAddin {

    CmsMarket::CmsMarket(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Period>& expiries,
        const std::vector< boost::shared_ptr<QuantLib::SwapIndex> >& swapIndices,
        const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& bidAskSpreads,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTermStructure,
        const std::vector< boost::shared_ptr<QuantLib::CmsCouponPricer> >& pricers,
        bool permanent) : ObjectHandler::LibraryObject<QuantLib::CmsMarket>(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::CmsMarket>(new
            QuantLib::CmsMarket(expiries,
                                swapIndices,
                                bidAskSpreads,
                                pricers,
                                yieldTermStructure));
    }

    const std::vector<std::vector<boost::any> > CmsMarket::getCmsMarket()
    {
        QuantLib::Matrix cmsMarket = libraryObject_->browse();
        return browseCmsMarket(cmsMarket);
    }


    std::vector<std::vector<boost::any> > browseCmsMarket(QuantLib::Matrix & cmsMarket){
        std::vector<std::vector<boost::any> > result;
        QuantLib::Size numberOfColumn = 19;

        std::vector<boost::any> headings(numberOfColumn);
        headings[0]=std::string("CM Swap Index");
        headings[1]=std::string("Maturity");

        headings[2]=std::string("Mkt Bid - Spread (bps)");
        headings[3]=std::string("Mkt Ask - Spread (bps)");
        headings[4]=std::string("Mkt Mid - Spread (bps)");
        headings[5]=std::string("Model Mid - Spread (bps)");
        headings[6]=std::string("Error - Spread (bps)");
        headings[7]=std::string("Outside bid/ask (bps)");
        
        headings[8]=std::string("Mkt bid - Spot price");
        headings[9]=std::string("Mkt ask - Spot price");
        headings[10]=std::string("Mkt mid - Spot price");
        headings[11]=std::string("Model mid - Spot price");
        headings[12]=std::string("Error - Spot price");  
        
        headings[13]=std::string("Mkt bid - Fwd price");
        headings[14]=std::string("Mkt ask - Fwd price");
        headings[15]=std::string("Mkt mid - Fwd price");
        headings[16]=std::string("Model mid - Fwd price");
        headings[17]=std::string("Error - Fwd price"); 
        
        headings[18]=std::string("Mean rev.");

        result.push_back(headings);

        for(QuantLib::Size i=0; i<cmsMarket.rows(); ++i)
        {
            std::vector<boost::any> row(numberOfColumn, std::string("N/A"));
            for(QuantLib::Size j=0; j<cmsMarket.columns(); ++j)
            {
               row[j] = cmsMarket[i][j];
            }
            result.push_back(row);
        }
        return result;
    }
}

