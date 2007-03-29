
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/cmsmarket.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/termstructures.hpp>


namespace QuantLibAddin {

   CmsMarket::CmsMarket(
        const std::vector<QuantLib::Period>& expiries,
        const std::vector< boost::shared_ptr<QuantLib::SwapIndex> >& swapIndices,
        const std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > >& bidAskSpreads,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTermStructure,
        const std::vector< boost::shared_ptr<QuantLib::CmsCouponPricer> >& pricers) {

        std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > temp(bidAskSpreads.size());
        QuantLib::Size nbColumns  = bidAskSpreads.front().size();
        for(QuantLib::Size i = 0; i<temp.size(); ++i){
            temp[i].resize(nbColumns);
            for (QuantLib::Size j = 0; j<nbColumns; ++j)
                temp[i][j]=  bidAskSpreads[i][j];
        }
        
        libraryObject_ = boost::shared_ptr<QuantLib::CmsMarket>(
            new QuantLib::CmsMarket(
                            expiries,
                            swapIndices,
                            temp,
                            pricers,
                            yieldTermStructure));
    }

    std::vector<std::vector<boost::any> > browseCmsMarket(QuantLib::Matrix & cmsMarket){
        std::vector<std::vector<boost::any> > result;
        QuantLib::Size numberOfColumn = 19;

        std::vector<boost::any> headings(numberOfColumn);
        headings[0]=std::string("CM Swap Index");
        headings[1]=std::string("Exp");

        headings[2]=std::string("Mkt Bid (bps)");
        headings[3]=std::string("Mkt Ask (bps)");
        headings[4]=std::string("Mkt Mid (bps)");
        headings[5]=std::string("Model Mid (bps)");
        headings[6]=std::string("Error (bps)");
        headings[7]=std::string("Outside bid/ask");
        
        headings[8]=std::string("Mkt bid - spot price");
        headings[9]=std::string("Mkt ask - spot price");
        headings[10]=std::string("Mkt mid - spot price");
        headings[11]=std::string("Model mid - spot price");
        headings[12]=std::string("Spot price error");  
        
        headings[13]=std::string("Mkt bid - fwd price");
        headings[14]=std::string("Mkt ask - fwd price");
        headings[15]=std::string("Mkt mid - fwd price");
        headings[16]=std::string("Model mid - fwd price");
        headings[17]=std::string("Fwd price error"); 
        
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

    
    SmileAndCmsCalibrationBySabr::SmileAndCmsCalibrationBySabr(
        QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& volCube,
        boost::shared_ptr<QuantLib::CmsMarket>& cmsMarket,
        const QuantLib::Matrix& weights,
        QuantLib::SmileAndCmsCalibrationBySabr::CalibrationType calibrationType){
        
        libraryObject_ = boost::shared_ptr<QuantLib::SmileAndCmsCalibrationBySabr>(
        new QuantLib::SmileAndCmsCalibrationBySabr(
                        volCube,
                        cmsMarket,
                        weights,
                        calibrationType));         
     }
        
    std::vector<std::vector<boost::any> >
    SmileAndCmsCalibrationBySabr::getSparseSabrParameters() {
        return getSabrParameters(libraryObject_->sparseSabrParameters_);
    }

    std::vector<std::vector<boost::any> >
    SmileAndCmsCalibrationBySabr::getDenseSabrParameters() {
        return getSabrParameters(libraryObject_->denseSabrParameters_);
    }

    std::vector<std::vector<boost::any> >
    SmileAndCmsCalibrationBySabr::getCmsMarket() {
       return browseCmsMarket(libraryObject_->browseCmsMarket_);
    }
}
