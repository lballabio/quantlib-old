
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/cmsmarket.hpp>

using std::vector;
using boost::shared_ptr;
using boost::any;
using QuantLib::Size;
using QuantLib::CmsCouponPricer;
using QuantLib::ConundrumPricer;
using QuantLib::Matrix;
using QuantLib::Handle;
using QuantLib::Quote;

namespace QuantLibAddin {

    CmsMarket::CmsMarket(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        const vector<QuantLib::Period>& expiries,
        const vector<shared_ptr<QuantLib::SwapIndex> >& swapIndices,
        const shared_ptr<QuantLib::IborIndex>& iborIndex,
        const vector<vector<Handle<Quote> > >& bidAskSpreads,
        const vector<shared_ptr<CmsCouponPricer> >& pricers,
        const Handle<QuantLib::YieldTermStructure>& discountingTS,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::CmsMarket>(properties, permanent)
    {
        Size n = pricers.size();
        vector<shared_ptr<ConundrumPricer> > p(n);
        for (Size i=0; i<n; ++i) {
            p[i] = boost::dynamic_pointer_cast<ConundrumPricer>(pricers[i]);
            QL_REQUIRE(p[i],
                       "ConundrumPricer needed, not just a CmsCouponPricer");
        }
        libraryObject_ = shared_ptr<QuantLib::CmsMarket>(new
            QuantLib::CmsMarket(expiries,
                                swapIndices,
                                iborIndex,
                                bidAskSpreads,
                                p,
                                discountingTS));
    }

    vector<vector<any> > CmsMarket::getCmsMarket()
    {
        Matrix cmsMarket = libraryObject_->browse();
        return browseCmsMarket(cmsMarket);
    }


    vector<vector<any> > browseCmsMarket(const Matrix& cmsMarket) {
        Size numberOfColumn = 14;
        Size numberOfRows = cmsMarket.rows()+1;
        vector<vector<any> > result(numberOfRows, vector<any>(numberOfColumn));

        result[0][ 0] = std::string("CM Swap Index");
        result[0][ 1] = std::string("Maturity");

        result[0][ 2] = std::string("Mkt Bid - Spread (bps)");
        result[0][ 3] = std::string("Mkt Ask - Spread (bps)");
        result[0][ 4] = std::string("Mkt Mid - Spread (bps)");
        result[0][ 5] = std::string("Model Mid - Spread (bps)");
        result[0][ 6] = std::string("Error - Spread (bps)");
        result[0][ 7] = std::string("Outside bid/ask (bps)");
        
        result[0][ 8] = std::string("Mkt mid - Spot price");
        result[0][ 9] = std::string("Model mid - Spot price");
        result[0][10] = std::string("Error - Spot price");  
        
        result[0][11] = std::string("Mkt mid - Fwd price");
        result[0][12] = std::string("Model mid - Fwd price");
        result[0][13] = std::string("Error - Fwd price"); 
        
        for (Size i=0; i<cmsMarket.rows(); ++i)
            for(Size j=0; j<cmsMarket.columns(); ++j)
               result[i+1][j] = cmsMarket[i][j];

        return result;
    }
}
