
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Katiuscia Manzoni

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

#include <qlo/swap.hpp>
#include <ql/instruments/swap.hpp>
#include <ql/instruments/makecms.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/couponpricer.hpp>

namespace QuantLibAddin {

    Swap::Swap(const std::vector<boost::shared_ptr<Leg> >& legWrappers,
               const std::vector<bool>& payer,
               const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS) {

        std::vector<QuantLib::Leg> legs(legWrappers.size());
        for (QuantLib::Size i = 0; i<legWrappers.size(); ++i)
            legs[i] = legWrappers[i]->getQuantLibLeg();
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::Swap(hYTS, legs, payer));
    }

    // QuantLib::MakeCms
    Swap::Swap(
        const QuantLib::Period& swapTenor,
        const boost::shared_ptr<QuantLib::SwapIndex>& swapIndex,
        const QuantLib::Spread iborSpread,
        const boost::shared_ptr<QuantLib::CmsCouponPricer>& pricer,
        const QuantLib::Period& forwardStart)
    {
        boost::shared_ptr<QuantLib::Swap> swap = QuantLib::MakeCms(swapTenor, swapIndex, iborSpread,
            forwardStart).operator
                boost::shared_ptr<QuantLib::Swap>();
        QuantLib::setCouponPricer(swap->leg(0), pricer);
        libraryObject_ = swap;
    }

    std::vector<std::vector<boost::any> > Swap::legAnalysis(QuantLib::Size i)
    {
        boost::shared_ptr<QuantLib::Swap> temp;
        getLibraryObject(temp);
        return flowAnalysis(temp->leg(i));
    }

}
