
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007 Ferdinando Ametrano
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
#include <qlo/flowanalysis.hpp>

#include <ql/instruments/swap.hpp>
#include <ql/instruments/makecms.hpp>
#include <ql/cashflows/couponpricer.hpp>

using boost::shared_ptr;

namespace QuantLibAddin {

    Swap::Swap(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
               const std::vector<boost::shared_ptr<Leg> >& legWrappers,
               const std::vector<bool>& payer,
               bool permanent) : Instrument(properties, permanent) {

        std::vector<QuantLib::Leg> legs(legWrappers.size());
        for (QuantLib::Size i = 0; i<legWrappers.size(); ++i)
            legs[i] = legWrappers[i]->getQuantLibLeg();
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::Swap(legs, payer));
    }

    Swap::Swap(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Period& swapTenor,
        const boost::shared_ptr<QuantLib::SwapIndex>& swapIndex,
        const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
        QuantLib::Spread iborSpread,
        const QuantLib::Period& forwardStart,
        const boost::shared_ptr<QuantLib::CmsCouponPricer>& pricer,
        bool permanent)
    : Instrument(properties, permanent)
    {
        libraryObject_ = QuantLib::MakeCms(swapTenor, swapIndex,
                                           iborIndex, iborSpread,
                                           forwardStart)
                         .withCmsCouponPricer(pricer)
                         .operator boost::shared_ptr<QuantLib::Swap>();
    }

    std::vector<std::vector<ObjectHandler::property_t> > Swap::legAnalysis(QuantLib::Size i)
    {
        boost::shared_ptr<QuantLib::Swap> temp;
        getLibraryObject(temp);
        return flowAnalysis(temp->leg(i));
    }

}
