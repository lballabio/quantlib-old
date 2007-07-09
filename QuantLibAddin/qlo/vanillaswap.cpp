
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

#include <qlo/vanillaswap.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLibAddin {

    VanillaSwap::VanillaSwap(
            const QuantLib::VanillaSwap::Type type,
            const QuantLib::Real nominal,
            const boost::shared_ptr<QuantLib::Schedule>& fixedSchedule,
            const QuantLib::Rate fixRate,
            const QuantLib::DayCounter& fixDayCounter,
            const boost::shared_ptr<QuantLib::Schedule>& floatSchedule,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const QuantLib::Spread spread,
            const QuantLib::DayCounter& floatDayCounter,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::VanillaSwap(type,
                                      nominal,
                                      *fixedSchedule,
                                      fixRate,
                                      fixDayCounter,
                                      *floatSchedule,
                                      index,
                                      spread,
                                      floatDayCounter,
                                      hYTS));
    }

    VanillaSwap::VanillaSwap(
            const QuantLib::Period& swapTenor, 
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Rate fixedRate,
            const QuantLib::Period& forwardStart) {
        libraryObject_ = QuantLib::MakeVanillaSwap(swapTenor, index,
            fixedRate, forwardStart).operator
                boost::shared_ptr<QuantLib::VanillaSwap>();
    }

    VanillaSwap::VanillaSwap(const boost::shared_ptr<QuantLib::SwapIndex>& index,
        const QuantLib::Date& fixingDate) {

         libraryObject_ = index->underlyingSwap(fixingDate);
    
    }


    std::vector<std::vector<boost::any> > VanillaSwap::fixedLegAnalysis() {
        return Swap::legAnalysis(0);
    }

    std::vector<std::vector<boost::any> > VanillaSwap::floatingLegAnalysis() {
        return Swap::legAnalysis(1);
    }

}
