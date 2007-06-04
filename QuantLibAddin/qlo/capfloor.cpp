
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif

#include <qlo/capfloor.hpp>
#include <qlo/flowanalysis.hpp>

#include <ql/instruments/makecapfloor.hpp>

namespace QuantLibAddin {

    CapFloor::CapFloor(
                 QuantLib::CapFloor::Type type,
                 const boost::shared_ptr<Leg>& floatingLeg,
                 const std::vector<QuantLib::Rate>& strikes,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& termStructure,
                 const boost::shared_ptr<QuantLib::PricingEngine>& engine)
    {
        const QuantLib::Leg& leg = floatingLeg->getQuantLibLeg();
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::CapFloor(type, leg, strikes, termStructure, engine));
    }

    CapFloor::CapFloor(QuantLib::CapFloor::Type capFloorType,
                       const QuantLib::Period& capFloorTenor, 
                       const boost::shared_ptr<QuantLib::IborIndex>& index,
                       QuantLib::Rate strike,
                       const QuantLib::Period& forwardStart,
                 const boost::shared_ptr<QuantLib::PricingEngine>& engine) {
        libraryObject_ = QuantLib::MakeCapFloor(capFloorType, capFloorTenor,
            index, strike, forwardStart, engine).operator
                boost::shared_ptr<QuantLib::CapFloor>();
    }

    std::vector<std::vector<boost::any> > CapFloor::legAnalysis()
    {
        boost::shared_ptr<QuantLib::CapFloor> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->floatingLeg();
        return flowAnalysis(cashflows);
    }

}
