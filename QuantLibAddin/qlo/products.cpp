
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/products.hpp>
#include <qlo/evolutiondescription.hpp>
#include <qlo/ValueObjects/vo_evolutiondescription.hpp>

namespace QuantLibAddin {


    OneStepForwards::OneStepForwards(
        const std::vector<QuantLib::Time>& rateTimes,
        const std::vector<QuantLib::Real>& accruals,
        const std::vector<QuantLib::Time>& paymentTimes,
        const std::vector<QuantLib::Rate>& strikes)
    {
        QL_REQUIRE(rateTimes.size()>1, "rate times vector must contain at least two values");
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelMultiProduct>(
            new QuantLib::OneStepForwards(rateTimes, accruals,
                                          paymentTimes, strikes));
    }

    MultiStepRatchet::MultiStepRatchet(
        const std::vector<QuantLib::Time>& rateTimes,
        const std::vector<QuantLib::Real>& accruals,
        const std::vector<QuantLib::Time>& paymentTimes,
        QuantLib::Real gearingOfFloor,
        QuantLib::Real gearingOfFixing,
        QuantLib::Rate spreadOfFloor,
        QuantLib::Rate spreadOfFixing,
        QuantLib::Real initialFloor,
        bool payer)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelMultiProduct>(
            new QuantLib::MultiStepRatchet(rateTimes, accruals,
                                          paymentTimes,
                                          gearingOfFloor, gearingOfFixing,
                                          spreadOfFloor, spreadOfFixing,
                                          initialFloor, payer));
    }

    std::string MarketModelMultiProduct::evolution() const
    {
        const QuantLib::EvolutionDescription& ev = libraryObject_->evolution();
        boost::shared_ptr<ObjectHandler::Object> objectPointer(
            new QuantLibAddin::EvolutionDescription(ev));
        // Eric 27-Apr-07 - A limitation of OH redesign is that there can only be one anonymous
        // object per cell.  The line below may cause problems if creation of a second anonymous
        // object is attempted elsewhere within the same overall operation.
        std::string anonymousID =
            ObjectHandler::Repository::instance().storeObject("", objectPointer);
        objectPointer->setProperties(
            boost::shared_ptr<ObjectHandler::ValueObject>(
            new ValueObjects::qlEvolutionDescription(
                anonymousID,
                ev.rateTimes(),
                ev.evolutionTimes())));
        return anonymousID;
    }

    OneStepOptionlets::OneStepOptionlets(
            const std::vector<QuantLib::Time>& rateTimes,
            const std::vector<QuantLib::Real>& accruals,
            const std::vector<QuantLib::Time>& paymentTimes,
            const std::vector<boost::shared_ptr<QuantLib::Payoff> >& payoffs)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::MarketModelMultiProduct>(new
                QuantLib::OneStepOptionlets(
                    rateTimes, accruals, paymentTimes, payoffs));
    }

  
}
