
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers

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

#include <qlo/payoffs.hpp>
#include <qlo/Enumerations/Factories/payoffsfactory.hpp>

namespace QuantLibAddin {

    StrikedTypePayoff::StrikedTypePayoff(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& payoffID,
            const QuantLib::Option::Type& optionType,
            const QuantLib::Real strike,
            const QuantLib::Real thirdParameter,
            bool permanent) : TypePayoff(properties, permanent) {
        if (thirdParameter==QuantLib::Null<QuantLib::Real>())
            libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::Payoff> >()(
                payoffID, optionType, strike);
        else
            libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::Payoff> >()(
                payoffID, optionType, strike, thirdParameter);
    }

    QuantLib::Real StrikedTypePayoff::thirdParameter() const {
        return 0.0;
    }
}

