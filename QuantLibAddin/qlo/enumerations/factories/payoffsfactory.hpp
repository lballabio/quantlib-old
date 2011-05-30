/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qla_payoffsfactory_hpp
#define qla_payoffsfactory_hpp

#include <oh/enumerations/typefactory.hpp>
#include <ql/instruments/payoffs.hpp>

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::Payoff>(*StrikedTypePayoffConstructor1)(
        const QuantLib::Option::Type&, const double);
    typedef boost::shared_ptr<QuantLib::Payoff>(*StrikedTypePayoffConstructor2)(
        const QuantLib::Option::Type&, const double, const double);

    template<>
    class Create<boost::shared_ptr<QuantLib::Payoff> > :
        private RegistryManager<QuantLib::Payoff, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Payoff> operator()(
                const std::string& payoffID,
                const QuantLib::Option::Type& optionType,
                const double strike) {
            StrikedTypePayoffConstructor1 strikedTypePayoffConstructor =
                reinterpret_cast<StrikedTypePayoffConstructor1>(getType(payoffID));
            return strikedTypePayoffConstructor(optionType, strike);
        }
        boost::shared_ptr<QuantLib::Payoff> operator()(
                const std::string& payoffID,
                const QuantLib::Option::Type& optionType,
                const double strike,
                const double strikeIncrement) {
            StrikedTypePayoffConstructor2 strikedTypePayoffConstructor =
                reinterpret_cast<StrikedTypePayoffConstructor2>(getType(payoffID));
            return strikedTypePayoffConstructor(optionType, strike, strikeIncrement);
        }
        using RegistryManager<QuantLib::Payoff, EnumClassRegistry>::registerType;
    };
 }

#endif

