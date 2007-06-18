
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

#ifndef qla_termstructuresfactory_hpp
#define qla_termstructuresfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class Calendar;
    class DayCounter;
    class RateHelper;
    class YieldTermStructure;
}

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::YieldTermStructure>(*YieldTermStructureConstructor)(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rh,
            const QuantLib::DayCounter& dayCounter);

    template<>
    class Create<boost::shared_ptr<QuantLib::YieldTermStructure> > :
        private RegistryManager<QuantLib::YieldTermStructure,
                                EnumPairRegistry> {
    public:
        boost::shared_ptr<QuantLib::YieldTermStructure> operator() (
                const std::string& traitsID,
                const std::string& interpolatorID,
                QuantLib::Natural nDays,
                const QuantLib::Calendar& calendar,
                const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rh,
                const QuantLib::DayCounter& dayCounter) {
            KeyPair key(traitsID, interpolatorID);
            YieldTermStructureConstructor yieldTermStructureConstructor =
                reinterpret_cast<YieldTermStructureConstructor>(getType(key));
            return yieldTermStructureConstructor(nDays, calendar,
                                                 rh, dayCounter);
        }
        using RegistryManager<QuantLib::YieldTermStructure,
                              EnumPairRegistry>::registerType;
    };

 }

#endif
