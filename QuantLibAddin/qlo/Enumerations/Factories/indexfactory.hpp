
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

#ifndef qla_indexfactory_hpp
#define qla_indexfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/indexes/ibor/eurlibor.hpp>
#include <ql/indexes/swap/euriborswapfixa.hpp>
#include <ql/indexes/swap/euriborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixa.hpp>
#include <ql/indexes/swap/eurliborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixifr.hpp>
#include <ql/indexes/swap/euriborswapfixifr.hpp>

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::Index>(*IndexConstructor)();

    template<>
    class Create<boost::shared_ptr<QuantLib::Index> > :
        private RegistryManager<QuantLib::Index, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Index> operator() (
                const std::string& euriborID) {
            IndexConstructor euriborConstructor =
                reinterpret_cast<IndexConstructor>(getType(euriborID));
            return euriborConstructor();
        }
        using RegistryManager<QuantLib::Index, EnumClassRegistry>::checkType;
        using RegistryManager<QuantLib::Index, EnumClassRegistry>::registerType;
    };

}

namespace QuantLibAddin {

    // A singleton to store the Handle<YieldTermStructure>
    // shared by all enumerated Euribor classes
    class EuriborHandle : public QuantLib::Singleton<EuriborHandle> {
        friend class QuantLib::Singleton<EuriborHandle>;
    public:
        const QuantLib::Handle<QuantLib::YieldTermStructure> &handleYieldTermStructure() const {
            return handleYieldTermStructure_;
        }
        void linkEuriborHandle(boost::shared_ptr<QuantLib::YieldTermStructure> yieldTermStructure) {
            handleYieldTermStructure_.linkTo(yieldTermStructure);
        }
    private:
        QuantLib::RelinkableHandle<QuantLib::YieldTermStructure> handleYieldTermStructure_;
    };

 }

#endif

