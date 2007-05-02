
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_indexfactory_hpp
#define qla_indexfactory_hpp

#include <qlo/typefactory.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/indexes/ibor/eurlibor.hpp>
#include <ql/indexes/swap/euriborswapfixa.hpp>
#include <ql/indexes/swap/euriborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixa.hpp>
#include <ql/indexes/swap/eurliborswapfixb.hpp>
#include <ql/indexes/swap/eurliborswapfixifr.hpp>
#include <ql/indexes/swap/euriborswapfixifr.hpp>

namespace QuantLibAddin {

    ///* *** Index *** */
    typedef boost::shared_ptr<QuantLib::Index>(*IndexConstructor)();

    template<>
    class Create<boost::shared_ptr<QuantLib::Index> > :
        private RegistryManager<QuantLib::Index, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::Index> operator() (
                const std::string& euriborID) {
            IndexConstructor euriborConstructor = 
                (IndexConstructor)(getType(euriborID));
            return euriborConstructor();
        }
        using RegistryManager<QuantLib::Index, EnumClassRegistry>::checkType;
    };

 }

#endif

