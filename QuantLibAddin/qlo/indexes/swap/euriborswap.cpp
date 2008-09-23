/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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

#include <qlo/indexes/swap/euriborswap.hpp>

#include <ql/indexes/swap/euriborswap.hpp>

namespace QuantLibAddin {

    EuriborSwap::EuriborSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            SwapIndex::FixingType fixingType,
            const QuantLib::Period& p,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& h,
            bool permanent) : SwapIndex(properties, permanent)
    {
        switch (fixingType) {
          case IsdaFixA:
            libraryObject_ = boost::shared_ptr<QuantLib::EuriborSwapIsdaFixA>(new
                QuantLib::EuriborSwapIsdaFixA(p, h));
            break;
          case IsdaFixB:
            libraryObject_ = boost::shared_ptr<QuantLib::EuriborSwapIsdaFixB>(new
                QuantLib::EuriborSwapIsdaFixB(p, h));
            break;
          case IfrFix:
            libraryObject_ = boost::shared_ptr<QuantLib::EuriborSwapIfrFix>(new
                QuantLib::EuriborSwapIfrFix(p, h));
            break;
          default:
              QL_FAIL("Euribor Swap " << fixingType << " is not defined");
        }
    }

}
