/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008, 2009 Ferdinando Ametrano
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

#include <ql/termstructures/yieldtermstructure.hpp>
#include <qlo/indexes/swap/liborswap.hpp>

#include <ql/indexes/swap/chfliborswap.hpp>
#include <ql/indexes/swap/eurliborswap.hpp>
#include <ql/indexes/swap/gbpliborswap.hpp>
#include <ql/indexes/swap/jpyliborswap.hpp>
#include <ql/indexes/swap/usdliborswap.hpp>

namespace QuantLibAddin {

    LiborSwap::LiborSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Currency& currency,
            SwapIndex::FixingType fixingType,
            const QuantLib::Period& p,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& f,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& d,
            bool permanent) : SwapIndex(properties, permanent)
    {
        switch (currency.numericCode()) {
          case 978: // EUR
            switch (fixingType) {
              case IsdaFixA:
                libraryObject_ = boost::shared_ptr<QuantLib::EurLiborSwapIsdaFixA>(new
                    QuantLib::EurLiborSwapIsdaFixA(p, f, d));
                break;
              case IsdaFixB:
                libraryObject_ = boost::shared_ptr<QuantLib::EurLiborSwapIsdaFixB>(new
                    QuantLib::EurLiborSwapIsdaFixB(p, f, d));
                break;
              case IfrFix:
                libraryObject_ = boost::shared_ptr<QuantLib::EurLiborSwapIfrFix>(new
                    QuantLib::EurLiborSwapIfrFix(p, f, d));
                break;
              default:
                  QL_FAIL(currency << " Libor Swap " << fixingType << " is not defined");
            }
            break;
          case 840: // USD
            switch (fixingType) {
              case IsdaFixAm:
                libraryObject_ = boost::shared_ptr<QuantLib::UsdLiborSwapIsdaFixAm>(new
                    QuantLib::UsdLiborSwapIsdaFixAm(p, f));
                break;
              case IsdaFixPm:
                libraryObject_ = boost::shared_ptr<QuantLib::UsdLiborSwapIsdaFixPm>(new
                    QuantLib::UsdLiborSwapIsdaFixPm(p, f));
                break;
              default:
                  QL_FAIL(currency << " Libor Swap " << fixingType << " is not defined");
            }
            break;
          case 826: // GBP
            switch (fixingType) {
              case Isda:
                libraryObject_ = boost::shared_ptr<QuantLib::GbpLiborSwapIsdaFix>(new
                    QuantLib::GbpLiborSwapIsdaFix(p, f));
                break;
              default:
                  QL_FAIL(currency << " Libor Swap " << fixingType << " is not defined");
            }
            break;
          case 756: // CHF
            switch (fixingType) {
              case Isda:
                libraryObject_ = boost::shared_ptr<QuantLib::ChfLiborSwapIsdaFix>(new
                    QuantLib::ChfLiborSwapIsdaFix(p, f));
                break;
              default:
                  QL_FAIL(currency << " Libor Swap " << fixingType << " is not defined");
            }
            break;
          case 392: // JPY
            switch (fixingType) {
              case IsdaFixAm:
                libraryObject_ = boost::shared_ptr<QuantLib::JpyLiborSwapIsdaFixAm>(new
                    QuantLib::JpyLiborSwapIsdaFixAm(p, f));
                break;
              case IsdaFixPm:
                libraryObject_ = boost::shared_ptr<QuantLib::JpyLiborSwapIsdaFixPm>(new
                    QuantLib::JpyLiborSwapIsdaFixPm(p, f));
                break;
              default:
                  QL_FAIL(currency << " Libor Swap " << fixingType << " is not defined");
            }
            break;
          default:
              QL_FAIL("Unhandled currency " << currency);
        }
    }

}
