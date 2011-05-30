/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2010 Ferdinando Ametrano

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

#ifndef qla_btp_hpp
#define qla_btp_hpp

#include <qlo/bonds.hpp>
#include <qlo/quote.hpp>

namespace QuantLib {
    class RendistatoBasket;
    class RendistatoCalculator;
    class BTP;
    template <class T>
    class Handle;
    class Quote;
    class Euribor;
}

namespace QuantLibAddin {

    class BTP : public FixedRateBond {
      public:
        BTP(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            const QuantLib::Date& maturityDate,
            QuantLib::Rate fixedRate,
            QuantLib::Real redemption,
            const QuantLib::Date& startDate,
            const QuantLib::Date& issueDate,
            bool permanent);
    };

    class RendistatoBasket : public ObjectHandler::LibraryObject<QuantLib::RendistatoBasket> {
      public:
        RendistatoBasket(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<boost::shared_ptr<QuantLib::BTP> >& btps,
            const std::vector<QuantLib::Real>& outstandings,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& cleanPriceQuotes,
            bool permanent);
    };

    class RendistatoCalculator : public ObjectHandler::LibraryObject<QuantLib::RendistatoCalculator> {
      public:
        RendistatoCalculator(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::RendistatoBasket>& basket,
            const boost::shared_ptr<QuantLib::Euribor>& euriborIndex,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discountCurve,
            bool permanent);
    };

    class RendistatoEquivalentSwapLengthQuote : public Quote {
      public:
        RendistatoEquivalentSwapLengthQuote(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::RendistatoCalculator>& r,
            bool permanent);
    };

    class RendistatoEquivalentSwapSpreadQuote : public Quote {
      public:
        RendistatoEquivalentSwapSpreadQuote(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::RendistatoCalculator>& r,
            bool permanent);
    };

}

#endif
