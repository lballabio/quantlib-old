/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2010, 2011 Ferdinando Ametrano

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

#include <qlo/btp.hpp>

#include <ql/instruments/bonds/btp.hpp>
#include <ql/currencies/europe.hpp>
#include <ql/utilities/dataformatters.hpp>

using std::vector;
using boost::shared_ptr;

namespace QuantLibAddin {

    CCTEU::CCTEU(const shared_ptr<ObjectHandler::ValueObject>& properties,
                 const std::string& des,
                 const QuantLib::Date& maturityDate,
                 QuantLib::Spread spread,
                 const QuantLib::Date& startDate,
                 const QuantLib::Date& issueDate,
                 bool permanent)
    : FloatingRateBond(properties, des, QuantLib::EURCurrency(), permanent)
    {
        qlBondObject_ = shared_ptr<QuantLib::CCTEU>(new
            QuantLib::CCTEU(maturityDate, spread, startDate, issueDate));
        libraryObject_ = qlBondObject_;
        if (description_.empty()) {
            std::ostringstream temp;
            temp << "CCTEU ";
            temp << QuantLib::io::iso_date(qlBondObject_->maturityDate());
            temp << " " << spread*10000 << "bp";
            description_ = temp.str();
        }
    }

    BTP::BTP(const shared_ptr<ObjectHandler::ValueObject>& properties,
             const std::string& des,
             const QuantLib::Date& maturityDate,
             QuantLib::Rate fixedRate,
             const QuantLib::Date& startDate,
             const QuantLib::Date& issueDate,
             bool permanent)
    : FixedRateBond(properties, des, QuantLib::EURCurrency(), permanent)
    {
        qlBondObject_ = shared_ptr<QuantLib::BTP>(new
            QuantLib::BTP(maturityDate, fixedRate, startDate, issueDate));
        libraryObject_ = qlBondObject_;
        if (description_.empty()) {
            std::ostringstream temp;
            temp << "BTP ";
            temp << QuantLib::io::iso_date(qlBondObject_->maturityDate());
            temp << " " << fixedRate*100.0 << "%";
            description_ = temp.str();
        }
    }

    BTP::BTP(const shared_ptr<ObjectHandler::ValueObject>& properties,
             const std::string& des,
             const QuantLib::Date& maturityDate,
             QuantLib::Rate fixedRate,
             QuantLib::Real redemption,
             const QuantLib::Date& startDate,
             const QuantLib::Date& issueDate,
             bool permanent)
    : FixedRateBond(properties, des, QuantLib::EURCurrency(), permanent)
    {
        qlBondObject_ = shared_ptr<QuantLib::BTP>(new
            QuantLib::BTP(maturityDate, fixedRate,
                          redemption, startDate, issueDate));
        libraryObject_ = qlBondObject_;
        if (description_.empty()) {
            std::ostringstream temp;
            temp << "BTP ";
            temp << QuantLib::io::iso_date(qlBondObject_->maturityDate());
            temp << " " << fixedRate*100.0 << "%";
            description_ = temp.str();
        }
    }

    RendistatoBasket::RendistatoBasket(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<boost::shared_ptr<QuantLib::BTP> >& btps,
        const std::vector<QuantLib::Real>& outstandings,
        const std::vector<QuantLib::Handle<QuantLib::Quote> >& cleanPriceQuotes,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::RendistatoBasket>(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RendistatoBasket>(new
            QuantLib::RendistatoBasket(btps, outstandings, cleanPriceQuotes));
    }

    RendistatoCalculator::RendistatoCalculator(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::RendistatoBasket>& basket,
        const boost::shared_ptr<QuantLib::Euribor>& euriborIndex,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& discountCurve,
        bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::RendistatoCalculator>(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RendistatoCalculator>(new
            QuantLib::RendistatoCalculator(basket, euriborIndex, discountCurve));
    }


    RendistatoEquivalentSwapLengthQuote::RendistatoEquivalentSwapLengthQuote(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::RendistatoCalculator>& r,
        bool permanent)
    : Quote(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::RendistatoEquivalentSwapLengthQuote(r));
    }

    RendistatoEquivalentSwapSpreadQuote::RendistatoEquivalentSwapSpreadQuote(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::RendistatoCalculator>& r,
        bool permanent)
    : Quote(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::RendistatoEquivalentSwapSpreadQuote(r));
    }

}
