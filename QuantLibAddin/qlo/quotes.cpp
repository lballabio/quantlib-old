
/*
 Copyright (C) 2006 François du Vignaud
 Copyright (C) 2006 Giorgio Facchinetti

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


#include <qlo/quotes.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/quotes/derivedquote.hpp>
#include <ql/quotes/eurodollarfuturesquote.hpp>
#include <ql/quotes/forwardvaluequote.hpp>
#include <ql/quotes/futuresconvadjustmentquote.hpp>
#include <ql/quotes/impliedstddevquote.hpp>
#include <ql/termstructures/volatilities/interestrate/caplet/capstripper2.hpp>

namespace QuantLibAddin {

    SimpleQuote::SimpleQuote(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Real value,
            QuantLib::Real tickValue,
            bool permanent) : Quote(properties, permanent) {
        // The base class requires us to store a reference
        // to a QuantLib::Quote in libraryObject_.
        // For performance reasons we also store a reference to
        // QuantLib::SimpleQuote in simpleQuote_, without this we would have to
        // do a pointer cast on every call to QuantLibAddin::SimpleQuote::setValue().
        libraryObject_ = simpleQuote_ = boost::shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(value));
    }

    QuantLib::Real SimpleQuote::tickValue() const {
        return boost::any_cast<double>(propertyValue("TickValue"));
    }

    void SimpleQuote::setTickValue(QuantLib::Real tickValue) {
        properties()->setProperty("TickValue", tickValue);
    }

    // QuantLibAddin::SimpleQuote::setValue() wraps QuantLib::SimpleQuote::setValue(),
    // updating the ValueObject.  The "Value" property of the ValueObject is represented
    // as datatype ObjectHandler::Variant (rather than double), this is necessary for
    // platform-independent processing of value Null<Real>.
    QuantLib::Real SimpleQuote::setValue(QuantLib::Real value) {

        QuantLib::Real result;

        try {
            result = simpleQuote_->setValue(value);
        } catch (...) { 
            // In the event of an exception, ensure that the ValueObject remains in synch with
            // the simpleQuote_ before rethrowing.  
            // If QuantLib::SimpleQuote::isValid() is false then QuantLib::SimpleQuote::value() throws,
            // which here in the catch clause would cause the app to crash, so test for that case.
            if (simpleQuote_->isValid())
                properties()->setProperty("Value", ObjectHandler::Variant(simpleQuote_->value()));
            else
                properties()->setProperty("Value", 
                    ObjectHandler::Variant(static_cast<double>(QuantLib::Null<QuantLib::Real>())));
            throw;
        }

        properties()->setProperty("Value", ObjectHandler::Variant(value));
        return result;

    }


    ForwardValueQuote::ForwardValueQuote(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::IborIndex>& index,
        const QuantLib::Date& fixingDate,
        bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::ForwardValueQuote(index, fixingDate));
    }
    
    ImpliedStdDevQuote::ImpliedStdDevQuote(
                            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                            QuantLib::Option::Type optionType,
                            const QuantLib::Handle<QuantLib::Quote>& forward,
                            const QuantLib::Handle<QuantLib::Quote>& price,
                            QuantLib::Real strike,
                            QuantLib::Real guess,
                            QuantLib::Real accuracy,
                            bool permanent) : Quote(properties, permanent)
    {
    libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
        QuantLib::ImpliedStdDevQuote(optionType, forward, price, strike,
                                     guess, accuracy));
    }

    EurodollarFuturesImpliedStdDevQuote::EurodollarFuturesImpliedStdDevQuote(
                        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                        const QuantLib::Handle<QuantLib::Quote>& forward,
                        const QuantLib::Handle<QuantLib::Quote>& callPrice,
                        const QuantLib::Handle<QuantLib::Quote>& putPrice,
                        QuantLib::Real strike,
                        QuantLib::Real guess,
                        QuantLib::Real accuracy,
                        bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::EurodollarFuturesImpliedStdDevQuote(forward,
                callPrice, putPrice, strike, guess, accuracy));
    }

    FuturesConvAdjustmentQuote::FuturesConvAdjustmentQuote(
                               const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                               const boost::shared_ptr<QuantLib::IborIndex>& index,
                               const std::string& immCode, 
                               const QuantLib::Handle<QuantLib::Quote>& futuresQuote,
                               const QuantLib::Handle<QuantLib::Quote>& volatility,
                               const QuantLib::Handle<QuantLib::Quote>& meanReversion,
                               bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::FuturesConvAdjustmentQuote(index, immCode,
                                                 futuresQuote,
                                                 volatility,
                                                 meanReversion));
    }

    struct PriceToRate {
        QuantLib::Real operator ()(QuantLib::Real price) const {
            return 1-price*.01;
        }
    };

    PriceToRateQuote::PriceToRateQuote(
                               const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                               const QuantLib::Handle<QuantLib::Quote>& quote,
                               bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::DerivedQuote<PriceToRate>(quote, PriceToRate()));
    }

    CapStripperQuote::CapStripperQuote(
                    const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    const boost::shared_ptr<QuantLib::CapsStripper2>& capsStripper,
                    QuantLib::Period& tenor,
                    QuantLib::Real strike,
                    bool permanent): Quote(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::CapStripperQuote(capsStripper, tenor, strike));
    }

}

