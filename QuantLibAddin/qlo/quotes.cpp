
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

namespace QuantLibAddin {

    SimpleQuote::SimpleQuote(QuantLib::Real value,
                             QuantLib::Real tickValue) {
        // The base class requires us to store a reference
        // to a QuantLib::Quote in libraryObject_.
        // For performance reasons we also store a reference to
        // QuantLib::SimpleQuote in simpleQuote_, without this we would have to
        // do a pointer cast on every call to QuantLibAddin::SimpleQuote::setValue().
        libraryObject_ = simpleQuote_ = boost::shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(value));
    }

    QuantLib::Real SimpleQuote::tickValue() const {
        return boost::any_cast<double>(propertyValue("tickValue"));
    }

    void SimpleQuote::setTickValue(QuantLib::Real tickValue) {
        properties()->setProperty("tickValue", tickValue);
    }

    QuantLib::Real SimpleQuote::setValue(QuantLib::Real value) {

        QuantLib::Real result;

        try {
            result = simpleQuote_->setValue(value);
        } catch (...) { 
            // In the event of an exception, ensure that the ValueObject remains
            // in synch with the simpleQuote_ before rethrowing.
                    // is the if/else really needed? not in my opinion...
                    // 2007-07-13 Nando
            if (simpleQuote_->isValid())
                properties()->setProperty("value", simpleQuote_->value());
            else
                properties()->setProperty("value", QuantLib::Null<QuantLib::Real>());
            throw;
        }

        properties()->setProperty("value", value);
        return result;

    }


    ForwardValueQuote::ForwardValueQuote(
        const boost::shared_ptr<QuantLib::IborIndex>& index,
        const QuantLib::Date& fixingDate)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::ForwardValueQuote(index, fixingDate));
    }
    
    ImpliedStdDevQuote::ImpliedStdDevQuote(
                            QuantLib::Option::Type optionType,
                            const QuantLib::Handle<QuantLib::Quote>& forward,
                            const QuantLib::Handle<QuantLib::Quote>& price,
                            QuantLib::Real strike,
                            QuantLib::Real guess,
                            QuantLib::Real accuracy)
    {
    libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
        QuantLib::ImpliedStdDevQuote(optionType, forward, price, strike,
                                     guess, accuracy));
    }

    EurodollarFuturesImpliedStdDevQuote::EurodollarFuturesImpliedStdDevQuote(
                        const QuantLib::Handle<QuantLib::Quote>& forward,
                        const QuantLib::Handle<QuantLib::Quote>& callPrice,
                        const QuantLib::Handle<QuantLib::Quote>& putPrice,
                        QuantLib::Real strike,
                        QuantLib::Real guess,
                        QuantLib::Real accuracy)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::EurodollarFuturesImpliedStdDevQuote(forward,
                callPrice, putPrice, strike, guess, accuracy));
    }

    FuturesConvAdjustmentQuote::FuturesConvAdjustmentQuote(
                               const boost::shared_ptr<QuantLib::IborIndex>& index,
                               const std::string& immCode, 
                               const QuantLib::Handle<QuantLib::Quote>& futuresQuote,
                               const QuantLib::Handle<QuantLib::Quote>& volatility,
                               const QuantLib::Handle<QuantLib::Quote>& meanReversion)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Quote>(new
            QuantLib::FuturesConvAdjustmentQuote(index, immCode,
                                                 futuresQuote,
                                                 volatility,
                                                 meanReversion));
    }

}
