/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Francois du Vignaud
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
#include <ql/quotes/forwardswapquote.hpp>
#include <ql/quotes/futuresconvadjustmentquote.hpp>
#include <ql/quotes/impliedstddevquote.hpp>
#include <ql/quotes/sensitivityanalysis.hpp>
#include <ql/termstructures/volatility/optionlet/optionletstripper.hpp>

using std::vector;
using std::pair;
using boost::shared_ptr;
using QuantLib::Real;

namespace QuantLibAddin {

    SimpleQuote::SimpleQuote(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            Real value,
            Real tickValue,
            bool permanent) : Quote(properties, permanent) {
        // The base class requires us to store a reference
        // to a QuantLib::Quote in libraryObject_.
        // For performance reasons we also store a reference to
        // QuantLib::SimpleQuote in simpleQuote_, without this we would have to
        // do a pointer cast on every call to QuantLibAddin::SimpleQuote::setValue().
        libraryObject_ = simpleQuote_ = shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(value));
    }

    Real SimpleQuote::tickValue() const {
        return boost::any_cast<double>(propertyValue("TICKVALUE"));
    }

    void SimpleQuote::setTickValue(Real tickValue) {
        properties()->setProperty("TICKVALUE", tickValue);
    }

    // QuantLibAddin::SimpleQuote::setValue() wraps QuantLib::SimpleQuote::setValue(),
    // updating the ValueObject.  The "Value" property of the ValueObject is represented
    // as datatype ObjectHandler::Variant (rather than double), this is necessary for
    // platform-independent processing of value Null<Real>.
    Real SimpleQuote::setValue(Real value) {

        Real result;

        try {
            result = simpleQuote_->setValue(value);
        } catch (...) { 
            // In the event of an exception, ensure that the ValueObject remains in synch with
            // the simpleQuote_ before rethrowing.  
            // If QuantLib::SimpleQuote::isValid() is false then QuantLib::SimpleQuote::value() throws,
            // which here in the catch clause would cause the app to crash, so test for that case.
            if (simpleQuote_->isValid())
                properties()->setProperty("VALUE", ObjectHandler::Variant(simpleQuote_->value()));
            else
                properties()->setProperty("VALUE", 
                    ObjectHandler::Variant(static_cast<double>(QuantLib::Null<Real>())));
            throw;
        }

        properties()->setProperty("VALUE", ObjectHandler::Variant(value));
        return result;

    }


    ForwardValueQuote::ForwardValueQuote(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        const shared_ptr<QuantLib::IborIndex>& index,
        const QuantLib::Date& fixingDate,
        bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::ForwardValueQuote(index, fixingDate));
    }
    
    ForwardSwapQuote::ForwardSwapQuote(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const shared_ptr<QuantLib::SwapIndex>& swapIndex,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            const QuantLib::Period& fwdStart,
            bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::ForwardSwapQuote(swapIndex, spread, fwdStart));
    }

    ImpliedStdDevQuote::ImpliedStdDevQuote(
                            const shared_ptr<ObjectHandler::ValueObject>& properties,
                            QuantLib::Option::Type optionType,
                            const QuantLib::Handle<QuantLib::Quote>& forward,
                            const QuantLib::Handle<QuantLib::Quote>& price,
                            Real strike,
                            Real guess,
                            Real accuracy,
                            bool permanent) : Quote(properties, permanent)
    {
    libraryObject_ = shared_ptr<QuantLib::Quote>(new
        QuantLib::ImpliedStdDevQuote(optionType, forward, price, strike,
                                     guess, accuracy));
    }

    EurodollarFuturesImpliedStdDevQuote::EurodollarFuturesImpliedStdDevQuote(
                        const shared_ptr<ObjectHandler::ValueObject>& properties,
                        const QuantLib::Handle<QuantLib::Quote>& forward,
                        const QuantLib::Handle<QuantLib::Quote>& callPrice,
                        const QuantLib::Handle<QuantLib::Quote>& putPrice,
                        Real strike,
                        Real guess,
                        Real accuracy,
                        bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::EurodollarFuturesImpliedStdDevQuote(forward,
                callPrice, putPrice, strike, guess, accuracy));
    }

    FuturesConvAdjustmentQuote::FuturesConvAdjustmentQuote(
                               const shared_ptr<ObjectHandler::ValueObject>& properties,
                               const shared_ptr<QuantLib::IborIndex>& index,
                               const std::string& immCode, 
                               const QuantLib::Handle<QuantLib::Quote>& futuresQuote,
                               const QuantLib::Handle<QuantLib::Quote>& volatility,
                               const QuantLib::Handle<QuantLib::Quote>& meanReversion,
                               bool permanent) : Quote(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Quote>(new
            QuantLib::FuturesConvAdjustmentQuote(index, immCode,
                                                 futuresQuote,
                                                 volatility,
                                                 meanReversion));
    }

    vector<vector<Real> >
    bucketAnalysis(const vector<vector<QuantLib::Handle<QuantLib::Quote> > >& q,
                   const vector<shared_ptr<QuantLib::Instrument> >& instr,
                   const std::vector<QuantLib::Real>& quant,
                   Real shift,
                   QuantLib::SensitivityAnalysis type)
    {
        pair<vector<vector<Real> >, vector<vector<Real> > > result;
        vector<vector<QuantLib::Handle<QuantLib::SimpleQuote> > > sq(q.size());
        for (QuantLib::Size i=0; i<q.size(); ++i) {
            sq[i] = vector<QuantLib::Handle<QuantLib::SimpleQuote> >(q[i].size());
            for (QuantLib::Size j=0; j<q[i].size(); ++j) {
                boost::shared_ptr<QuantLib::Quote> t(q[i][j].currentLink());
                boost::shared_ptr<QuantLib::SimpleQuote> tt = 
                    boost::dynamic_pointer_cast<QuantLib::SimpleQuote>(t);
                sq[i][j] = QuantLib::Handle<QuantLib::SimpleQuote>(tt);
            }
        }
        result = QuantLib::bucketAnalysis(sq, instr, quant, shift, type);
        return result.first;
    }
}
