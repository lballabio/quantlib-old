/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2015 Peter Caspers

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

/*! \file instrument.hpp
    \brief Abstract instrument class
*/

#ifndef quantlib_instrument_hpp
#define quantlib_instrument_hpp

#include <ql/patterns/lazyobject.hpp>
#include <ql/pricingengine.hpp>
#include <ql/utilities/null.hpp>
#include <ql/time/date.hpp>
#include <boost/any.hpp>
#include <map>
#include <string>

namespace QuantLib {

    //! Abstract instrument class
    /*! This class is purely abstract and defines the interface of concrete
        instruments which will be derived from this one.

        \test observability of class instances is checked.
    */
    template<class T>
    class Instrument_t : public LazyObject {
      public:
        class results;
        Instrument_t();
        //! \name Inspectors
        //@{

        //! returns the net present value of the instrument.
        T NPV() const;
        //! returns the error estimate on the NPV when available.
        T errorEstimate() const;
        //! returns the date the net present value refers to.
        const Date& valuationDate() const;

        //! returns any additional result returned by the pricing engine.
        template <typename R> R result(const std::string& tag) const;
        //! returns all additional result returned by the pricing engine.
        const std::map<std::string,boost::any>& additionalResults() const;

        //! returns whether the instrument might have value greater than zero.
        virtual bool isExpired() const = 0;
        //@}
        //! \name Modifiers
        //@{
        //! set the pricing engine to be used.
        /*! \warning calling this method will have no effects in
                     case the <b>performCalculation</b> method
                     was overridden in a derived class.
        */
        void setPricingEngine(const boost::shared_ptr<PricingEngine>&);
        //@}
        /*! When a derived argument structure is defined for an
            instrument, this method should be overridden to fill
            it. This is mandatory in case a pricing engine is used.
        */
        virtual void setupArguments(PricingEngine::arguments*) const;
        /*! When a derived result structure is defined for an
            instrument, this method should be overridden to read from
            it. This is mandatory in case a pricing engine is used.
        */
        virtual void fetchResults(const PricingEngine::results*) const;
      protected:
        //! \name Calculations
        //@{
        void calculate() const;
        /*! This method must leave the instrument in a consistent
            state when the expiration condition is met.
        */
        virtual void setupExpired() const;
        /*! In case a pricing engine is <b>not</b> used, this
            method must be overridden to perform the actual
            calculations and set any needed results. In case
            a pricing engine is used, the default implementation
            can be used.
        */
        virtual void performCalculations() const;
        //@}
        /*! \name Results
            The value of this attribute and any other that derived
            classes might declare must be set during calculation.
        */
        //@{
        mutable T NPV_, errorEstimate_;
        mutable Date valuationDate_;
        mutable std::map<std::string,boost::any> additionalResults_;
        //@}
        boost::shared_ptr<PricingEngine> engine_;
    };

    template<class T>
    class Instrument_t<T>::results : public virtual PricingEngine::results {
      public:
        void reset() {
            value = errorEstimate = Null<Real>();
            valuationDate = Date();
            additionalResults.clear();
        }
        T value;
        T errorEstimate;
        Date valuationDate;
        std::map<std::string,boost::any> additionalResults;
    };

    typedef Instrument_t<Real> Instrument;

    // inline definitions

    template<class T>
    inline Instrument_t<T>::Instrument_t()
    : NPV_(Null<Real>()), errorEstimate_(Null<Real>()),
      valuationDate_(Date()) {}

    template<class T>
    inline void Instrument_t<T>::setPricingEngine(
                                  const boost::shared_ptr<PricingEngine>& e) {
        if (engine_)
            unregisterWith(engine_);
        engine_ = e;
        if (engine_)
            registerWith(engine_);
        // trigger (lazy) recalculation and notify observers
        update();
    }

    template<class T>
    inline void Instrument_t<T>::setupArguments(PricingEngine::arguments*) const {
        QL_FAIL("Instrument::setupArguments() not implemented");
    }

    template<class T>
    inline void Instrument_t<T>::calculate() const {
        if (isExpired()) {
            setupExpired();
            calculated_ = true;
        } else {
            LazyObject::calculate();
        }
    }

    template<class T>
    inline void Instrument_t<T>::setupExpired() const {
        NPV_ = errorEstimate_ = 0.0;
        valuationDate_ = Date();
        additionalResults_.clear();
    }

    template<class T>
    inline void Instrument_t<T>::performCalculations() const {
        QL_REQUIRE(engine_, "null pricing engine");
        engine_->reset();
        setupArguments(engine_->getArguments());
        engine_->getArguments()->validate();
        engine_->calculate();
        fetchResults(engine_->getResults());
    }

    template<class T>
    inline void Instrument_t<T>::fetchResults(
                                      const PricingEngine::results* r) const {
        const Instrument_t<T>::results* results =
            dynamic_cast<const Instrument::results*>(r);
        QL_ENSURE(results != 0,
                  "no results returned from pricing engine");

        NPV_ = results->value;
        errorEstimate_ = results->errorEstimate;
        valuationDate_ = results->valuationDate;

        additionalResults_ = results->additionalResults;
    }

    template<class T>
    inline T Instrument_t<T>::NPV() const {
        calculate();
        QL_REQUIRE(NPV_ != Null<Real>(), "NPV not provided");
        return NPV_;
    }

    template<class T>
    inline T Instrument_t<T>::errorEstimate() const {
        calculate();
        QL_REQUIRE(errorEstimate_ != Null<Real>(),
                   "error estimate not provided");
        return errorEstimate_;
    }

    template<class T>
    inline const Date& Instrument_t<T>::valuationDate() const {
        calculate();
        QL_REQUIRE(valuationDate_ != Date(),
                   "valuation date not provided");
        return valuationDate_;
    }

    template <class T> template <class R>
    inline R Instrument_t<T>::result(const std::string& tag) const {
        calculate();
        std::map<std::string,boost::any>::const_iterator value =
            additionalResults_.find(tag);
        QL_REQUIRE(value != additionalResults_.end(),
                   tag << " not provided");
        return boost::any_cast<R>(value->second);
    }

    template<class T>
    inline const std::map<std::string,boost::any>&
    Instrument_t<T>::additionalResults() const {
        return additionalResults_;
    }

}

#endif
