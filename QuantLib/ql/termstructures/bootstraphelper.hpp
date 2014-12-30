/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2007, 2009 Ferdinando Ametrano
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

/*! \file bootstraphelper.hpp
    \brief base helper class used for bootstrapping
*/

#ifndef quantlib_bootstrap_helper_hpp
#define quantlib_bootstrap_helper_hpp

#include <ql/quote.hpp>
#include <ql/time/date.hpp>
#include <ql/handle.hpp>
#include <ql/patterns/observable.hpp>
#include <ql/patterns/visitor.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/settings.hpp>

namespace QuantLib {

//! Base helper class for bootstrapping
/*! This class provides an abstraction for the instruments used to
    bootstrap a term structure.

    It is advised that a bootstrap helper for an instrument
    contains an instance of the actual instrument class to ensure
    consistancy between the algorithms used during bootstrapping
    and later instrument pricing. This is not yet fully enforced
    in the available bootstrap helpers.
*/
template <class TS, class T = Real>
class BootstrapHelper : public Observer, public Observable {
  public:
    BootstrapHelper(const Handle<Quote_t<T> > &quote);
    BootstrapHelper(T quote);
    virtual ~BootstrapHelper() {}
    //! \name BootstrapHelper interface
    //@{
    const Handle<Quote_t<T> > &quote() const { return quote_; }
    virtual T impliedQuote() const = 0;
    T quoteError() const { return quote_->value() - impliedQuote(); }
    //! sets the term structure to be used for pricing
    /*! \warning Being a pointer and not a shared_ptr, the term
                 structure is not guaranteed to remain allocated
                 for the whole life of the rate helper. It is
                 responsibility of the programmer to ensure that
                 the pointer remains valid. It is advised that
                 this method is called only inside the term
                 structure being bootstrapped, setting the pointer
                 to <b>this</b>, i.e., the term structure itself.
    */
    virtual void setTermStructure(TS *);
    //! earliest relevant date
    /*! The earliest date at which data are needed by the
        helper in order to provide a quote.
    */
    virtual Date earliestDate() const;
    //! latest relevant date
    /*! The latest date at which data are needed by the
        helper in order to provide a quote. It does not
        necessarily equal the maturity of the underlying
        instrument.
    */
    virtual Date latestDate() const;
    //@}
    //! \name Observer interface
    //@{
    virtual void update();
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  protected:
    Handle<Quote_t<T> > quote_;
    TS *termStructure_;
    Date earliestDate_, latestDate_;
};

//! Bootstrap helper with date schedule relative to global evaluation date
/*! Derived classes must takes care of rebuilding the date schedule when
    the global evaluation date changes
*/
template <class TS, class T = Real>
class RelativeDateBootstrapHelper : public BootstrapHelper<TS, T> {
  public:
    RelativeDateBootstrapHelper(const Handle<Quote_t<T> > &quote);
    RelativeDateBootstrapHelper(T quote);
    //! \name Observer interface
    //@{
    void update() {
        if (evaluationDate_ != Settings::instance().evaluationDate()) {
            evaluationDate_ = Settings::instance().evaluationDate();
            initializeDates();
        }
        BootstrapHelper<TS, T>::update();
    }
    //@}
  protected:
    virtual void initializeDates() = 0;
    Date evaluationDate_;
};

// template definitions

template <class TS, class T>
BootstrapHelper<TS, T>::BootstrapHelper(const Handle<Quote_t<T> > &quote)
    : quote_(quote), termStructure_(0) {
    registerWith(quote_);
}

template <class TS, class T>
BootstrapHelper<TS, T>::BootstrapHelper(T quote)
    : quote_(Handle<Quote_t<T> >(
          boost::shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(quote)))),
      termStructure_(0) {}

template <class TS, class T>
void BootstrapHelper<TS, T>::setTermStructure(TS *t) {
    QL_REQUIRE(t != 0, "null term structure given");
    termStructure_ = t;
}

template <class TS, class T> Date BootstrapHelper<TS, T>::earliestDate() const {
    return earliestDate_;
}

template <class TS, class T> Date BootstrapHelper<TS, T>::latestDate() const {
    return latestDate_;
}

template <class TS, class T> void BootstrapHelper<TS, T>::update() {
    notifyObservers();
}

template <class TS, class T>
void BootstrapHelper<TS, T>::accept(AcyclicVisitor &v) {
    Visitor<BootstrapHelper<TS, T> > *v1 =
        dynamic_cast<Visitor<BootstrapHelper<TS, T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        QL_FAIL("not a bootstrap-helper visitor");
}

template <class TS, class T>
RelativeDateBootstrapHelper<TS, T>::RelativeDateBootstrapHelper(
    const Handle<Quote_t<T> > &quote)
    : BootstrapHelper<TS, T>(quote) {
    this->registerWith(Settings::instance().evaluationDate());
    evaluationDate_ = Settings::instance().evaluationDate();
}

template <class TS, class T>
RelativeDateBootstrapHelper<TS, T>::RelativeDateBootstrapHelper(T quote)
    : BootstrapHelper<TS, T>(quote) {
    this->registerWith(Settings::instance().evaluationDate());
    evaluationDate_ = Settings::instance().evaluationDate();
}

namespace detail {

class BootstrapHelperSorter {
  public:
    template <class Helper>
    bool operator()(const boost::shared_ptr<Helper> &h1,
                    const boost::shared_ptr<Helper> &h2) const {
        return (h1->latestDate() < h2->latestDate());
    }
};
}
}

#endif
