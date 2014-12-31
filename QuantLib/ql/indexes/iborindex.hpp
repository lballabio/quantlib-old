/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 StatPro Italia srl
 Copyright (C) 2009 Ferdinando Ametrano
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

/*! \file iborindex.hpp
    \brief base class for Inter-Bank-Offered-Rate indexes
*/

#ifndef quantlib_ibor_index_hpp
#define quantlib_ibor_index_hpp

#include <ql/indexes/interestrateindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

template <class T> class IborCoupon_t;

//! base class for Inter-Bank-Offered-Rate indexes (e.g. %Libor, etc.)
template <class T> class IborIndex_t : public InterestRateIndex_t<T> {
  public:
    IborIndex_t(const std::string &familyName, const Period &tenor,
                Natural settlementDays, const Currency &currency,
                const Calendar &fixingCalendar,
                BusinessDayConvention convention, bool endOfMonth,
                const DayCounter &dayCounter,
                const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >());
    //! \name InterestRateIndex interface
    //@{
    Date maturityDate(const Date &valueDate) const;
    T forecastFixing(const Date &fixingDate) const;
    // @}
    //! \name Inspectors
    //@{
    BusinessDayConvention businessDayConvention() const;
    bool endOfMonth() const { return endOfMonth_; }
    //! the curve used to forecast fixings
    Handle<YieldTermStructure_t<T> > forwardingTermStructure() const;
    //@}
    //! \name Other methods
    //@{
    //! returns a copy of itself linked to a different forwarding curve
    virtual boost::shared_ptr<IborIndex_t<T> >
    clone(const Handle<YieldTermStructure_t<T> > &forwarding) const;
    // @}
  protected:
    BusinessDayConvention convention_;
    Handle<YieldTermStructure_t<T> > termStructure_;
    bool endOfMonth_;

  private:
    // overload to avoid date/time (re)calculation
    /* This can be called with cached coupon dates (and it does
       give quite a performance boost to coupon calculations) but
       is potentially misleading: by passing the wrong dates, one
       can ask a 6-months index for a 1-year fixing.

       For that reason, we're leaving this method private and
       we're declaring the IborCoupon class (which uses it) as a
       friend.  Should the need arise, we might promote it to
       public, but before doing that I'd think hard whether we
       have any other way to get the same results.
    */
    T forecastFixing(const Date &valueDate, const Date &endDate, Time t) const;
    friend class IborCoupon_t<T>;
};

typedef IborIndex_t<Real> IborIndex;

// todo ...
class OvernightIndex : public IborIndex {
  public:
    OvernightIndex(
        const std::string &familyName, Natural settlementDays,
        const Currency &currency, const Calendar &fixingCalendar,
        const DayCounter &dayCounter,
        const Handle<YieldTermStructure> &h = Handle<YieldTermStructure>());
    //! returns a copy of itself linked to a different forwarding curve
    boost::shared_ptr<IborIndex>
    clone(const Handle<YieldTermStructure> &h) const;
};

// inline

template <class T>
inline T IborIndex_t<T>::forecastFixing(const Date &d1, const Date &d2,
                                        Time t) const {
    QL_REQUIRE(!this->termStructure_.empty(),
               "null term structure set to this instance of " << this->name());
    T disc1 = this->termStructure_->discount(d1);
    T disc2 = this->termStructure_->discount(d2);
    return (disc1 / disc2 - 1.0) / t;
}

// implementation

template <class T>
IborIndex_t<T>::IborIndex_t(const std::string &familyName, const Period &tenor,
                            Natural settlementDays, const Currency &currency,
                            const Calendar &fixingCalendar,
                            BusinessDayConvention convention, bool endOfMonth,
                            const DayCounter &dayCounter,
                            const Handle<YieldTermStructure_t<T> > &h)
    : InterestRateIndex_t<T>(familyName, tenor, settlementDays, currency,
                             fixingCalendar, dayCounter),
      convention_(convention), termStructure_(h), endOfMonth_(endOfMonth) {
    this->registerWith(termStructure_);
}

template <class T>
T IborIndex_t<T>::forecastFixing(const Date &fixingDate) const {
    Date d1 = this->valueDate(fixingDate);
    Date d2 = this->maturityDate(d1);
    Time t = this->dayCounter_.yearFraction(d1, d2);
    QL_REQUIRE(t > 0.0, "\n cannot calculate forward rate between "
                            << d1 << " and " << d2 << ":\n non positive time ("
                            << t << ") using " << this->dayCounter_.name()
                            << " daycounter");
    return this->forecastFixing(d1, d2, t);
}

template <class T>
Date IborIndex_t<T>::maturityDate(const Date &valueDate) const {
    return this->fixingCalendar().advance(valueDate, this->tenor_,
                                          this->convention_, this->endOfMonth_);
}

template <class T>
BusinessDayConvention IborIndex_t<T>::businessDayConvention() const {
    return this->convention_;
}

template <class T>
Handle<YieldTermStructure_t<T> >
IborIndex_t<T>::forwardingTermStructure() const {
    return this->termStructure_;
}

template <class T>
boost::shared_ptr<IborIndex_t<T> >
IborIndex_t<T>::clone(const Handle<YieldTermStructure_t<T> > &h) const {
    return boost::shared_ptr<IborIndex_t<T> >(new IborIndex_t<T>(
        this->familyName(), this->tenor(), this->fixingDays(), this->currency(),
        this->fixingCalendar(), this->businessDayConvention(),
        this->endOfMonth(), this->dayCounter(), h));
}

}

#endif
