/*
 Copyright (C) 2006, 2009 Ferdinando Ametrano
 Copyright (C) 2006, 2007, 2009 StatPro Italia srl
 Copyright (C) 2015 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.


 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details. */

/*! \file swapindex.hpp
    \brief swap-rate indexes
*/

#ifndef quantlib_swapindex_hpp
#define quantlib_swapindex_hpp

#include <ql/indexes/interestrateindex.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/instruments/makeois.hpp>
#include <ql/time/schedule.hpp>

#include <sstream>

namespace QuantLib {

using boost::shared_ptr;

class Schedule;

class VanillaSwap;

class OvernightIndex;
class OvernightIndexedSwap;

//! base class for swap-rate indexes
template <class T> class SwapIndex_t : public InterestRateIndex_t<T> {
  public:
    SwapIndex_t(const std::string &familyName, const Period &tenor,
                Natural settlementDays, Currency currency,
                const Calendar &fixingCalendar, const Period &fixedLegTenor,
                BusinessDayConvention fixedLegConvention,
                const DayCounter &fixedLegDayCounter,
                const boost::shared_ptr<IborIndex_t<T> > &iborIndex);
    SwapIndex_t(
        const std::string &familyName, const Period &tenor,
        Natural settlementDays, Currency currency,
        const Calendar &fixingCalendar, const Period &fixedLegTenor,
        BusinessDayConvention fixedLegConvention,
        const DayCounter &fixedLegDayCounter,
        const boost::shared_ptr<IborIndex_t<T> > &iborIndex,
        const Handle<YieldTermStructure_t<T> > &discountingTermStructure);
    //! \name InterestRateIndex interface
    //@{
    Date maturityDate(const Date &valueDate) const;
    //@}
    //! \name Inspectors
    //@{
    Period fixedLegTenor() const { return fixedLegTenor_; }
    BusinessDayConvention fixedLegConvention() const;
    boost::shared_ptr<IborIndex_t<T> > iborIndex() const { return iborIndex_; }
    Handle<YieldTermStructure_t<T> > forwardingTermStructure() const;
    Handle<YieldTermStructure_t<T> > discountingTermStructure() const;
    bool exogenousDiscount() const;
    /*! \warning Relinking the term structure underlying the index will
                 not have effect on the returned swap.
    */
    boost::shared_ptr<VanillaSwap> underlyingSwap(const Date &fixingDate) const;
    //@}
    //! \name Other methods
    //@{
    //! returns a copy of itself linked to a different forwarding curve
    virtual boost::shared_ptr<SwapIndex_t<T> >
    clone(const Handle<YieldTermStructure_t<T> > &forwarding) const;
    //! returns a copy of itself linked to different curves
    virtual boost::shared_ptr<SwapIndex_t<T> >
    clone(const Handle<YieldTermStructure_t<T> > &forwarding,
          const Handle<YieldTermStructure_t<T> > &discounting) const;
    //! returns a copy of itself with different tenor
    virtual boost::shared_ptr<SwapIndex_t<T> > clone(const Period &tenor) const;
    // @}
  protected:
    T forecastFixing(const Date &fixingDate) const;
    Period tenor_;
    boost::shared_ptr<IborIndex_t<T> > iborIndex_;
    Period fixedLegTenor_;
    BusinessDayConvention fixedLegConvention_;
    bool exogenousDiscount_;
    Handle<YieldTermStructure_t<T> > discount_;
    // cache data to avoid swap recreation when the same fixing date
    // is used multiple time to forecast changing fixing
    mutable boost::shared_ptr<VanillaSwap> lastSwap_;
    mutable Date lastFixingDate_;
};

typedef SwapIndex_t<Real> SwapIndex;

//! base class for overnight indexed swap indexes
class OvernightIndexedSwapIndex : public SwapIndex {
  public:
    OvernightIndexedSwapIndex(
        const std::string &familyName, const Period &tenor,
        Natural settlementDays, Currency currency,
        const boost::shared_ptr<OvernightIndex> &overnightIndex);
    //! \name Inspectors
    //@{
    boost::shared_ptr<OvernightIndex> overnightIndex() const;
    /*! \warning Relinking the term structure underlying the index will
                 not have effect on the returned swap.
    */
    boost::shared_ptr<OvernightIndexedSwap>
    underlyingSwap(const Date &fixingDate) const;
    //@}
  protected:
    boost::shared_ptr<OvernightIndex> overnightIndex_;
    // cache data to avoid swap recreation when the same fixing date
    // is used multiple time to forecast changing fixing
    mutable boost::shared_ptr<OvernightIndexedSwap> lastSwap_;
    mutable Date lastFixingDate_;
};

// inline definitions

template <class T>
inline BusinessDayConvention SwapIndex_t<T>::fixedLegConvention() const {
    return fixedLegConvention_;
}

template <class T> inline bool SwapIndex_t<T>::exogenousDiscount() const {
    return exogenousDiscount_;
}

inline boost::shared_ptr<OvernightIndex>
OvernightIndexedSwapIndex::overnightIndex() const {
    return overnightIndex_;
}

// implementation

template <class T>
SwapIndex_t<T>::SwapIndex_t(const std::string &familyName, const Period &tenor,
                            Natural settlementDays, Currency currency,
                            const Calendar &fixingCalendar,
                            const Period &fixedLegTenor,
                            BusinessDayConvention fixedLegConvention,
                            const DayCounter &fixedLegDayCounter,
                            const shared_ptr<IborIndex_t<T> > &iborIndex)
    : InterestRateIndex(familyName, tenor, settlementDays, currency,
                        fixingCalendar, fixedLegDayCounter),
      tenor_(tenor), iborIndex_(iborIndex), fixedLegTenor_(fixedLegTenor),
      fixedLegConvention_(fixedLegConvention), exogenousDiscount_(false),
      discount_(Handle<YieldTermStructure>()) {
    this->registerWith(iborIndex_);
}

template <class T>
SwapIndex_t<T>::SwapIndex_t(const std::string &familyName, const Period &tenor,
                            Natural settlementDays, Currency currency,
                            const Calendar &fixingCalendar,
                            const Period &fixedLegTenor,
                            BusinessDayConvention fixedLegConvention,
                            const DayCounter &fixedLegDayCounter,
                            const shared_ptr<IborIndex_t<T> > &iborIndex,
                            const Handle<YieldTermStructure_t<T> > &discount)
    : InterestRateIndex(familyName, tenor, settlementDays, currency,
                        fixingCalendar, fixedLegDayCounter),
      tenor_(tenor), iborIndex_(iborIndex), fixedLegTenor_(fixedLegTenor),
      fixedLegConvention_(fixedLegConvention), exogenousDiscount_(true),
      discount_(discount) {
    this->registerWith(iborIndex_);
}

template <class T>
Handle<YieldTermStructure_t<T> >
SwapIndex_t<T>::forwardingTermStructure() const {
    return iborIndex_->forwardingTermStructure();
}

template <class T>
Handle<YieldTermStructure_t<T> >
SwapIndex_t<T>::discountingTermStructure() const {
    return discount_; // empty if not exogenous
}

template <class T>
T SwapIndex_t<T>::forecastFixing(const Date &fixingDate) const {
    return underlyingSwap(fixingDate)->fairRate();
}

template <class T>
shared_ptr<VanillaSwap>
SwapIndex_t<T>::underlyingSwap(const Date &fixingDate) const {

    QL_REQUIRE(fixingDate != Date(), "null fixing date");

    // caching mechanism
    if (lastFixingDate_ != fixingDate) {
        Rate fixedRate = 0.0;
        if (exogenousDiscount_)
            lastSwap_ =
                MakeVanillaSwap(tenor_, iborIndex_, fixedRate)
                    .withEffectiveDate(this->valueDate(fixingDate))
                    .withFixedLegCalendar(this->fixingCalendar())
                    .withFixedLegDayCount(this->dayCounter_)
                    .withFixedLegTenor(fixedLegTenor_)
                    .withFixedLegConvention(fixedLegConvention_)
                    .withFixedLegTerminationDateConvention(fixedLegConvention_)
                    .withDiscountingTermStructure(discount_);
        else
            lastSwap_ =
                MakeVanillaSwap(tenor_, iborIndex_, fixedRate)
                    .withEffectiveDate(this->valueDate(fixingDate))
                    .withFixedLegCalendar(this->fixingCalendar())
                    .withFixedLegDayCount(this->dayCounter_)
                    .withFixedLegTenor(fixedLegTenor_)
                    .withFixedLegConvention(fixedLegConvention_)
                    .withFixedLegTerminationDateConvention(fixedLegConvention_);
        lastFixingDate_ = fixingDate;
    }
    return lastSwap_;
}

template <class T>
Date SwapIndex_t<T>::maturityDate(const Date &valueDate) const {
    Date fixDate = this->fixingDate(valueDate);
    return underlyingSwap(fixDate)->maturityDate();
}

template <class T>
shared_ptr<SwapIndex_t<T> > SwapIndex_t<T>::clone(
    const Handle<YieldTermStructure_t<T> > &forwarding) const {

    if (exogenousDiscount_)
        return shared_ptr<SwapIndex_t<T> >(new SwapIndex_t<T>(
            this->familyName(), this->tenor(), this->fixingDays(),
            this->currency(), this->fixingCalendar(), fixedLegTenor(),
            fixedLegConvention(), this->dayCounter(),
            iborIndex_->clone(forwarding), discount_));
    else
        return shared_ptr<SwapIndex_t<T> >(new SwapIndex_t<T>(
            this->familyName(), this->tenor(), this->fixingDays(),
            this->currency(), this->fixingCalendar(), fixedLegTenor(),
            fixedLegConvention(), this->dayCounter(),
            iborIndex_->clone(forwarding)));
}

template <class T>
shared_ptr<SwapIndex_t<T> > SwapIndex_t<T>::clone(
    const Handle<YieldTermStructure_t<T> > &forwarding,
    const Handle<YieldTermStructure_t<T> > &discounting) const {
    return shared_ptr<SwapIndex_t<T> >(new SwapIndex_t<T>(
        this->familyName(), this->tenor(), this->fixingDays(), this->currency(),
        this->fixingCalendar(), fixedLegTenor(), fixedLegConvention(),
        this->dayCounter(), iborIndex_->clone(forwarding), discounting));
}

template <class T>
shared_ptr<SwapIndex_t<T> > SwapIndex_t<T>::clone(const Period &tenor) const {

    if (exogenousDiscount_)
        return shared_ptr<SwapIndex_t<T> >(new SwapIndex_t<T>(
            this->familyName(), tenor, this->fixingDays(), this->currency(), this->fixingCalendar(),
            fixedLegTenor(), fixedLegConvention(), this->dayCounter(), iborIndex(),
            discountingTermStructure()));
    else
        return shared_ptr<SwapIndex_t<T> >(new SwapIndex_t<T>(
            this->familyName(), tenor, this->fixingDays(), this->currency(), this->fixingCalendar(),
            fixedLegTenor(), fixedLegConvention(), this->dayCounter(), iborIndex()));
}

}

#endif
