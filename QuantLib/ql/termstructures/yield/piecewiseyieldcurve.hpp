/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2007, 2008, 2009 Ferdinando Ametrano
 Copyright (C) 2007 Chris Kenyon
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

/*! \file piecewiseyieldcurve.hpp
    \brief piecewise-interpolated term structure
*/

#ifndef quantlib_piecewise_yield_curve_hpp
#define quantlib_piecewise_yield_curve_hpp

#include <ql/termstructures/iterativebootstrap.hpp>
#include <ql/termstructures/localbootstrap.hpp>
#include <ql/termstructures/yield/bootstraptraits.hpp>
#include <ql/patterns/lazyobject.hpp>

namespace QuantLib {

//! Piecewise yield term structure
/*! This term structure is bootstrapped on a number of interest
    rate instruments which are passed as a vector of handles to
    RateHelper instances. Their maturities mark the boundaries of
    the interpolated segments.

    Each segment is determined sequentially starting from the
    earliest period to the latest and is chosen so that the
    instrument whose maturity marks the end of such segment is
    correctly repriced on the curve.

    \warning The bootstrapping algorithm will raise an exception if
             any two instruments have the same maturity date.

    \ingroup yieldtermstructures

    \test
    - the correctness of the returned values is tested by
      checking them against the original inputs.
    - the observability of the term structure is tested.
*/
template <template <class> class Traits, template <class> class Interpolator,
          template <class, class> class Bootstrap = IterativeBootstrap,
          class T = Real>
class PiecewiseYieldCurve
    : public Traits<T>::template curve<Interpolator>::type,
      public LazyObject {
  private:
    typedef
        typename Traits<T>::template curve<Interpolator>::type base_curve;
    typedef PiecewiseYieldCurve<Traits, Interpolator, Bootstrap> this_curve;

  public:
    typedef Traits<T> traits_type;
    typedef Interpolator<T> interpolator_type;
    //! \name Constructors
    //@{
    PiecewiseYieldCurve(
        const Date &referenceDate,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter, const std::vector<Handle<Quote> > &jumps =
                                          std::vector<Handle<Quote> >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        Real accuracy = 1.0e-12, const Interpolator<T> &i = Interpolator<T>(),
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(referenceDate, dayCounter, jumps, jumpDates, i),
          instruments_(instruments), accuracy_(accuracy),
          bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    PiecewiseYieldCurve(
        const Date &referenceDate,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter, Real accuracy,
        const Interpolator<T> &i = Interpolator<T>(),
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(referenceDate, dayCounter,
                     std::vector<Handle<Quote_t<T> > >(), std::vector<Date>(),
                     i),
          instruments_(instruments), accuracy_(accuracy),
          bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    PiecewiseYieldCurve(
        const Date &referenceDate,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter, const Interpolator<T> &i,
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(referenceDate, dayCounter,
                     std::vector<Handle<Quote_t<T> > >(), std::vector<Date>(),
                     i),
          instruments_(instruments), accuracy_(1.0e-12), bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    PiecewiseYieldCurve(
        Natural settlementDays, const Calendar &calendar,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        Real accuracy = 1.0e-12, const Interpolator<T> &i = Interpolator<T>(),
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(settlementDays, calendar, dayCounter, jumps, jumpDates, i),
          instruments_(instruments), accuracy_(accuracy),
          bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    PiecewiseYieldCurve(
        Natural settlementDays, const Calendar &calendar,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter, Real accuracy,
        const Interpolator<T> &i = Interpolator<T>(),
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(settlementDays, calendar, dayCounter,
                     std::vector<Handle<Quote_t<T> > >(), std::vector<Date>(),
                     i),
          instruments_(instruments), accuracy_(accuracy),
          bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    PiecewiseYieldCurve(
        Natural settlementDays, const Calendar &calendar,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        const DayCounter &dayCounter, const Interpolator<T> &i,
        const Bootstrap<this_curve, T> &bootstrap = Bootstrap<this_curve, T>())
        : base_curve(settlementDays, calendar, dayCounter,
                     std::vector<Handle<Quote_t<T> > >(), std::vector<Date>(),
                     i),
          instruments_(instruments), accuracy_(1.0e-12), bootstrap_(bootstrap) {
        bootstrap_.setup(this);
    }
    //@}
    //! \name TermStructure interface
    //@{
    Date maxDate() const;
    //@}
    //! \name base_curve interface
    //@{
    const std::vector<Time> &times() const;
    const std::vector<Date> &dates() const;
    const std::vector<Real> &data() const;
    std::vector<std::pair<Date, T> > nodes() const;
    //@}
    //! \name Observer interface
    //@{
    void update();
    //@}
  private:
    //! \name LazyObject interface
    //@{
    void performCalculations() const;
    //@}
    // methods
    DiscountFactor discountImpl(Time) const;
    // data members
    std::vector<boost::shared_ptr<typename Traits<T>::helper> > instruments_;
    Real accuracy_;

    // bootstrapper classes are declared as friend to manipulate
    // the curve data. They might be passed the data instead, but
    // it would increase the complexity---which is high enough
    // already.
    friend class Bootstrap<this_curve, T>;
    friend class BootstrapError<this_curve, T>;
    friend class PenaltyFunction<this_curve, T>;
    Bootstrap<this_curve, T> bootstrap_;
};

// inline definitions

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline Date PiecewiseYieldCurve<C, I, B, T>::maxDate() const {
    calculate();
    return base_curve::maxDate();
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline const std::vector<Time> &PiecewiseYieldCurve<C, I, B, T>::times() const {
    calculate();
    return base_curve::times();
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline const std::vector<Date> &PiecewiseYieldCurve<C, I, B, T>::dates() const {
    calculate();
    return base_curve::dates();
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline const std::vector<Real> &PiecewiseYieldCurve<C, I, B, T>::data() const {
    calculate();
    return base_curve::data();
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline std::vector<std::pair<Date, T> >
PiecewiseYieldCurve<C, I, B, T>::nodes() const {
    calculate();
    return base_curve::nodes();
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline void PiecewiseYieldCurve<C, I, B, T>::update() {

    // it dispatches notifications only if (!calculated_ && !frozen_)
    LazyObject::update();

    // do not use base_curve::update() as it would always notify observers

    // TermStructure::update() update part
    if (this->moving_)
        this->updated_ = false;
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline DiscountFactor PiecewiseYieldCurve<C, I, B, T>::discountImpl(Time t) const {
    calculate();
    return base_curve::discountImpl(t);
}

template <template <class> class C, template <class> class I,
          template <class, class> class B, class T>
inline void PiecewiseYieldCurve<C, I, B, T>::performCalculations() const {
    // just delegate to the bootstrapper
    bootstrap_.calculate();
}
}

#endif
