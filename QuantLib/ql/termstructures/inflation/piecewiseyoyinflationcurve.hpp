/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Chris Kenyon
 Copyright (C) 2007, 2008 StatPro Italia srl
 Copyright (C) 2011 Ferdinando Ametrano
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

/*! \file piecewiseyoyinflationcurve.hpp
    \brief Piecewise year-on-year inflation term structure
*/

#ifndef quantlib_piecewise_yoy_inflation_curve_hpp
#define quantlib_piecewise_yoy_inflation_curve_hpp

#include <ql/termstructures/iterativebootstrap.hpp>
#include <ql/termstructures/inflation/inflationtraits.hpp>
#include <ql/patterns/lazyobject.hpp>

namespace QuantLib {

//! Piecewise year-on-year inflation term structure
template <template <class> class Interpolator,
          template <class, class> class Bootstrap = IterativeBootstrap,
          template <class> class Traits = YoYInflationTraits, class T = Real>
class PiecewiseYoYInflationCurve
    : public InterpolatedYoYInflationCurve_t<Interpolator,T>,
      public LazyObject {
  private:
    typedef InterpolatedYoYInflationCurve_t<Interpolator, T> base_curve;
    typedef PiecewiseYoYInflationCurve<Interpolator, Bootstrap, Traits, T>
        this_curve;

  public:
    typedef Traits<T> traits_type;
    typedef Interpolator<T> interpolator_type;
    //! \name Constructors
    //@{
    PiecewiseYoYInflationCurve(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, const Period &lag, Frequency frequency,
        bool indexIsInterpolated, Rate baseYoYRate,
        const Handle<YieldTermStructure_t<T> > &nominalTS,
        const std::vector<boost::shared_ptr<typename Traits<T>::helper> > &
            instruments,
        Real accuracy = 1.0e-12, const Interpolator<T> &i = Interpolator<T>())
        : base_curve(referenceDate, calendar, dayCounter, baseYoYRate, lag,
                     frequency, indexIsInterpolated, nominalTS, i),
          instruments_(instruments), accuracy_(accuracy) {

        bootstrap_.setup(this);
    }
    //@}
    //! \name Inflation interface
    //@{
    Date baseDate() const;
    Date maxDate() const;
    //@
    //! \name Inspectors
    //@{
    const std::vector<Time> &times() const;
    const std::vector<Date> &dates() const;
    const std::vector<T> &data() const;
    std::vector<std::pair<Date, T> > nodes() const;
    //@}
    //! \name Observer interface
    //@{
    void update();
    //@}
  private:
    // methods
    void performCalculations() const;
    // data members
    std::vector<boost::shared_ptr<typename Traits<T>::helper> > instruments_;
    T accuracy_;

#if !defined(QL_PATCH_MSVC90)
    // this avoids defining another name...
    friend class Bootstrap<this_curve, T>;
#else
    // ...but VC++ 9 cannot digest it in some contexts.
    typedef typename Bootstrap<this_curve, T> bootstrapper;
    friend class bootstrapper;
#endif
    friend class BootstrapError<this_curve, T>;
    Bootstrap<this_curve, T> bootstrap_;
};

// inline and template definitions

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
inline Date PiecewiseYoYInflationCurve<I, B, U, T>::baseDate() const {
    this->calculate();
    return base_curve::baseDate();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
inline Date PiecewiseYoYInflationCurve<I, B, U, T>::maxDate() const {
    this->calculate();
    return base_curve::maxDate();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
const std::vector<Time> &PiecewiseYoYInflationCurve<I, B, U, T>::times() const {
    calculate();
    return base_curve::times();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
const std::vector<Date> &PiecewiseYoYInflationCurve<I, B, U, T>::dates() const {
    calculate();
    return base_curve::dates();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
const std::vector<T> &PiecewiseYoYInflationCurve<I, B, U, T>::data() const {
    calculate();
    return base_curve::data();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
std::vector<std::pair<Date, T> >
PiecewiseYoYInflationCurve<I, B, U, T>::nodes() const {
    calculate();
    return base_curve::nodes();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
void PiecewiseYoYInflationCurve<I, B, U, T>::performCalculations() const {
    bootstrap_.calculate();
}

template <template <class> class I, template <class, class> class B,
          template <class> class U, class T>
void PiecewiseYoYInflationCurve<I, B, U, T>::update() {
    base_curve::update();
    LazyObject::update();
}
}

#endif
