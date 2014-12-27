/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2007 StatPro Italia srl
 Copyright (C) 2011 Ferdinando Ametrano
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

/*! \file bootstraptraits.hpp
    \brief bootstrap traits
*/

#ifndef ql_bootstrap_traits_hpp
#define ql_bootstrap_traits_hpp

#include <ql/termstructures/yield/discountcurve.hpp>
#include <ql/termstructures/yield/zerocurve.hpp>
#include <ql/termstructures/yield/forwardcurve.hpp>
#include <ql/termstructures/bootstraphelper.hpp>

namespace QuantLib {

using std::log;
using std::exp;

namespace detail {
const Real avgRate = 0.05;
const Real maxRate = 1.0;
}

//! Discount-curve traits
template <class T = Real> struct Discount_t {
    // interpolated curve type
    template <class Interpolator> struct curve {
        typedef InterpolatedDiscountCurve_t<Interpolator, T> type;
    };
    // helper class
    typedef BootstrapHelper<YieldTermStructure_t<T> > helper;

    // start of curve data
    static Date initialDate(const YieldTermStructure_t<T> *c) {
        return c->referenceDate();
    }
    // value at reference date
    static T initialValue(const YieldTermStructure_t<T> *) { return 1.0; }

    // guesses
    template <class C>
    static T guess(Size i, const C *c, bool validData,
                   Size) // firstAliveHelper
    {
        if (validData) // previous iteration value
            return c->data()[i];

        if (i == 1) // first pillar
            return 1.0 / (1.0 + detail::avgRate * c->times()[1]);

        // flat rate extrapolation
        Real r = -log(c->data()[i - 1]) / c->times()[i - 1];
        return exp(-r * c->times()[i]);
    }

    // possible constraints based on previous values
    template <class C>
    static T minValueAfter(Size i, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
        if (validData) {
#if defined(QL_NEGATIVE_RATES)
            return *(std::min_element(c->data().begin(), c->data().end())) /
                   2.0;
#else
            return c->data().back() / 2.0;
#endif
        }
        Time dt = c->times()[i] - c->times()[i - 1];
        return c->data()[i - 1] * exp(-detail::maxRate * dt);
    }
    template <class C>
    static T maxValueAfter(Size i, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
#if defined(QL_NEGATIVE_RATES)
        Time dt = c->times()[i] - c->times()[i - 1];
        return c->data()[i - 1] * exp(detail::maxRate * dt);
#else
        // discounts cannot increase
        return c->data()[i - 1];
#endif
    }

    // root-finding update
    static void updateGuess(std::vector<T> &data, T discount, Size i) {
        data[i] = discount;
    }
    // upper bound for convergence loop
    static Size maxIterations() { return 100; }
};

typedef Discount_t<Real> Discount;

//! Zero-curve traits
template <class T> struct ZeroYield_t {
    // interpolated curve type
    template <class Interpolator> struct curve {
        typedef InterpolatedZeroCurve_t<Interpolator, T> type;
    };
    // helper class
    typedef BootstrapHelper<YieldTermStructure_t<T> > helper;

    // start of curve data
    static Date initialDate(const YieldTermStructure_t<T> *c) {
        return c->referenceDate();
    }
    // dummy value at reference date
    static T initialValue(const YieldTermStructure_t<T> *) {
        return detail::avgRate;
    }

    // guesses
    template <class C>
    static T guess(Size i, const C *c, bool validData,
                   Size) // firstAliveHelper
    {
        if (validData) // previous iteration value
            return c->data()[i];

        if (i == 1) // first pillar
            return detail::avgRate;

        // extrapolate
        Date d = c->dates()[i];
        return c->zeroRate(d, c->dayCounter(), Continuous, Annual, true);
    }

    // possible constraints based on previous values
    template <class C>
    static T minValueAfter(Size, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
        if (validData) {
            Real r = *(std::min_element(c->data().begin(), c->data().end()));
#if defined(QL_NEGATIVE_RATES)
            return r < 0.0 ? r * 2.0 : r / 2.0;
#else
            return r / 2.0;
#endif
        }
#if defined(QL_NEGATIVE_RATES)
        // no constraints.
        // We choose as min a value very unlikely to be exceeded.
        return -detail::maxRate;
#else
        return QL_EPSILON;
#endif
    }
    template <class C>
    static T maxValueAfter(Size, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
        if (validData) {
            Real r = *(std::max_element(c->data().begin(), c->data().end()));
#if defined(QL_NEGATIVE_RATES)
            return r < 0.0 ? r / 2.0 : r * 2.0;
#else
            return r * 2.0;
#endif
        }
        // no constraints.
        // We choose as max a value very unlikely to be exceeded.
        return detail::maxRate;
    }

    // root-finding update
    static void updateGuess(std::vector<T> &data, T rate, Size i) {
        data[i] = rate;
        if (i == 1)
            data[0] = rate; // first point is updated as well
    }
    // upper bound for convergence loop
    static Size maxIterations() { return 100; }
};

typedef ZeroYield_t<Real> ZeroYield;

//! Forward-curve traits
template <class T> struct ForwardRate_t {
    // interpolated curve type
    template <class Interpolator> struct curve {
        typedef InterpolatedForwardCurve_t<Interpolator, T> type;
    };
    // helper class
    typedef BootstrapHelper<YieldTermStructure_t<T> > helper;

    // start of curve data
    static Date initialDate(const YieldTermStructure_t<T> *c) {
        return c->referenceDate();
    }
    // dummy value at reference date
    static T initialValue(const YieldTermStructure_t<T> *) {
        return detail::avgRate;
    }

    // guesses
    template <class C>
    static T guess(Size i, const C *c, bool validData,
                   Size) // firstAliveHelper
    {
        if (validData) // previous iteration value
            return c->data()[i];

        if (i == 1) // first pillar
            return detail::avgRate;

        // extrapolate
        Date d = c->dates()[i];
        return c->forwardRate(d, d, c->dayCounter(), Continuous, Annual, true);
    }

    // possible constraints based on previous values
    template <class C>
    static T minValueAfter(Size, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
        if (validData) {
            Real r = *(std::min_element(c->data().begin(), c->data().end()));
#if defined(QL_NEGATIVE_RATES)
            return r < 0.0 ? r * 2.0 : r / 2.0;
#else
            return r / 2.0;
#endif
        }
#if defined(QL_NEGATIVE_RATES)
        // no constraints.
        // We choose as min a value very unlikely to be exceeded.
        return -detail::maxRate;
#else
        return QL_EPSILON;
#endif
    }
    template <class C>
    static T maxValueAfter(Size, const C *c, bool validData,
                           Size) // firstAliveHelper
    {
        if (validData) {
            Real r = *(std::max_element(c->data().begin(), c->data().end()));
#if defined(QL_NEGATIVE_RATES)
            return r < 0.0 ? r / 2.0 : r * 2.0;
#else
            return r * 2.0;
#endif
        }
        // no constraints.
        // We choose as max a value very unlikely to be exceeded.
        return detail::maxRate;
    }

    // root-finding update
    static void updateGuess(std::vector<T> &data, Real forward, Size i) {
        data[i] = forward;
        if (i == 1)
            data[0] = forward; // first point is updated as well
    }
    // upper bound for convergence loop
    static Size maxIterations() { return 100; }
};

typedef ForwardRate_t<Real> ForwardRate;
}

#endif
