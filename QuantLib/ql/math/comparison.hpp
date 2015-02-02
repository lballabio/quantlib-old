/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl

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

/*! \file comparison.hpp
    \brief floating-point comparisons
*/

#ifndef quantlib_comparison_hpp
#define quantlib_comparison_hpp

#include <ql/types.hpp>
#include <boost/shared_ptr.hpp>

namespace QuantLib {

    using std::abs;

    /*! Follows somewhat the advice of Knuth on checking for floating-point
        equality. The closeness relationship is:
        \f[
        \mathrm{close}(x,y,n) \equiv |x-y| \leq \varepsilon |x|
                              \wedge |x-y| \leq \varepsilon |y|
        \f]
        where \f$ \varepsilon \f$ is \f$ n \f$ times the machine accuracy;
        \f$ n \f$ equals 42 if not given.

        T must be consistent with machine accuracy QL_EPSILON to make sense
    */
    template<class T> bool close(T x, T y);
    template<class T> bool close(T x, T y, Size n);

    /*! Follows somewhat the advice of Knuth on checking for floating-point
        equality. The closeness relationship is:
        \f[
        \mathrm{close}(x,y,n) \equiv |x-y| \leq \varepsilon |x|
                                \vee |x-y| \leq \varepsilon |y|
        \f]
        where \f$ \varepsilon \f$ is \f$ n \f$ times the machine accuracy;
        \f$ n \f$ equals 42 if not given.
    */
    template<class T> bool close_enough(T x, T y);
    template<class T> bool close_enough(T x, T y, Size n);


    // inline definitions

    template<class T> inline bool close(T x, T y) {
        return close<T>(x,y,42);
    }

    template<class T> inline bool close(T x, T y, Size n) {
        // Deals with +infinity and -infinity representations etc.
        if (x == y)
            return true;

        T diff = abs(x-y), tolerance = n * QL_EPSILON;

        if (x * y == 0.0) // x or y = 0.0
            return diff < (tolerance * tolerance);

        return diff <= tolerance*abs(x) &&
               diff <= tolerance*abs(y);
    }

    template<class T> inline bool close_enough(T x, T y) {
        return close_enough<T>(x,y,42);
    }

    template<class T> inline bool close_enough(T x, T y, Size n) {
        // Deals with +infinity and -infinity representations etc.
        if (x == y)
            return true;

        T diff = abs(x-y), tolerance = n * QL_EPSILON;

        if (x * y == 0.0) // x or y = 0.0
            return diff < (tolerance * tolerance);

        return diff <= tolerance*std::fabs(x) ||
               diff <= tolerance*std::fabs(y);
    }



    //! compare two objects by date
    /*! There is no generic implementation of this struct.
        Template specializations will have to be defined for
        each needed type (see CashFlow for an example.)
    */
    template <class T> struct earlier_than;

    /* partial specialization for shared pointers, forwarding to their
       pointees. */
    template <class T>
    struct earlier_than<boost::shared_ptr<T> >
        : std::binary_function<boost::shared_ptr<T>,
                               boost::shared_ptr<T>,
                               bool> {
        bool operator()(const boost::shared_ptr<T>& x,
                        const boost::shared_ptr<T>& y) {
            return earlier_than<T>()(*x,*y);
        }
    };

}


#endif
