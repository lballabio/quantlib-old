/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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

/*! \file interpolation.hpp
    \brief base class for 1-D interpolations
*/

#ifndef quantlib_interpolation_hpp
#define quantlib_interpolation_hpp

#include <ql/math/interpolations/extrapolation.hpp>
#include <ql/math/comparison.hpp>
#include <ql/errors.hpp>
#include <vector>

namespace QuantLib {

    //! base class for 1-D interpolations.
    /*! Classes derived from this class will provide interpolated
        values from two sequences of equal length, representing
        discretized values of a variable and a function of the former,
        respectively.
    */
    template <class T = Real>
    class Interpolation_t : public Extrapolator {
      protected:
        //! abstract base class for interpolation implementations
        class Impl {
          public:
            virtual ~Impl() {}
            virtual void update() = 0;
            virtual T xMin() const = 0;
            virtual T xMax() const = 0;
            virtual std::vector<T> xValues() const = 0;
            virtual std::vector<T> yValues() const = 0;
            virtual bool isInRange(T) const = 0;
            virtual Real value(T) const = 0;
            virtual Real primitive(T) const = 0;
            virtual Real derivative(T) const = 0;
            virtual Real secondDerivative(T) const = 0;
        };
        boost::shared_ptr<Impl> impl_;
      public:
        typedef T argument_type;
        typedef T result_type;
        //! basic template implementation
        template <class I1, class I2>
        class templateImpl : public Impl {
          public:
            templateImpl(const I1& xBegin, const I1& xEnd, const I2& yBegin)
            : xBegin_(xBegin), xEnd_(xEnd), yBegin_(yBegin) {
                QL_REQUIRE(static_cast<int>(xEnd_-xBegin_) >= 2,
                           "not enough points to interpolate: at least 2 "
                           "required, " << static_cast<int>(xEnd_-xBegin_)<< " provided");
            }
            T xMin() const {
                return *xBegin_;
            }
            T xMax() const {
                return *(xEnd_-1);
            }
            std::vector<T> xValues() const {
                return std::vector<T>(xBegin_,xEnd_);
            }
            std::vector<T> yValues() const {
                return std::vector<T>(yBegin_,yBegin_+(xEnd_-xBegin_));
            }
            bool isInRange(T x) const {
                #if defined(QL_EXTRA_SAFETY_CHECKS)
                for (I1 i=xBegin_, j=xBegin_+1; j!=xEnd_; ++i, ++j)
                    QL_REQUIRE(*j > *i, "unsorted x values");
                #endif
                T x1 = xMin(), x2 = xMax();
                return (x >= x1 && x <= x2) || close(x,x1) || close(x,x2);
            }
          protected:
            Size locate(Real x) const {
                #if defined(QL_EXTRA_SAFETY_CHECKS)
                for (I1 i=xBegin_, j=xBegin_+1; j!=xEnd_; ++i, ++j)
                    QL_REQUIRE(*j > *i, "unsorted x values");
                #endif
                if (x < *xBegin_)
                    return 0;
                else if (x > *(xEnd_-1))
                    return xEnd_-xBegin_-2;
                else
                    return std::upper_bound(xBegin_,xEnd_-1,x)-xBegin_-1;
            }
            I1 xBegin_, xEnd_;
            I2 yBegin_;
        };
      public:
        Interpolation_t() {}
        virtual ~Interpolation_t() {}
        bool empty() const { return !impl_; }
        T operator()(T x, bool allowExtrapolation = false) const {
            checkRange(x,allowExtrapolation);
            return impl_->value(x);
        }
        T primitive(T x, bool allowExtrapolation = false) const {
            checkRange(x,allowExtrapolation);
            return impl_->primitive(x);
        }
        T derivative(T x, bool allowExtrapolation = false) const {
            checkRange(x,allowExtrapolation);
            return impl_->derivative(x);
        }
        T secondDerivative(T x, bool allowExtrapolation = false) const {
            checkRange(x,allowExtrapolation);
            return impl_->secondDerivative(x);
        }
        T xMin() const {
            return impl_->xMin();
        }
        T xMax() const {
            return impl_->xMax();
        }
        bool isInRange(Real x) const {
            return impl_->isInRange(x);
        }
        void update() {
            impl_->update();
        }
      protected:
        void checkRange(T x, bool extrapolate) const {
            QL_REQUIRE(extrapolate || allowsExtrapolation() ||
                       impl_->isInRange(x),
                       "interpolation range is ["
                       << impl_->xMin() << ", " << impl_->xMax()
                       << "]: extrapolation at " << x << " not allowed");
        }
    };

    typedef Interpolation_t<Real> Interpolation;

}

#endif
