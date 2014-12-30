/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
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

/*! \file finitedifferencenewtonsafe.hpp
    \brief Safe (bracketed) Newton 1-D solver with finite difference derivatives
*/

#ifndef quantlib_solver1d_finitedifferencenewtonsafe_h
#define quantlib_solver1d_finitedifferencenewtonsafe_h

#include <ql/math/solver1d.hpp>

namespace QuantLib {

//! safe %Newton 1-D solver with finite difference derivatives
/*!
    \test the correctness of the returned values is tested by
          checking them against known good results.
*/

using std::abs;

template <class T>
class FiniteDifferenceNewtonSafe_t
    : public Solver1D<FiniteDifferenceNewtonSafe_t<T>, T> {
  public:
    template <class F>
    T solveImpl(const F &f, T xAccuracy) const {

        // Orient the search so that f(xl) < 0
        T xh, xl;
        if (this->fxMin_ < 0.0) {
            xl = this->xMin_;
            xh = this->xMax_;
        } else {
            xh = this->xMin_;
            xl = this->xMax_;
        }

        T froot = f(this->root_);
        ++this->evaluationNumber_;
        // first order finite difference derivative
        T dfroot = this->xMax_ - this->root_ < this->root_ - this->xMin_
                       ? (this->fxMax_ - froot) / (this->xMax_ - this->root_)
                       : (this->fxMin_ - froot) / (this->xMin_ - this->root_);

        // xMax_-xMin_>0 is verified in the constructor
        T dx = this->xMax_ - this->xMin_;
        while (this->evaluationNumber_ <= this->maxEvaluations_) {
            T frootold = froot;
            T rootold = this->root_;
            T dxold = dx;
            // Bisect if (out of range || not decreasing fast enough)
            if ((((this->root_ - xh) * dfroot - froot) *
                     ((this->root_ - xl) * dfroot - froot) >
                 0.0) ||
                (std::fabs(2.0 * froot) > std::fabs(dxold * dfroot))) {
                dx = (xh - xl) / 2.0;
                this->root_ = xl + dx;
                // if the root estimate just computed is close to the
                // previous one, we should calculate dfroot at root and
                // xh rather than root and rootold (xl instead of xh would
                // be just as good)
                if (close(this->root_, rootold, 2500)) {
                    rootold = xh;
                    frootold = f(xh);
                }
            } else { // Newton
                dx = froot / dfroot;
                this->root_ -= dx;
            }

            // Convergence criterion
            if (abs(dx) < xAccuracy)
                return this->root_;

            froot = f(this->root_);
            ++this->evaluationNumber_;
            dfroot = (frootold - froot) / (rootold - this->root_);

            if (froot < 0.0)
                xl = this->root_;
            else
                xh = this->root_;
        }

        QL_FAIL("maximum number of function evaluations ("
                << this->maxEvaluations_ << ") exceeded");
    }
};

typedef FiniteDifferenceNewtonSafe_t<Real> FiniteDifferenceNewtonSafe;
}

#endif
