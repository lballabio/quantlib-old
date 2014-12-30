/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyritht (C) 2015 Peter Caspers

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

/*! \file brent.hpp
    \brief Brent 1-D solver
*/

#ifndef quantlib_solver1d_brent_h
#define quantlib_solver1d_brent_h

#include <ql/math/solver1d.hpp>

namespace QuantLib {

//! %Brent 1-D solver
/*! \test the correctness of the returned values is tested by
          checking them against known good results.
*/

using std::abs;

template <class T = Real> class Brent_t : public Solver1D<Brent_t<T>, T> {
  public:
    template <class F> T solveImpl(const F &f, T xAccuracy) const {

        /* The implementation of the algorithm was inspired by
           Press, Teukolsky, Vetterling, and Flannery,
           "Numerical Recipes in C", 2nd edition, Cambridge
           University Press
        */

        T min1, min2;
        T froot, p, q, r, s, xAcc1, xMid;

        // we want to start with root_ (which equals the guess) on
        // one side of the bracket and both xMin_ and xMax_ on the
        // other.
        froot = f(this->root_);
        ++this->evaluationNumber_;
        if (froot * this->fxMin_ < 0) {
            this->xMax_ = this->xMin_;
            this->fxMax_ = this->fxMin_;
        } else {
            this->xMin_ = this->xMax_;
            this->fxMin_ = this->fxMax_;
        }
        T d = this->root_ - this->xMax_;
        T e = d;

        while (this->evaluationNumber_ <= this->maxEvaluations_) {
            if ((froot > 0.0 && this->fxMax_ > 0.0) ||
                (froot < 0.0 && this->fxMax_ < 0.0)) {

                // Rename xMin_, root_, xMax_ and adjust bounds
                this->xMax_ = this->xMin_;
                this->fxMax_ = this->fxMin_;
                e = d = this->root_ - this->xMin_;
            }
            if (abs(this->fxMax_) < abs(froot)) {
                this->xMin_ = this->root_;
                this->root_ = this->xMax_;
                this->xMax_ = this->xMin_;
                this->fxMin_ = froot;
                froot = this->fxMax_;
                this->fxMax_ = this->fxMin_;
            }
            // Convergence check
            xAcc1 = 2.0 * QL_EPSILON * abs(this->root_) + 0.5 * xAccuracy;
            xMid = (this->xMax_ - this->root_) / 2.0;
            if (abs(xMid) <= xAcc1 || (close<T>(froot, 0.0))) {
                f(this->root_);
                ++this->evaluationNumber_;
                return this->root_;
            }
            if (abs(e) >= xAcc1 && abs(this->fxMin_) > abs(froot)) {

                // Attempt inverse quadratic interpolation
                s = froot / this->fxMin_;
                if (close(this->xMin_, this->xMax_)) {
                    p = 2.0 * xMid * s;
                    q = 1.0 - s;
                } else {
                    q = this->fxMin_ / this->fxMax_;
                    r = froot / this->fxMax_;
                    p = s * (2.0 * xMid * q * (q - r) -
                             (this->root_ - this->xMin_) * (r - 1.0));
                    q = (q - 1.0) * (r - 1.0) * (s - 1.0);
                }
                if (p > 0.0)
                    q = -q; // Check whether in bounds
                p = abs(p);
                min1 = 3.0 * xMid * q - abs(xAcc1 * q);
                min2 = abs(e * q);
                if (2.0 * p < (min1 < min2 ? min1 : min2)) {
                    e = d; // Accept interpolation
                    d = p / q;
                } else {
                    d = xMid; // Interpolation failed, use bisection
                    e = d;
                }
            } else {
                // Bounds decreasing too slowly, use bisection
                d = xMid;
                e = d;
            }
            this->xMin_ = this->root_;
            this->fxMin_ = froot;
            if (abs(d) > xAcc1)
                this->root_ += d;
            else
                this->root_ += sign(xAcc1, xMid);
            froot = f(this->root_);
            ++this->evaluationNumber_;
        }
        QL_FAIL("maximum number of function evaluations ("
                << this->maxEvaluations_ << ") exceeded");
    }

  private:
    T sign(T a, T b) const { return b >= 0.0 ? abs(a) : -abs(a); }
};

typedef Brent_t<Real> Brent;
}

#endif
