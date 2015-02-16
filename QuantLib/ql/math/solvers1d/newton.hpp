/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
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

/*! \file newton.hpp
    \brief Newton 1-D solver
*/

#ifndef quantlib_solver1d_newton_h
#define quantlib_solver1d_newton_h

#include <ql/math/solvers1d/newtonsafe.hpp>

namespace QuantLib {

//! %Newton 1-D solver
/*! \note This solver requires that the passed function object
          implement a method <tt>Real derivative(Real)</tt>.

    \test the correctness of the returned values is tested by
          checking them against known good results.
*/

template <class T> class Newton_t : public Solver1D<Newton_t<T>, T> {
  public:
    template <class F> T solveImpl(const F &f, T xAccuracy) const {

        /* The implementation of the algorithm was inspired by
           Press, Teukolsky, Vetterling, and Flannery,
           "Numerical Recipes in C", 2nd edition,
           Cambridge University Press
        */

        T froot, dfroot, dx;

        froot = f(this->root_);
        dfroot = f.derivative(this->root_);
        QL_REQUIRE(dfroot != Null<Real>(),
                   "Newton requires function's derivative");
        ++this->evaluationNumber_;

        while (this->evaluationNumber_ <= this->maxEvaluations_) {
            dx = froot / dfroot;
            this->root_ -= dx;
            // jumped out of brackets, switch to NewtonSafe
            if ((this->xMin_ - this->root_) * (this->root_ - this->xMax_) <
                0.0) {
                NewtonSafe_t<T> s;
                s.setMaxEvaluations(this->maxEvaluations_ -
                                    this->evaluationNumber_);
                return s.solve(f, xAccuracy, this->root_ + dx, this->xMin_,
                               this->xMax_);
            }
            if (QLFCT::abs(dx) < xAccuracy) {
                f(this->root_);
                ++this->evaluationNumber_;
                return this->root_;
            }
            froot = f(this->root_);
            dfroot = f.derivative(this->root_);
            ++this->evaluationNumber_;
        }

        QL_FAIL("maximum number of function evaluations ("
                << this->maxEvaluations_ << ") exceeded");
    }
};

typedef Newton_t<Real> Newton;
}

#endif
