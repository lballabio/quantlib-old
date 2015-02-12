/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

/*! \file newtonsafe.hpp
    \brief Safe (bracketed) Newton 1-D solver
*/

#ifndef quantlib_solver1d_newtonsafe_h
#define quantlib_solver1d_newtonsafe_h

#include <ql/math/solver1d.hpp>

namespace QuantLib {

    //! safe %Newton 1-D solver
    /*! \note This solver requires that the passed function object
              implement a method <tt>Real derivative(Real)</tt>.

        \test the correctness of the returned values is tested by
              checking them against known good results.
    */

    template<class T>
    class NewtonSafe_t : public Solver1D<NewtonSafe_t<T>,T> {
      public:
        template <class F>
        T solveImpl(const F& f,
                       T xAccuracy) const {

            /* The implementation of the algorithm was inspired by
               Press, Teukolsky, Vetterling, and Flannery,
               "Numerical Recipes in C", 2nd edition,
               Cambridge University Press
            */

            T froot, dfroot, dx, dxold;
            T xh, xl;

            // Orient the search so that f(xl) < 0
            if (this->fxMin_ < 0.0) {
                xl = this->xMin_;
                xh = this->xMax_;
            } else {
                xh = this->xMin_;
                xl = this->xMax_;
            }

            // the "stepsize before last"
            dxold = this->xMax_-this->xMin_;
            // it was dxold=std::fabs(xMax_-xMin_); in Numerical Recipes
            // here (xMax_-xMin_ > 0) is verified in the constructor

            // and the last step
            dx = dxold;

            froot = f(this->root_);
            dfroot = f.derivative(this->root_);
            QL_REQUIRE(dfroot != Null<Real>(),
                       "NewtonSafe requires function's derivative");
            ++this->evaluationNumber_;

            while (this->evaluationNumber_<=this->maxEvaluations_) {
                // Bisect if (out of range || not decreasing fast enough)
                if ((((this->root_-xh)*dfroot-froot)*
                     ((this->root_-xl)*dfroot-froot) > 0.0)
                    || (QLFCT::abs(2.0*froot) > QLFCT::abs(dxold*dfroot))) {

                    dxold = dx;
                    dx = (xh-xl)/2.0;
                    this->root_=xl+dx;
                } else {
                    dxold = dx;
                    dx = froot/dfroot;
                    this->root_ -= dx;
                }
                // Convergence criterion
                if (QLFCT::abs(dx) < xAccuracy) {
                    f(this->root_);
                    ++this->evaluationNumber_;
                    return this->root_;
                }
                froot = f(this->root_);
                dfroot = f.derivative(this->root_);
                ++this->evaluationNumber_;
                if (froot < 0.0)
                    xl=this->root_;
                else
                    xh=this->root_;
            }

            QL_FAIL("maximum number of function evaluations ("
                    << this->maxEvaluations_ << ") exceeded");
        }
    };

    typedef NewtonSafe_t<Real> NewtonSafe;

}

#endif
