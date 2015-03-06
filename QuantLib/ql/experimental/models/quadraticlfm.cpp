/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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

#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/methods/finitedifferences/meshers/concentrating1dmesher.hpp>
#include <ql/methods/finitedifferences/meshers/fdmmeshercomposite.hpp>
#include <ql/methods/finitedifferences/utilities/fdmdirichletboundary.hpp>
#include <ql/methods/finitedifferences/utilities/fdmboundaryconditionset.hpp>
#include <ql/methods/finitedifferences/boundarycondition.hpp>
#include <ql/methods/finitedifferences/solvers/fdmbackwardsolver.hpp>
#include <ql/experimental/models/quadraticlfm.hpp>
#include <ql/experimental/finitedifferences/fdmdupire1dop.hpp>

#include <boost/function.hpp>

#include <algorithm>

namespace QuantLib {

QuadraticLfm::QuadraticLfm(
    const std::vector<Real> &rateTimes,
    const std::vector<Real> &initialForwards,
    const std::vector<std::vector<std::vector<Real> > > &sigma,
    const std::vector<std::vector<Real> > &b,
    const std::vector<std::vector<Real> > &c)
    : rateTimes_(rateTimes), initialForwards_(initialForwards), sigma_(sigma),
      b_(b), c_(c) {
    N_ = rateTimes.size();
    QL_REQUIRE(N_ - 1 == initialForwards_.size(),
               "rateTimes size ("
                   << N_ << ") minus 1 must be equal to number of forwards ("
                   << initialForwards_.size() << ")");
    K_ = sigma_.size();
    QL_REQUIRE(K_ >= 1, "number of factors ("
                            << K_ << ") must be greater or equal to one");
    for (Size k = 0; k < K_; ++k) {
        QL_REQUIRE(N_ - 1 == sigma_[k].size(),
                   "for factor k ("
                       << k << ") the number of sigma functions ("
                       << sigma_[k].size()
                       << ") must be equal to the number of forwards N-1 ("
                       << (N_ - 1) << ")");
        for (Size i = 0; i < N_ - 1; ++i) {
            QL_REQUIRE(N_ - 1 == sigma_[k][i].size(),
                       "for factor k (" << k << ") and Libor i (" << i
                                        << ") the piecewise sigma function "
                                           "must consist of N-1 (" << (N_ - 1)
                                        << ") values, but is ("
                                        << sigma_[k][i].size() << ")");
        }
    }
}

const void QuadraticLfm::checkSwapParameters(const Size n, const Size m,
                                             const Size step) {
    QL_REQUIRE(N_ - 1 >= m && m > n && n >= 0,
               "for a swap rate 0 <= n (" << n << ") < m (" << m << ") <= N-1 ("
                                          << (N_ - 1) << ") must hold");
    QL_REQUIRE((m - n) % step == 0,
               "m (" << m << ") minus n (" << n << ") = " << (m - n)
                     << " must be divisible by step (" << step << ")");
    return;
}

int QuadraticLfm::q(const Real t) {
    QL_REQUIRE(t >= 0.0 && t < rateTimes_[N_ - 2],
               "at time " << t << " all forwards are dead");
    return static_cast<int>(
        std::upper_bound(rateTimes_.begin(), rateTimes_.end(), t) -
        rateTimes_.begin());
}

Real QuadraticLfm::P(const Size n, const Size m) {
    QL_REQUIRE(N_ - 1 >= m && m > n && n >= 0, "for a discount factor 0 <= n ("
                                                   << n << ") < m (" << m
                                                   << ") <= N-1 (" << (N_ - 1)
                                                   << ") must hold");
    Real tmp = 1.0;
    for (Size i = n; i < m; ++i)
        tmp *=
            1.0 /
            (1.0 + initialForwards_[i] * (rateTimes_[i + 1] - rateTimes_[i]));
    return tmp;
}

Real QuadraticLfm::S(const Size n, const Size m, const Size step) {
    checkSwapParameters(n, m, step);
    Real annuity = 0.0;
    for (Size i = n + step; i <= m; i += step)
        annuity += P(n, i) * (rateTimes_[i] - rateTimes_[i - step]);
    return (1.0 - P(n, m)) / annuity;
}

Real QuadraticLfm::dSdL(const Size n, const Size m, const Size step,
                        const Size i, const Real h) {
    checkSwapParameters(n, m, step);
    QL_REQUIRE(N_ - 2 >= i && i >= 0, "for dSdL, 0 <= i (" << i << ") <= N-2 ("
                                                           << (N_ - 2)
                                                           << ") must hold");
    QL_REQUIRE(h > 0.0, "for dSdL h (" << h << ") must be positive");
    Real f = S(n, m, step);
    Real tmp = initialForwards_[i];
    initialForwards_[i] += h;
    Real fh = S(n, m, step);
    initialForwards_[i] = tmp;
    return (fh - f) / h;
}

Real QuadraticLfm::eta(const Size n, const Size m, const Size step,
                       const Real t, const Real s) {
    Array sVec(1, s);
    return eta(n, m, step, t, sVec)[0];
}

Disposable<Array> QuadraticLfm::eta(const Size n, const Size m, const Size step,
                                    const Real t, const Array &s) {

    checkSwapParameters(n, m, step);

    int ind = q(t);
    Real s0 = S(n, m, step);

    // set up vectors

    std::vector<Real> qi(m - n, 0.0);
    std::vector<std::vector<Real> > sij; //, intsi;
    std::vector<std::vector<std::vector<Real> > > intsisj;
    for (Size j = n; j < m; ++j) {
        std::vector<Real> sijtmp(m - n, 0.0);
        sij.push_back(sijtmp);
    }
    for (Size k = 0; k < K_; ++k) {
        std::vector<Real> intsitmp(m - n, 0.0);
        std::vector<std::vector<Real> > intsisjtmp;
        for (Size i = n; i < m; ++i) {
            std::vector<Real> intsisjtmp2(m - n, 0.0);
            intsisjtmp.push_back(intsisjtmp2);
        }
        intsisj.push_back(intsisjtmp);
    }

    // precompute results

    for (Size i = n; i < m; ++i) {
        qi[i - n] = dSdL(n, m, step, i);
        for (Size k = 0; k < K_; ++k) {
            for (Size j = n; j < m; ++j) {
                sij[i - n][j - n] +=
                    ind >= 0 ? sigma_[k][i - n][ind] * sigma_[k][j - n][ind]
                             : 0.0;
            }
        }
        for (Size j = n; j < m; ++j) {
            for (Size k = 0; k < K_; ++k) {
                intsisj[k][i - n][j - n] += sigma_[k][i - n][ind] *
                                            sigma_[k][j - n][ind] *
                                            (t - rateTimes_[ind]);
                for (int ii = 0; ii < ind; ++ii) {
                    intsisj[k][i - n][j - n] +=
                        sigma_[k][i - n][ii] * sigma_[k][j - n][ii] *
                        (rateTimes_[ii + 1] - rateTimes_[ii]);
                }
            }
        }
    }

    // E_i

    std::vector<Real> Ei(m - n, 0.0);

    Real denom = 0.0;
    for (Size i = n; i < m; ++i) {
        for (Size j = n; j < m; ++j) {
            Real tmp = 0.0;
            for (Size k = 0; k < K_; ++k) {
                tmp += intsisj[k][i - n][j - n];
            }
            denom += initialForwards_[i] * initialForwards_[j] * qi[i - n] *
                     qi[j - n] * tmp;
        }
    }

    for (Size i = n; i < m; ++i) {
        Real tmp1 = 0.0;
        for (Size k = 0; k < K_; ++k) {
            for (Size j = n; j < m; ++j) {
                tmp1 +=
                    initialForwards_[j] * qi[j - n] * intsisj[k][i - n][j - n];
            }
        }
        tmp1 *= initialForwards_[i];
        Ei[i - n] = tmp1 / denom;
    }

    // eta squared

    Array eta2(s.size(), 0.0);
    for (Size i = n; i < m; ++i) {
        for (Size j = n; j < m; ++j) {
            for (Size k = 0; k < s.size(); ++k) {
                eta2[k] += qi[i - n] * qi[j - n] * sij[i - n][j - n] *
                           (initialForwards_[i] * initialForwards_[j] +
                            b_[i][ind] * Ei[i - n] * initialForwards_[j] * (s[k] - s0) +
                            b_[j][ind] * Ei[j - n] * initialForwards_[i] * (s[k] - s0) +
                            (c_[i][ind] * Ei[i - n] * Ei[i - n] * initialForwards_[j] +
                             c_[j][ind] * Ei[j - n] * Ei[j - n] * initialForwards_[i]) *
                                (s[k] - s0) * (s[k] - s0));
            }
        }
    }

    for (Size k = 0; k < s.size(); ++k) {
        // through the approximation for eta2 it may get negative (?)
        eta2[k] = std::sqrt(std::max(eta2[k], 0.0));
    }

    return eta2;

} // eta

Disposable<std::vector<Real> >
QuadraticLfm::callPrices(const Size n, const Size m, const Size step,
                         const std::vector<Real> &strikes) {

    checkSwapParameters(n, m, step);

    // expiry time
    Real expiryTime = rateTimes_[n];

    // forward swap rate
    Real forward = S(n, m, step);

    // grid parameters (hardcoded here ... !)
    const Real start = std::min(0.00001, strikes.front() * 0.5);
    const Real end = std::max(0.10, strikes.back() * 1.5);
    const Size size = 500;
    const Real density = 0.1;
    const Size steps = static_cast<Size>(std::ceil(expiryTime * 24));
    const Size dampingSteps = 5;

    // Layout
    std::vector<Size> dim(1, size);
    const boost::shared_ptr<FdmLinearOpLayout> layout(
        new FdmLinearOpLayout(dim));

    // Mesher
    const boost::shared_ptr<Fdm1dMesher> m1(new Concentrating1dMesher(
        start, end, size, std::pair<Real, Real>(forward, density), true));
    const std::vector<boost::shared_ptr<Fdm1dMesher> > meshers(1, m1);
    const boost::shared_ptr<FdmMesher> mesher(
        new FdmMesherComposite(layout, meshers));

    // Boundary conditions
    FdmBoundaryConditionSet boundaries;

    // initial values
    Array rhs(mesher->layout()->size());
    for (FdmLinearOpIterator iter = layout->begin(); iter != layout->end();
         ++iter) {
        Real k = mesher->location(iter, 0);
        rhs[iter.index()] = std::max(forward - k, 0.0);
    }

    // strike grid
    const Array strikeGrid = mesher->locations(0);

    // local vol function
    LocalVolHelper localVol(this, n, m, step, strikeGrid);

    // solver
    boost::shared_ptr<FdmDupire1dOp> map(new FdmDupire1dOp(mesher, localVol));
    FdmBackwardSolver solver(map, boundaries,
                             boost::shared_ptr<FdmStepConditionComposite>(),
                             FdmSchemeDesc::Douglas());
    solver.rollback(rhs, expiryTime, 0.0, steps, dampingSteps);

    // interpolate solution
    boost::shared_ptr<Interpolation> solution(new CubicInterpolation(
        strikeGrid.begin(), strikeGrid.end(), rhs.begin(),
        CubicInterpolation::Spline, true, CubicInterpolation::SecondDerivative,
        0.0, CubicInterpolation::SecondDerivative, 0.0));
    // boost::shared_ptr<Interpolation> solution(new
    // LinearInterpolation(k.begin(),k.end(),rhs.begin()));
    solution->disableExtrapolation();
    std::vector<Real> result(strikes.size());
    std::transform(strikes.begin(), strikes.end(), result.begin(), *solution);
    return result;

} // callPrices

} // namespace QuantLib
