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

#include <ql/experimental/models/quadraticlfm.hpp>

#include <algorithm>

namespace QuantLib {

QuadraticLfm::QuadraticLfm(std::vector<Real> &rateTimes,
                           std::vector<Real> &initialForwards,
                           std::vector<std::vector<std::vector<Real> > > &sigma,
                           std::vector<std::vector<Real> > &b,
                           std::vector<std::vector<Real> > &c)
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
    QL_REQUIRE(N_ - 1 >= m && m > n && n >= 0,
               "for a swap rate 0 <= n (" << n << ") < m (" << m << ") <= N-1 ("
                                          << (N_ - 1) << ") must hold");
    QL_REQUIRE((m - n) % step == 0,
               "m (" << m << ") minus n (" << n << ") = " << (m - n)
                     << " must be divisible by step (" << step << ")");
    Real annuity = 0.0;
    for (Size i = n + step; i <= m; i += step)
        annuity += P(n, i) * (rateTimes_[i] - rateTimes_[i - step]);
    return (1.0 - P(n, m)) / annuity;
}

Real QuadraticLfm::dSdL(const Size n, const Size m, const Size step,
                        const Size i, const Real h) {
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

    int ind = q(t);
    Real s0 = S(n, m, step);

    // set up vectors

    std::vector<Real> qi(m - n, 0.0);
    std::vector<std::vector<Real> > sij, intsi;
    std::vector<std::vector<std::vector<Real> > > intsisj;
    for (Size j = n; j < m; ++j) {
        std::vector<Real> sijtmp(m - n, 0.0);
        sij.push_back(sijtmp);
    }
    for (Size k = 0; k < K_; ++k) {
        std::vector<Real> intsitmp(m - n, 0.0);
        intsi.push_back(intsitmp);
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
            intsi[k][i - n] =
                ind >= 0 ? sigma_[k][i - n][ind] * (t - rateTimes_[ind]) : 0.0;
            for (int ii = 0; ii < ind; ++ii) {
                intsi[k][i - n] += sigma_[k][i - n][ii] *
                                   (rateTimes_[ii + 1] - rateTimes_[ii]);
            }
        }
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
            denom += initialForwards_[i] * initialForwards_[j] * qi[i - n] *
                     qi[j - n];
            Real tmp = 0.0;
            for (Size k = 0; k < K_; ++k) {
                tmp += intsisj[k][i - n][j - n];
            }
        }
    }

    for (Size i = n; i < m; ++i) {
        Real tmp2 = 0.0;
        for (Size k = 0; k < K_; ++k) {
            Real tmp1 = 0.0;
            for (Size j = n; j < m; ++j) {
                tmp1 += initialForwards_[j] * qi[j - n] * intsi[k][j - n];
            }
            tmp2 += tmp1 * intsi[k][i - n];
        }
        tmp2 *= initialForwards_[i];
        Ei[i - n] = tmp2 / denom * (s - s0);
    }

    // eta squared

    Real eta2 = 0.0;
    for (Size i = n; i < m; ++i) {
        for (Size j = n; j < m; ++j) {
            eta2 += qi[i - n] * qi[j - n] * sij[i - n][j - n] *
                    (initialForwards_[i] * initialForwards_[j] +
                     b_[i][ind] * Ei[i - n] + b_[j][ind] * Ei[j - n] +
                     c_[i][ind] * Ei[i - n] * Ei[i - n] +
                     c_[j][ind] * Ei[j - n] * Ei[j - n]);
        }
    }

    // through the approximation for eta2 it may get negative (?)
    return std::sqrt(std::max(eta2,0.0));

} // eta

} // namespace QuantLib
