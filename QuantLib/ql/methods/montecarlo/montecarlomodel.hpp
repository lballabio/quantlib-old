/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl
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

/*! \file montecarlomodel.hpp
    \brief General-purpose Monte Carlo model
*/

#ifndef quantlib_montecarlo_model_hpp
#define quantlib_montecarlo_model_hpp

#include <ql/methods/montecarlo/mctraits.hpp>
#include <ql/math/statistics/statistics.hpp>
#include <boost/shared_ptr.hpp>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace QuantLib {

    //! General-purpose Monte Carlo model for path samples
    /*! The template arguments of this class correspond to available
        policies for the particular model to be instantiated---i.e.,
        whether it is single- or multi-asset, or whether it should use
        pseudo-random or low-discrepancy numbers for path
        generation. Such decisions are grouped in trait classes so as
        to be orthogonal---see mctraits.hpp for examples.

        The constructor accepts two safe references, i.e. two smart
        pointers, one to a path generator and the other to a path
        pricer.  In case of control variate technique the user should
        provide the additional control option, namely the option path
        pricer and the option value.

        When using multithreading (by enabling OpenMP *and* using a
        multithreaded RNG) it must be ensured that both the path pricer
        and the process (used for path generation) are implemented in a
        thread safe way (w.r.t. omp parallelization).

        \ingroup mcarlo
    */
    template <template <class> class MC, class RNG, class S = Statistics>
    class MonteCarloModel {
      public:
        typedef MC<RNG> mc_traits;
        typedef RNG rng_traits;
        typedef typename MC<RNG>::path_generator_type path_generator_type;
        typedef typename MC<RNG>::path_pricer_type path_pricer_type;
        typedef typename path_generator_type::sample_type sample_type;
        typedef typename path_pricer_type::result_type result_type;
        typedef S stats_type;
        // constructor
        MonteCarloModel(
                  const boost::shared_ptr<path_generator_type>& pathGenerator,
                  const boost::shared_ptr<path_pricer_type>& pathPricer,
                  const stats_type& sampleAccumulator,
                  bool antitheticVariate,
                  const boost::shared_ptr<path_pricer_type>& cvPathPricer
                        = boost::shared_ptr<path_pricer_type>(),
                  result_type cvOptionValue = result_type(),
                  const boost::shared_ptr<path_generator_type>& cvPathGenerator
                        = boost::shared_ptr<path_generator_type>())
        : pathGenerator_(pathGenerator), pathPricer_(pathPricer),
          sampleAccumulator_(sampleAccumulator),
          isAntitheticVariate_(antitheticVariate),
          cvPathPricer_(cvPathPricer), cvOptionValue_(cvOptionValue),
          isControlVariate_(cvPathPricer_!=NULL),
          cvPathGenerator_(cvPathGenerator) {}
        void addSamples(Size samples);
        const stats_type& sampleAccumulator(void) const;
      private:
        const boost::shared_ptr<path_generator_type> pathGenerator_;
        const boost::shared_ptr<path_pricer_type> pathPricer_;
        stats_type sampleAccumulator_;
        const bool isAntitheticVariate_;
        const boost::shared_ptr<path_pricer_type> cvPathPricer_;
        const result_type cvOptionValue_;
        const bool isControlVariate_;
        const boost::shared_ptr<path_generator_type> cvPathGenerator_;
    };

    // inline definitions
    template <template <class> class MC, class RNG, class S>
    inline void MonteCarloModel<MC,RNG,S>::addSamples(Size samples) {

        unsigned int threadId = 0;

#pragma omp parallel for firstprivate(threadId) schedule(static) if(RNG::maxNumberOfThreads > 1)
        for(Size j = 1; j <= samples; j++) {

#ifdef _OPENMP
            if (RNG::maxNumberOfThreads > 1)
                threadId = omp_get_thread_num();
#endif

            sample_type path = pathGenerator_->next(threadId);

            result_type price = (*pathPricer_)(path.value);

            if (isControlVariate_) {
                if (!cvPathGenerator_) {
                    price += cvOptionValue_-(*cvPathPricer_)(path.value);
                }
                else {
                    sample_type cvPath = cvPathGenerator_->next(threadId);
                    price += cvOptionValue_-(*cvPathPricer_)(cvPath.value);
                }
            }

            if (isAntitheticVariate_) {
                path = pathGenerator_->antithetic(threadId);
                result_type price2 = (*pathPricer_)(path.value);
                if (isControlVariate_) {
                    if (!cvPathGenerator_)
                        price2 += cvOptionValue_-(*cvPathPricer_)(path.value);
                    else {
                        sample_type cvPath = cvPathGenerator_->antithetic(threadId);
                        price2 += cvOptionValue_-(*cvPathPricer_)(cvPath.value);
                    }
                }
#pragma omp critical
                sampleAccumulator_.add((price+price2)/2.0, path.weight);
            } else {
#pragma omp critical
                sampleAccumulator_.add(price, path.weight);
            }
        } // for samples
    }

    template <template <class> class MC, class RNG, class S>
    inline const typename MonteCarloModel<MC,RNG,S>::stats_type&
    MonteCarloModel<MC,RNG,S>::sampleAccumulator() const {
        return sampleAccumulator_;
    }

}


#endif
