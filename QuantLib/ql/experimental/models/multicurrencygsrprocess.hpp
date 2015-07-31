/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
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

/*! \file multicurrencygsrprocess.hpp

    \brief multicurrency gsr process (risk neutral, domestic measure)
    The gsr input processes are in their original currency's measure,
    the drift adjustment for the domestic measure is only applied in
    the composite process here. The first gsr process must be the one
    in the reference (domestic) currency.

    \warning changes in the correlation matrix will not be recognized
    due to StochasticProcessArray where at construction time the pseudo
    square root of this matrix is computed and not updated afterwards.

    !!! this class does not work !!!
*/

#ifndef quantlib_multicurrency_gsr_process_hpp
#define quantlib_multicurrency_gsr_process_hpp

#include <ql/processes/stochasticprocessarray.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/experimental/models/gsrprocess_riskneutral.hpp>

namespace QuantLib {

class MultiCurrencyGsrProcess : public StochasticProcessArray {
  public:
    MultiCurrencyGsrProcess(
        const std::vector<boost::shared_ptr<StochasticProcess1D> >
            &fx_gsr_processes,
        const Matrix &correlation);
    Disposable<Array> drift(Time t, const Array &x) const;
    Disposable<Array> expectation(Time t0, const Array &x0, Time dt) const;
    Disposable<Array> evolve(Time t0, const Array &x0, Time dt,
                             const Array &dw) const;

  private:
    Matrix correlation_;
    Size n_;
};

} // namesapce QuantLib

#endif
