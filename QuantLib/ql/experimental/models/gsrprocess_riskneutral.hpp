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

/*! \file gsrprocess_riskneutral.hpp
    \brief GSR model process with piecewise volatilities and mean reversions,
           the dynamic is expressed in the risk neutral (bank account) measure.
           See the sister class GsrProcess for more details and references.
*/

#ifndef quantlib_gsr_process_riskneutral_hpp
#define quantlib_gsr_process_riskneutral_hpp

#include <ql/processes/gsrprocesscore.hpp>
#include <ql/stochasticprocess.hpp>
#include <ql/time/daycounter.hpp>

namespace QuantLib {

//! GSR stochastic process (in risk neutral measure)
/*! \ingroup processes */
class GsrProcessRiskNeutral : public StochasticProcess1D {
  public:
    GsrProcessRiskNeutral(const Array &times, const Array &vols,
                          const Array &reversions, const Array &adjusters,
                          const Date &referenceDate = Null<Date>(),
                          const DayCounter &dc = DayCounter());
    //! \name StochasticProcess1D interface
    //@{
    Real x0() const;
    Real drift(Time t, Real x) const;
    Real diffusion(Time t, Real) const;
    Real expectation(Time t0, Real x0, Time dt) const;
    Real stdDeviation(Time t0, Real x0, Time dt) const;
    Real variance(Time t0, Real, Time dt) const;
    Real time(const Date &d) const;
    //@}
    //! \name ForwardMeasureProcess1D interface
    void setForwardMeasureTime(Time t);
    //@}
    //! additional inspectors
    const Array& times() const { return core_.times(); }
    const Array& volatility() const { return core_.vols(); }
    const Array& reversion() const { return core_.reversions(); }
    const Array& adjuster() const { return core_.adjusters(); }
    Date referenceDate() const { return referenceDate_; }
    DayCounter dayCounter() const { return dc_; }
    Real sigma(Time t) const;
    Real reversion(Time t) const;
    Real y(Time t) const;
    Real G(Time t, Time T, Real x) const;
    //! reset cache
    void flushCache() const;

  private:
    const detail::GsrProcessCore core_;
    Date referenceDate_;
    DayCounter dc_;
};

// inline definitions

inline void GsrProcessRiskNeutral::flushCache() const { core_.flushCache(); }

} // namesapce QuantLib

#endif
