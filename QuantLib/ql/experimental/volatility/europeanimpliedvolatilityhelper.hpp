/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Klaus Spanderen

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

/*! \file europeanimpliedvolatility.hpp
    \brief Utilities for implied-volatility calculation of European options
*/

#ifndef quantlib_european_implied_volatility_helper_hpp
#define quantlib_european_implied_volatility_helper_hpp

#include <ql/types.hpp>
#include <ql/option.hpp>

namespace QuantLib {
	//! Optimized implied volatility calculator for vanilla European options

	/*! References:
		Jäckel, P., 2006. By Implication,
		http://www.pjaeckel.webspace.virginmedia.com/ByImplication.pdf

		Jäckel, P., 2014. Let's be rational,
		http://www.pjaeckel.webspace.virginmedia.com/LetsBeRational.pdf

		This implementation includes code taken from
		Peter Jaeckel's reference implementation,
		http://www.pjaeckel.webspace.virginmedia.com/LetsBeRational.7z
	 */

	class EuropeanImpliedVolatilityHelper {
	  public:
		EuropeanImpliedVolatilityHelper(Option::Type type,
										Real forward, Real strike,
										DiscountFactor discount, Time maturity);

		Size iterations() const;

		Real blackPrice(Volatility vol) const;
		Volatility calculate(Real price,
							 Real tol=1e-12, Size maxEvaluations=10) const;

		Real normalizedPrice(Real price) const;
		Real normalizedPrice(Real x, Volatility stdDev, Real theta) const;

	  private:
		const Real theta_, x_;
		const Real forward_, strike_;
		const DiscountFactor discount_;
		const Time maturity_;
		mutable Size iterations_;

		static Real N(Real x);
		static Real Y(Real x);
		static Real invN(Real x);
		static Real erfcx(Real x);
		static Volatility stdDevLow (Real x, Real beta, Real b_c);
		static Volatility stdDevHigh(Real x, Real beta,
									  Real theta, Real b_c);
	};
}

#endif
