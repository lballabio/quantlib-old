/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Paul Cao

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

 /*
  Black Variance Surface that is plotted in moneyness
  so that it can extrapolates volatility at a new price with the 
  original volatility surface. e.g.,

  BlackVarianceSurface volatilitySurface(...)
  volatilitySurface.enableExtrapolation();
  volatilitySurface.setCurrentPrice(newPrice);
  volatilitySurface.blackVol(expiration, strikePrice, true);
 */

#ifndef BLACKVARIANCEMONEYNESSSURFACE_HPP_
#define BLACKVARIANCEMONEYNESSSURFACE_HPP_
#include <vector>
#include <ql/quantlib.hpp>
#include <cstdlib>
#include <ql/termstructures/volatility/equityfx/blackvariancesurface.hpp>
using namespace QuantLib;
using namespace std;

class BlackVarianceMoneynessSurface : public BlackVarianceSurface {
public:
	BlackVarianceMoneynessSurface(const Date& referenceDate, const Calendar& cal,
		     	 	 	 	 const std::vector<Date>& dates,
		     	 	 	 	 const std::vector<Real>& strikes,
		     	 	 	 	 const Matrix& blackVolMatrix,
		     	 	 	 	 const DayCounter& dayCounter,
		     	 	 	 	 const double currentPrice,
		     	 	 	 	 Extrapolation lowerExtrapolation = InterpolatorDefaultExtrapolation,
		     	 	 	 	 Extrapolation upperExtrapolation = InterpolatorDefaultExtrapolation);

	Volatility blackVol(const Date& maturity, Real strike, bool extrapolate = false);
	void setCurrentPrice(double currentPrice);

private:
	double currentPrice_;
	const std::vector<Real> convertToMoneyness(const std::vector<Real>& strikes, const double currentPrice);
};

#endif /* BLACKVARIANCEMONEYNESSSURFACE_HPP_ */
