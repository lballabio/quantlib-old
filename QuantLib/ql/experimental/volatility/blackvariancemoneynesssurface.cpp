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

#include <ql/experimental/volatility/blackvariancemoneynesssurface.hpp>
#include <ql/math/matrix.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>
#include <vector>
#include <cstdlib>
#include <boost/assign/std/vector.hpp>
using namespace QuantLib;
using namespace boost::assign;
using namespace std;

BlackVarianceMoneynessSurface::BlackVarianceMoneynessSurface(const Date& referenceDate, const Calendar& cal,
						 const std::vector<Date>& dates,
						 const std::vector<Real>& strikes,
						 const Matrix& blackVolMatrix,
						 const DayCounter& dayCounter,
						 const double currentPrice,
						 BlackVarianceSurface::Extrapolation lowerExtrapolation,
						 BlackVarianceSurface::Extrapolation upperExtrapolation)
: BlackVarianceSurface(referenceDate, cal, dates, convertToMoneyness(strikes, currentPrice), blackVolMatrix, dayCounter,
								lowerExtrapolation, upperExtrapolation),
  currentPrice_(currentPrice)
{
}

void BlackVarianceMoneynessSurface::setCurrentPrice(double currentPrice) {
	currentPrice_ = currentPrice;
}

Volatility BlackVarianceMoneynessSurface::blackVol(const Date& maturity, Real strike, bool extrapolate) {
	strike = log(strike /currentPrice_); // convert the strike price to moneyness
	return BlackVarianceSurface::blackVol(maturity, strike, extrapolate);
}

const std::vector<Real> BlackVarianceMoneynessSurface::convertToMoneyness(const std::vector<Real>& strikes, const double currentPrice) {
	std::vector<double> moneynessStrikes;

	// convert strikes to moneyness
	for (std::vector<Real>::const_iterator it = strikes.begin(); it != strikes.end(); ++it) {
		double moneyness = log(*it / currentPrice);
		moneynessStrikes += moneyness;
	}
	return moneynessStrikes;
}

