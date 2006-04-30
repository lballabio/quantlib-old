
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef quantlib_volatilitymodels_i
#define quantlib_volatilitymodels_i

%include common.i
%include types.i
%include date.i
%include timeseries.i

%{
using QuantLib::ConstantEstimator;
using QuantLib::SimpleLocalEstimator;
%}

class ConstantEstimator {
public:
    ConstantEstimator(Size size);
    TimeSeries<Volatility> 
        calculate(const TimeSeries<Volatility> &volatilitySeries);
};

class SimpleLocalEstimator {
public:
	SimpleLocalEstimator(Real yearFraction);
        TimeSeries<Volatility>
        calculate(const TimeSeries<Real> &quoteSeries);
};

#endif
