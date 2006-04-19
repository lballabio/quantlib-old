
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

#ifndef quantlib_timeseries_i
#define quantlib_timeseries_i

%include common.i
%include types.i
%include date.i

%{
using QuantLib::TimeSeries;
%}

template <class T>
class TimeSeries {
    #if defined (SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__) size;
    #endif
  public:
    TimeSeries();
    TimeSeries(const std::vector<Date>&, const std::vector<Real>&);
    std::vector<Date> dates();
    std::vector<T> values();
    Size size();
    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGR)
        Real __getitem__(const Date& d) {
            return (*self)[d];
        }
        void __setitem__(const Date& d, Real value) {
            (*self)[d] = value;
        }
        #endif
    }
};

%template(RealTimeSeries) TimeSeries<Real>;
typedef RealTimeSeries VolatilityTimeSeries;

#if defined(SWIGR)
%Rruntime %{
setMethod('as.data.frame', '_p_TimeSeriesTdouble_t',
function(x,row.names,optional)
data.frame("date"=as(x$dates(), "character"),
"values"=as(x$values(), "numeric")))

setMethod("print", '_p_TimeSeriesTdouble_t',
function(x) print(as.data.frame(x)))
%}
#endif
#endif
