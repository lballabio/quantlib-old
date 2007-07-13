
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

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

#ifndef quantlib_timeseries_i
#define quantlib_timeseries_i

%include common.i
%include types.i
%include date.i
%include vectors.i

%{
using QuantLib::TimeSeries;
using QuantLib::IntervalPrice;
%}

template <class T, class Container = std::map<Date, T> >
class TimeSeries {
    #if defined (SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__) size;
    #endif
  public:
    TimeSeries();
    %extend {
        TimeSeries(const std::vector<Date>& d, const std::vector<T>& v) {
            return new TimeSeries<T>(d.begin(), d.end(), v.begin());
        }
    }
    std::vector<Date> dates();
    std::vector<T> values();
    Size size();
    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY) || defined(SWIGR)
        T __getitem__(const Date& d) {
            return (*self)[d];
        }
        void __setitem__(const Date& d, const T& value) {
            (*self)[d] = value;
        }
        #endif
    }
};

%template(RealTimeSeries) TimeSeries<Real>;
%template(IntervalPriceTimeSeries) TimeSeries<IntervalPrice>;
%template(IntervalPriceVector) std::vector<IntervalPrice>;

class IntervalPrice {
  public:
    enum Type {Open, Close, High, Low};
    IntervalPrice(Real, Real, Real, Real);
    void setValue(Real, IntervalPrice::Type);
    void setValues(Real, Real, Real, Real);
    Real value(IntervalPrice::Type t);
    Real open();
    Real close();
    Real high();
    Real low();
    static TimeSeries<IntervalPrice> makeSeries(const std::vector<Date>& d,
                                                const std::vector<Real>& open,
                                                const std::vector<Real>& close,
                                                const std::vector<Real>& high,
                                                const std::vector<Real>& low);
    static std::vector<Real> extractValues(TimeSeries<IntervalPrice>,
                                           IntervalPrice::Type t);
    static TimeSeries<Real> extractComponent(TimeSeries<IntervalPrice>,
                                             IntervalPrice::Type t);
};



typedef RealTimeSeries VolatilityTimeSeries;


#if defined(SWIGR)
%Rruntime %{
setMethod('as.data.frame', '_p_TimeSeriesTdouble_std__mapTDate_double_t_t',
function(x,row.names,optional)
data.frame("date"=as(x$dates(), "character"),
"values"=as(x$values(), "numeric")))

setMethod("print", '_p_TimeSeriesTdouble_std_mapTDate_double_t_t',
function(x) print(as.data.frame(x)))

setMethod('as.data.frame', '_p_TimeSeriesTIntervalPrice_std_mapTDate_IntervalPrice_t_t',
function(x,row.names,optional)
data.frame("date"=as(x$dates(), "character"),
"open"=as(IntervalPrice_extractValues(x, "Open"), "numeric"),
"close"=as(IntervalPrice_extractValues(x, "Close"), "numeric"),
"high"=as(IntervalPrice_extractValues(x, "High"), "numeric"),
"low"=as(IntervalPrice_extractValues(x, "Low"), "numeric")))

setMethod("print", '_p_TimeSeriesTIntervalPrice_std_mapTDate_IntervalPrice_t_t',
function(x) print(as.data.frame(x)))
%}

#endif
#endif
