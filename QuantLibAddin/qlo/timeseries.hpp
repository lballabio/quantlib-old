
/*
 Copyright (C) 2007 Ferdinando Ametrano

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

#ifndef qla_timeseries_hpp
#define qla_timeseries_hpp

#include <oh/libraryobject.hpp>
#include <ql/types.hpp>
#include <map>

namespace QuantLib {
    class Date;

    template<class T, class Container>
    class TimeSeries;

    typedef TimeSeries<QuantLib::Real, std::map<QuantLib::Date, QuantLib::Real> > TimeSeriesDef;
}

namespace QuantLibAddin {

    class TimeSeriesDef : public ObjectHandler::LibraryObject<QuantLib::TimeSeriesDef> {
      public:
        TimeSeriesDef(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Real>& values,
            bool permanent);
        // Workaround for VC7.  Addin function qlTimeSeriesValue() requires an address
        // of QuantLib::TimeSeries::operator[] but VC7 doesn't support the address of
        // an operator so here we wrap [] and let VC7 use the adddress of the wrappper.
        QuantLib::Real subscriptWrapper(const QuantLib::Date& d);
    };
}

#endif
