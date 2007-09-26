
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/timeseries.hpp>
#include <ql/timeseries.hpp>
#include <ql/errors.hpp>

namespace QuantLibAddin {

   TimeSeries::TimeSeries(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Date>& dates,
        const std::vector<QuantLib::Real>& values,
        bool permanent)
   : ObjectHandler::LibraryObject<QuantLib::TimeSeries<QuantLib::Real> >(properties, permanent)
   {
        QL_REQUIRE(values.size()==dates.size(),
                  "unmatched size between dates (" << dates.size() <<
                  ") and values(" << values.size() << ")");

        std::vector<QuantLib::Date> d;
        std::vector<QuantLib::Real> v;
        for (QuantLib::Size i=0; i<values.size(); ++i) {
            // skip null fixings
            if (values[i]!=0.0 && values[i]!=QuantLib::Null<QuantLib::Real>()) {
                QL_REQUIRE(values[i]>0.0,
                           "non positive fixing (" << values[i] <<
                           ") at date " << dates[i] << " not allowed");
                d.push_back(dates[i]);
                v.push_back(values[i]);
            }
        }

        libraryObject_ =
            boost::shared_ptr<QuantLib::TimeSeries<QuantLib::Real> >(new
                QuantLib::TimeSeries<QuantLib::Real>(d.begin(),
                                                     d.end(),
                                                     v.begin()));
    }
}
