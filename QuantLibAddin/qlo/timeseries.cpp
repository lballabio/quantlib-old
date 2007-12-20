/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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

    using QuantLib::Real;
    using QuantLib::Date;
    using QuantLib::Size;
    using std::vector;
    using boost::shared_ptr;
    using ObjectHandler::LibraryObject;
    using ObjectHandler::ValueObject;

   TimeSeriesDef::TimeSeriesDef(const shared_ptr<ValueObject>& prop,
                                const vector<Date>& dates,
                                const vector<Real>& values,
                                bool perm)
   : LibraryObject<QuantLib::TimeSeriesDef>(prop, perm)
   {
        QL_REQUIRE(values.size()==dates.size(),
                   "unmatched size between dates (" << dates.size() <<
                   ") and values(" << values.size() << ")");

        vector<Date> d;
        vector<Real> v;
        for (Size i=0; i<values.size(); ++i) {
            // skip null fixings
            if (values[i]!=0.0 && values[i]!=QuantLib::Null<Real>()) {
                QL_REQUIRE(values[i]>0.0,
                           "non positive fixing (" << values[i] <<
                           ") at date " << dates[i] << " not allowed");
                d.push_back(dates[i]);
                v.push_back(values[i]);
            }
        }

        libraryObject_ = shared_ptr<QuantLib::TimeSeriesDef>(new
            QuantLib::TimeSeriesDef(d.begin(),
                                    d.end(),
                                    v.begin()));
    }

    QuantLib::Real TimeSeriesDef::subscriptWrapper(const QuantLib::Date& d) {
        return libraryObject_->operator[](d);
    }

}
