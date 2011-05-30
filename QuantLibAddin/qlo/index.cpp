/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/index.hpp>

#include <ql/timeseries.hpp>
#include <ql/index.hpp>

namespace QuantLibAddin {

    void Index::addFixings(const std::vector<QuantLib::Date>& dates,
                           const std::vector<QuantLib::Real>& values,
                           bool forceOverwrite, bool updateValuObject) {
        QL_REQUIRE(dates.size()==values.size(),
                   "size mismatch between dates (" << dates.size() <<
                   ") and values (" << values.size() << ")");
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
        libraryObject_->addFixings(d.begin(), d.end(),
                                   v.begin(), forceOverwrite);

        if (updateValuObject) {
            std::vector<long> fixingDates;
            std::vector<QuantLib::Real> fixingRates;
            const QuantLib::TimeSeries<QuantLib::Real>& history = libraryObject_->timeSeries();
            for (QuantLib::TimeSeries<QuantLib::Real>::const_iterator hi = history.begin();
                 hi != history.end(); ++hi) {
                fixingDates.push_back(hi->first.serialNumber());
                fixingRates.push_back(hi->second);
            }
            boost::shared_ptr<ObjectHandler::ValueObject> inst_properties = properties();
            inst_properties->setProperty("IndexFixingDates", fixingDates);
            inst_properties->setProperty("IndexFixingRates", fixingRates);
        }
    }

}
