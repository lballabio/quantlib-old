/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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
#include <qlo/sequencestatistics.hpp>
#include <ql/math/statistics/sequencestatistics.hpp>

namespace QuantLibAddin {

    SequenceStatistics::SequenceStatistics(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Size dimension,
            const QuantLib::Matrix& values, 
            const std::vector<QuantLib::Real>& w,
            bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::SequenceStatistics>(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SequenceStatistics>(new
            QuantLib::SequenceStatistics(dimension));

        QL_REQUIRE(w.empty() || values.rows()==w.size(),
                   "Mismatch between number of samples (" <<
                   values.rows() << ") and number of weights (" <<
                   w.size() << ")");

        if (values.rows()>0) {
            if (!w.empty()) {
                for (QuantLib::Size i=0; i<values.rows(); ++i)
                    libraryObject_->add(values.row_begin(i),
                                        values.row_end(i),
                                        w[i]);
            } else {
                for (QuantLib::Size i=0; i<values.rows(); ++i)
                    libraryObject_->add(values.row_begin(i),
                                        values.row_end(i));
            }
        }

    }


    SequenceStatistics::SequenceStatistics(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Size dimension,
            bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::SequenceStatistics>(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::SequenceStatistics>(new
            QuantLib::SequenceStatistics(dimension));
    }

}

