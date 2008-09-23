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

#include <qlo/indexes/ibor/euribor.hpp>
#include <ql/indexes/ibor/euribor.hpp>

#include <ql/utilities/dataparsers.hpp>

#include <boost/algorithm/string/case_conv.hpp>
using boost::algorithm::to_upper_copy;

namespace QuantLibAddin {

    Euribor::Euribor(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                     const std::string& p,
                     const QuantLib::Handle<QuantLib::YieldTermStructure>& h,
                     bool permanent) : IborIndex(properties, permanent)
    {
        if (to_upper_copy(p)=="ON")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor(0, h));
        else if (to_upper_copy(p)=="1D")
            QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
        else if (to_upper_copy(p)=="TN")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor(1, h));
        else if (to_upper_copy(p)=="SN")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor(2, h));
        else if (to_upper_copy(p)=="SW")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
            QuantLib::Euribor(1*QuantLib::Weeks, h));
        else {
            QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
            pp.normalize();
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::Euribor(pp, h));
        }
    }

    Euribor365::Euribor365(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                           const std::string& p,
                           const QuantLib::Handle<QuantLib::YieldTermStructure>& h,
                           bool permanent) : IborIndex(properties, permanent)
    {
        if (to_upper_copy(p)=="ON")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor365(0, h));
        else if (to_upper_copy(p)=="1D")
            QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
        else if (to_upper_copy(p)=="TN")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor365(1, h));
        else if (to_upper_copy(p)=="SN")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::DailyTenorEuribor365(2, h));
        else if (to_upper_copy(p)=="SW")
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::Euribor365(1*QuantLib::Weeks, h));
        else {
            QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
            pp.normalize();
            libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
                QuantLib::Euribor365(pp, h));
        }
    }

}
