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

#include <qlo/indexes/ibor/libor.hpp>

#include <ql/indexes/ibor/eurlibor.hpp>
#include <ql/indexes/ibor/usdlibor.hpp>
#include <ql/indexes/ibor/gbplibor.hpp>
#include <ql/indexes/ibor/sonia.hpp>
#include <ql/indexes/ibor/chflibor.hpp>
#include <ql/indexes/ibor/jpylibor.hpp>

#include <ql/utilities/dataparsers.hpp>

#include <boost/algorithm/string/case_conv.hpp>
using boost::algorithm::to_upper_copy;
using ObjectHandler::ValueObject;
using boost::shared_ptr;

namespace QuantLibAddin {

    Libor::Libor(const shared_ptr<ValueObject>& properties,
                 const QuantLib::Currency& currency,
                 const std::string& p,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& h,
                 bool permanent) : IborIndex(properties, permanent)
    {
        switch (currency.numericCode()) {
          case 978: // EUR
            if (to_upper_copy(p)=="ON")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorEURLibor(0, h));
            else if (to_upper_copy(p)=="1D")
                QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
            else if (to_upper_copy(p)=="TN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorEURLibor(1, h));
            else if (to_upper_copy(p)=="SN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorEURLibor(2, h));
            else if (to_upper_copy(p)=="SW")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::EURLibor(1*QuantLib::Weeks, h));
            else {
                QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
                pp.normalize();
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::EURLibor(pp, h));
            }
            break;
          case 840: // USD
            if (to_upper_copy(p)=="ON")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorUSDLibor(0, h));
            else if (to_upper_copy(p)=="1D")
                QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
            else if (to_upper_copy(p)=="TN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorUSDLibor(1, h));
            else if (to_upper_copy(p)=="SN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorUSDLibor(2, h));
            else if (to_upper_copy(p)=="SW")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::USDLibor(1*QuantLib::Weeks, h));
            else {
                QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
                pp.normalize();
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::USDLibor(pp, h));
            }
            break;
          case 826: // GBP
            if (to_upper_copy(p)=="ON")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorGBPLibor(0, h));
            else if (to_upper_copy(p)=="1D")
                QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
            else if (to_upper_copy(p)=="TN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorGBPLibor(1, h));
            else if (to_upper_copy(p)=="SN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorGBPLibor(2, h));
            else if (to_upper_copy(p)=="SW")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::GBPLibor(1*QuantLib::Weeks, h));
            else {
                QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
                pp.normalize();
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::GBPLibor(pp, h));
            }
            break;
          case 756: // CHF
            if (to_upper_copy(p)=="ON")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorCHFLibor(0, h));
            else if (to_upper_copy(p)=="1D")
                QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
            else if (to_upper_copy(p)=="TN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorCHFLibor(1, h));
            else if (to_upper_copy(p)=="SN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorCHFLibor(2, h));
            else if (to_upper_copy(p)=="SW")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::CHFLibor(1*QuantLib::Weeks, h));
            else {
                QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
                pp.normalize();
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::CHFLibor(pp, h));
            }
            break;
          case 392: // JPY
            if (to_upper_copy(p)=="ON")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorJPYLibor(0, h));
            else if (to_upper_copy(p)=="1D")
                QL_FAIL("1D is ambigous: please specify ON, TN, or SN");
            else if (to_upper_copy(p)=="TN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorJPYLibor(1, h));
            else if (to_upper_copy(p)=="SN")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::DailyTenorJPYLibor(2, h));
            else if (to_upper_copy(p)=="SW")
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::JPYLibor(1*QuantLib::Weeks, h));
            else {
                QuantLib::Period pp = QuantLib::PeriodParser::parse(p);
                pp.normalize();
                libraryObject_ = shared_ptr<QuantLib::IborIndex>(new
                    QuantLib::JPYLibor(pp, h));
            }
            break;
          default:
              QL_FAIL("Unhandled currency " << currency);
        }
    }

    Sonia::Sonia(const shared_ptr<ValueObject>& properties,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& h,
                 bool permanent)
    : OvernightIndex(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Sonia>(new
            QuantLib::Sonia(h));
    }

}
