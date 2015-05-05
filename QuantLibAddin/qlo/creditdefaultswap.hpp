/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2010 Roland Lichters
 Copyright (C) 2014 Jose Aparicio

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

#ifndef qla_credit_default_swap_hpp
#define qla_credit_default_swap_hpp

#include <qlo/baseinstruments.hpp>

#include <ql/types.hpp>
#include <ql/default.hpp>
#include <ql/time/schedule.hpp>
#include <ql/time/businessdayconvention.hpp>
#include <ql/time/daycounter.hpp>

namespace QuantLib {
    class Date;
}

namespace QuantLibAddin {

    class CreditDefaultSwap : public Instrument {
    public:
        CreditDefaultSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Protection::Side side,
            QuantLib::Real notional,
            QuantLib::Rate upfront,
            QuantLib::Rate spread,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::BusinessDayConvention paymentConvention,
            const QuantLib::DayCounter& dayCounter,
            bool settlesAccrual,
            bool paysAtDefaultTime,
            const QuantLib::Date& protectionStart,
            const QuantLib::Date& upfrontDate,
            bool permanent);
    };

}

#endif
