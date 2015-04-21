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

#include <qlo/creditdefaultswap.hpp>

#include <ql/instruments/creditdefaultswap.hpp>

namespace QuantLibAddin {

    CreditDefaultSwap::CreditDefaultSwap(
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
        bool permanent)
    : Instrument(properties, permanent) {
		// dirty way to decide if this is constructed through a run only version
		if(upfrontDate == QuantLib::Null<QuantLib::Date>() && upfront == 0.) {
            libraryObject_ = boost::shared_ptr<QuantLib::CreditDefaultSwap>(
                new QuantLib::CreditDefaultSwap(
                    side,
                    notional,
                    spread,
                    *schedule,
                    paymentConvention,
                    dayCounter,
                    settlesAccrual,
                    paysAtDefaultTime,
                    protectionStart,
                    boost::shared_ptr<QuantLib::Claim>()));
			}else{
                libraryObject_ = boost::shared_ptr<QuantLib::CreditDefaultSwap>(
                    new QuantLib::CreditDefaultSwap(
                        side,
                        notional,
                        upfront,
                        spread,
                        *schedule,
                        paymentConvention,
                        dayCounter,
                        settlesAccrual,
                        paysAtDefaultTime,
                        protectionStart,
                        upfrontDate,
                        boost::shared_ptr<QuantLib::Claim>()));
            }
    }
    
}
