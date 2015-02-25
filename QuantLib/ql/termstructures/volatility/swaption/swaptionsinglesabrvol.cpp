/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

#include <ql/termstructures/volatility/swaption/swaptionsinglesabrvol.hpp>
#include <ql/termstructures/volatility/sabrsmilesection.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>
#include <ql/experimental/models/kahalesmilesection.hpp>

namespace QuantLib {

    // floating reference date
    SingleSabrSwaptionVolatility::SingleSabrSwaptionVolatility(Natural settlementDays,
                                   const Calendar& cal,
                                   BusinessDayConvention bdc,
                                   const Real alpha,
                                   const Real beta,
                                   const Real rho,
                                   const Real nu,
                                   const DayCounter& dc,
                                   const boost::shared_ptr<SwapIndex>& indexBase,
                                   const Real shift)
    : SwaptionVolatilityStructure(settlementDays, cal, bdc, dc),
        alpha_(alpha), beta_(beta), nu_(nu), rho_(rho), maxSwapTenor_(100*Years), indexBase_(indexBase), shift_(shift)  {
    }

    // fixed reference date
    SingleSabrSwaptionVolatility::SingleSabrSwaptionVolatility(const Date& referenceDate,
                                   const Calendar& cal,
                                   BusinessDayConvention bdc,
                                   const Real alpha,
                                   const Real beta,
                                   const Real rho,
                                   const Real nu,
                                   const DayCounter& dc,
                                   const boost::shared_ptr<SwapIndex>& indexBase,
                                   const Real shift)
    : SwaptionVolatilityStructure(referenceDate, cal, bdc, dc),
        alpha_(alpha), beta_(beta), nu_(nu), rho_(rho), maxSwapTenor_(100*Years), indexBase_(indexBase), shift_(shift) {
    }

    boost::shared_ptr<SmileSection>
    SingleSabrSwaptionVolatility::smileSectionImpl(const Date& d,
                                                 const Period& tenor) const {
        std::vector<Real> params(4);
        params[0] = alpha_;
        params[1] = beta_;
        params[2] = nu_;
        params[3] = rho_;
        Real forward = indexBase_->clone(tenor)->fixing(d);
        boost::shared_ptr<SmileSection> raw(new SabrSmileSection(d,forward,params,dayCounter(),shift_));
        // make it arbitrage free
        //boost::shared_ptr<SmileSection> af(new KahaleSmileSection(raw));
        //return af;
        return raw;
    }

    boost::shared_ptr<SmileSection>
    SingleSabrSwaptionVolatility::smileSectionImpl(Time optionTime,
                                                 Time swapLength) const {
        std::vector<Real> params(4);
        params[0] = alpha_;
        params[1] = beta_;
        params[2] = nu_;
        params[3] = rho_;
        DateHelper hlp(*this,optionTime);
        NewtonSafe newton;
        Date d(static_cast<BigInteger>(newton.solve(
            hlp, 0.1, 365.25 * optionTime +
                          static_cast<Real>(referenceDate().serialNumber()),
            1.0)));
        Period tenor(static_cast<Integer>(Rounding(0).operator()(swapLength*12.0)), Months);
        d = indexBase_->fixingCalendar().adjust(d);
        Real forward = indexBase_->clone(tenor)->fixing(d);
        boost::shared_ptr<SmileSection> raw(new SabrSmileSection(optionTime,forward,params,shift_));
        // make it arbitrage free
        //boost::shared_ptr<SmileSection> af(new KahaleSmileSection(raw));
        //return af;
        return raw;
    }

    Volatility SingleSabrSwaptionVolatility::volatilityImpl(const Date& d,
                                                          const Period& tenor,
                                                          Rate strike) const {
        return smileSectionImpl(d,tenor)->volatility(strike);
    }

    Volatility SingleSabrSwaptionVolatility::volatilityImpl(Time optionTime,
                                                          Time swapLength,
                                                          Rate strike) const {
        return smileSectionImpl(optionTime,swapLength)->volatility(strike);
    }

}
