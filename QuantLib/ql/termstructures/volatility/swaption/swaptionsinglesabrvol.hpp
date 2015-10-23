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

/*! \file swaptionsinglesabrvol.hpp
    \brief swaption volatility from a single sabr model
*/

#ifndef quantlib_swaption_singlesabr_volatility_hpp
#define quantlib_swaption_singlesabr_volatility_hpp

#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>
#include <ql/time/period.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLib {

    class Quote;

    //! swaption volatility from a single sabr model, i.e. the sabr parameters are constant
    class SingleSabrSwaptionVolatility : public SwaptionVolatilityStructure {
      public:
        //! floating reference date
        SingleSabrSwaptionVolatility(Natural settlementDays,
                                   const Calendar& cal,
                                   BusinessDayConvention bdc,
                                   const Real alpha,
                                   const Real beta,
                                   const Real rho,
                                   const Real nu,
                                   const DayCounter& dc,
                                   const boost::shared_ptr<SwapIndex>& indexBase,
                                   const Real shift = 0.0 );
        //! fixed reference date
        SingleSabrSwaptionVolatility(const Date& referenceDate,
                                   const Calendar& cal,
                                   BusinessDayConvention bdc,
                                   const Real alpha,
                                   const Real beta,
                                   const Real rho,
                                   const Real nu,
                                   const DayCounter& dc,
                                   const boost::shared_ptr<SwapIndex>& indexBase,
                                   const Real shift = 0.0 );
        //@{
        Date maxDate() const;
        //@}
        //! \name VolatilityTermStructure interface
        //@{
        Real minStrike() const;
        Real maxStrike() const;
        //@}
        //! \name SwaptionVolatilityStructure interface
        //@{
        const Period& maxSwapTenor() const;
        //@}
      protected:
        boost::shared_ptr<SmileSection> smileSectionImpl(const Date&,
                                                         const Period&) const;
        boost::shared_ptr<SmileSection> smileSectionImpl(Time,
                                                         Time) const;
        Volatility volatilityImpl(const Date&,
                                  const Period&,
                                  Rate) const;
        Volatility volatilityImpl(Time,
                                  Time,
                                  Rate) const;
      private:
        const Real alpha_,beta_,nu_,rho_;
        const Period maxSwapTenor_;
        const boost::shared_ptr<SwapIndex> indexBase_;
        const Real shift_;

        class DateHelper;
        friend class DateHelper;
        class DateHelper {
         public:
            DateHelper(const TermStructure& ts, const Time t) : ts_(ts), t_(t) {}
            Real operator()(Real date) const {
                Date d1(static_cast<BigInteger>(date));
                Date d2(static_cast<BigInteger>(date)+1);
                Real t1 = ts_.timeFromReference(d1)-t_;
                Real t2 = ts_.timeFromReference(d2)-t_;
                Real h = date - static_cast<BigInteger>(date);
                return h*t2+(1.0-h)*t1;
            }
            Real derivative(Real date) const {
                // use fwd difference to avoid dates before reference date
                return (operator()(date+1E-6)-operator()(date))*1E6; 
            }
            const TermStructure& ts_;
            const Time t_;
        };
            
    };


    // inline definitions

    inline Date SingleSabrSwaptionVolatility::maxDate() const {
        return Date::maxDate();
    }

    inline Real SingleSabrSwaptionVolatility::minStrike() const {
        return -shift_;
    }

    inline Real SingleSabrSwaptionVolatility::maxStrike() const {
        return QL_MAX_REAL;
    }

    inline const Period& SingleSabrSwaptionVolatility::maxSwapTenor() const {
        return maxSwapTenor_;

    }

}

#endif
