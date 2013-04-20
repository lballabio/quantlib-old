/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

/*! \file fixedinflationcoupon.hpp
 \brief Coupon paying a fixed rate lifted by an zero inflation index ratio
 */

#ifndef quantlib_fixedinfl_coupon_hpp
#define quantlib_fixedinfl_coupon_hpp

#include <ql/cashflows/inflationcoupon.hpp>
#include <ql/indexes/inflationindex.hpp>
#include <ql/time/schedule.hpp>

namespace QuantLib {

    //! %Coupon paying a zero inflation lifted fixed rate coupon
    class FixedInflationCoupon : public InflationCoupon {
    public:
        FixedInflationCoupon(const Date& paymentDate,
                        Real nominal,
                        const Date& startDate,
                        const Date& endDate,
						const Date& baseDate,
                        Natural fixingDays,
						const Real fixedRate,
                        const boost::shared_ptr<ZeroInflationIndex>& index,
                        const Period& observationLag,
                        const DayCounter& dayCounter,
                        Real gearing = 1.0,
                        Spread spread = 0.0,
                        const Date& refPeriodStart = Date(),
                        const Date& refPeriodEnd = Date()
                        );

        //! \name Inspectors
        //@{
        //! index gearing, i.e. multiplicative coefficient for the index
        Real gearing() const { return gearing_; }
        //! spread paid over the fixing of the underlying index
        Spread spread() const { return spread_; }
		//! base date reference fixing 
		Date baseDate() const { return baseDate_; }
		//! fixed rate
		Real fixedRate() const { return fixedRate_; }

        Rate adjustedFixing() const;

        const boost::shared_ptr<ZeroInflationIndex>& zeroInflationIndex() const;

        //@}
        //! \name Visitability
        //@{
        virtual void accept(AcyclicVisitor&);
        //@}

    private:
        boost::shared_ptr<ZeroInflationIndex> zeroInflationIndex_;
    protected:
		bool checkPricerImpl(const boost::shared_ptr<InflationCouponPricer>&) const;
        Real gearing_;
        Spread spread_;
		Real fixedRate_;
		Date baseDate_;
    };

    inline const boost::shared_ptr<ZeroInflationIndex>&
    FixedInflationCoupon::zeroInflationIndex() const {
        return zeroInflationIndex_;
    }

    inline Rate FixedInflationCoupon::adjustedFixing() const {
        return (rate()-spread())/gearing();
    }
    


}

#endif

