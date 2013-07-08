/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Minoru Mizutani

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

/*! \file mortgagebackedsecurity.hpp
    \brief mortgage backed securities (MBS)
*/

#ifndef MBS_H
#define MBS_H

#include <ql/instruments/bond.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/interestrate.hpp>

#define SIZE_X 100
#define SIZE_Y 100

namespace QuantLib {

    class Schedule;

    //! fixed-rate bond
    /*! \ingroup instruments
    */
    class MBS : public Bond {
      public:

        MBS(double principal, double coupon, double WAC, double WAM, double OAS) :
            faceValue_(principal), coupon_(coupon), WAC_(WAC), WAM_(WAM), OAS_(OAS),
            T_(WAM) {}

        //! simple annual compounding coupon rates
        MBS(Natural settlementDays,
            Real faceAmount,
            const Schedule& schedule,
            const std::vector<Rate>& coupons,
            const DayCounter& accrualDayCounter,
            BusinessDayConvention paymentConvention = Following,
            Real redemption = 100.0,
            const Date& issueDate = Date(),
            const Calendar& paymentCalendar = Calendar());
        /*! simple annual compounding coupon rates
            with internal schedule calculation */
        MBS(Natural settlementDays,
            const Calendar& couponCalendar,
            Real faceAmount,
            const Date& startDate,
            const Date& maturityDate,
            const Period& tenor,
            const std::vector<Rate>& coupons,
            const DayCounter& accrualDayCounter,
            BusinessDayConvention accrualConvention = Following,
            BusinessDayConvention paymentConvention = Following,
            Real redemption = 100.0,
            const Date& issueDate = Date(),
            const Date& stubDate = Date(),
            DateGeneration::Rule rule = DateGeneration::Backward,
            bool endOfMonth = false,
            const Calendar& paymentCalendar = Calendar());
        //! generic compounding and frequency InterestRate coupons
        MBS(Natural settlementDays,
            Real faceAmount,
            const Schedule& schedule,
            const std::vector<InterestRate>& coupons,
            BusinessDayConvention paymentConvention = Following,
            Real redemption = 100.0,
            const Date& issueDate = Date(),
            const Calendar& paymentCalendar = Calendar());
        Frequency frequency() const { return frequency_; }
        const DayCounter& dayCounter() const { return dayCounter_; }

        double calcPayment(double principal, double T);	// compute payment amount
        void calcPrice(double initRate, double financeRate, int N, long int M);
        double calcCPR(double rate);
        void buildTree(double initRate, double financeRate, int N);
        double computeZeroRates(int cnt, std::vector<double> rate);
        double calcSMM(double x);

        double getPrice() const;
        double getStdDev() const;
        double getStdErr() const;
        double getMaturity() const;
        double getWAM() const;
        double getWAC() const;
        double getOAS() const;

      protected:
        Frequency frequency_;
        DayCounter dayCounter_;

        double OAS_;               // option adjusted spread
        double faceValue_;	      // principal amount
        double coupon_;            // coupon rate
        double WAM_;				  // weighted average maturity
        double WAC_;		          // weighted average coupon
        std::vector<double> zeroRates_; // store discount zero coupon rates
        double T_;		          // maturity of MBS
        double mbsPrice_;	      // price
        double stdDev_;		      // standard deviation
        double stdErr_;		      // standard error
        std::vector<std::vector<double> > spotRate_;
        std::vector<std::vector<double> > discountRate_;

    };

    inline double MBS::getPrice() const {
         return mbsPrice_;
    }

    inline double MBS::getStdDev() const {
         return stdDev_;
    }

    inline double MBS::getStdErr() const {
         return stdErr_;
    }

    inline double MBS::getMaturity() const {
        return T_;
    }

    inline double MBS::getWAM() const {
        return WAM_;
    }

    inline double MBS::getWAC() const {
        return WAC_;
    }

    inline double MBS::getOAS() const {
        return OAS_;
    }

}

#endif // MBS_H
