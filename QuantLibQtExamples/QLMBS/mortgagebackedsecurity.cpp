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

#include "mortgagebackedsecurity.hpp"
#include <ql/math/array.hpp>
#include <ql/math/randomnumbers/seedgenerator.hpp>
#include <ql/math/randomnumbers/randomsequencegenerator.hpp>
#include <ql/math/randomnumbers/sobolrsg.hpp>
#include <ql/math/randomnumbers/boxmullergaussianrng.hpp>
//#include <ql/instruments/bonds/fixedratebond.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/time/schedule.hpp>

namespace QuantLib {

    MBS::MBS(Natural settlementDays,
                                 Real faceAmount,
                                 const Schedule& schedule,
                                 const std::vector<Rate>& coupons,
                                 const DayCounter& accrualDayCounter,
                                 BusinessDayConvention paymentConvention,
                                 Real redemption,
                                 const Date& issueDate,
                                 const Calendar& paymentCalendar)
     : Bond(settlementDays,
            paymentCalendar==Calendar() ? schedule.calendar() : paymentCalendar,
            issueDate),
      frequency_(schedule.tenor().frequency()),
      dayCounter_(accrualDayCounter) {

        maturityDate_ = schedule.endDate();

        cashflows_ = FixedRateLeg(schedule)
            .withNotionals(faceAmount)
            .withCouponRates(coupons, accrualDayCounter)
            .withPaymentCalendar(calendar_)
            .withPaymentAdjustment(paymentConvention);

        addRedemptionsToCashflows(std::vector<Real>(1, redemption));

        QL_ENSURE(!cashflows().empty(), "bond with no cashflows!");
        QL_ENSURE(redemptions_.size() == 1, "multiple redemptions created");
    }

    MBS::MBS(Natural settlementDays,
                                 const Calendar& calendar,
                                 Real faceAmount,
                                 const Date& startDate,
                                 const Date& maturityDate,
                                 const Period& tenor,
                                 const std::vector<Rate>& coupons,
                                 const DayCounter& accrualDayCounter,
                                 BusinessDayConvention accrualConvention,
                                 BusinessDayConvention paymentConvention,
                                 Real redemption,
                                 const Date& issueDate,
                                 const Date& stubDate,
                                 DateGeneration::Rule rule,
                                 bool endOfMonth,
                                 const Calendar& paymentCalendar)
     : Bond(settlementDays,
            paymentCalendar==Calendar() ? calendar : paymentCalendar,
            issueDate),
      frequency_(tenor.frequency()), dayCounter_(accrualDayCounter) {

        maturityDate_ = maturityDate;

        Date firstDate, nextToLastDate;
        switch (rule) {
          case DateGeneration::Backward:
            firstDate = Date();
            nextToLastDate = stubDate;
            break;
          case DateGeneration::Forward:
            firstDate = stubDate;
            nextToLastDate = Date();
            break;
          case DateGeneration::Zero:
          case DateGeneration::ThirdWednesday:
          case DateGeneration::Twentieth:
          case DateGeneration::TwentiethIMM:
            QL_FAIL("stub date (" << stubDate << ") not allowed with " <<
                    rule << " DateGeneration::Rule");
          default:
            QL_FAIL("unknown DateGeneration::Rule (" << Integer(rule) << ")");
        }

        Schedule schedule(startDate, maturityDate_, tenor,
                          calendar, accrualConvention, accrualConvention,
                          rule, endOfMonth,
                          firstDate, nextToLastDate);

        cashflows_ = FixedRateLeg(schedule)
            .withNotionals(faceAmount)
            .withCouponRates(coupons, accrualDayCounter)
            .withPaymentCalendar(calendar_)
            .withPaymentAdjustment(paymentConvention);

        addRedemptionsToCashflows(std::vector<Real>(1, redemption));

        QL_ENSURE(!cashflows().empty(), "bond with no cashflows!");
        QL_ENSURE(redemptions_.size() == 1, "multiple redemptions created");
    }

    MBS::MBS(Natural settlementDays,
                                 Real faceAmount,
                                 const Schedule& schedule,
                                 const std::vector<InterestRate>& coupons,
                                 BusinessDayConvention paymentConvention,
                                 Real redemption,
                                 const Date& issueDate,
                                 const Calendar& paymentCalendar)
     : Bond(settlementDays,
            paymentCalendar==Calendar() ? schedule.calendar() : paymentCalendar,
            issueDate),
      frequency_(schedule.tenor().frequency()),
      dayCounter_(coupons[0].dayCounter()) {

        maturityDate_ = schedule.endDate();

        cashflows_ = FixedRateLeg(schedule)
            .withNotionals(faceAmount)
            .withCouponRates(coupons)
            .withPaymentCalendar(calendar_)
            .withPaymentAdjustment(paymentConvention);

        addRedemptionsToCashflows(std::vector<Real>(1, redemption));

        QL_ENSURE(!cashflows().empty(), "bond with no cashflows!");
        QL_ENSURE(redemptions_.size() == 1, "multiple redemptions created");
    }





    void MBS::buildTree(double initRate, double financeRate, int N)
    {
        double u = 1.1; //FIXME
        double d = 1/u;
        double p = (exp(initRate * T) - d)/(u - d);
        double deviate = 0.0;
        //long seed = 0;
        double refRate = financeRate;
        long* idum = 0;
        double pay = faceValue;
        double faceAmount = 0.0;
        double interest = 0.0;
        double schedulePrincipal = 0.0;
        double prepaidPrincipal = 0.0;
        double CPR = 0.0;
        double balance = faceValue_;
        double sum = 0.0;
        double totalsum = 0.0;
        double SMM = 0.0;
        //TNT::Array1D<double> CF(SIZE_X);   // cash_flow
        Array<double>  CF(SIZE_X);   // cash_flow
        std::vector<double> disc(0.0);

        //srand(unsigned(time(0)));
        //seed = (long) rand() % 100;
        //idum = &seed;
        //Utility util;
        //BigInteger seed = SeedGenerator::instance().get();
        SobolRsg sobolGen(1);
        Real currSobolNum;
        MoroInverseCumulativeNormal invGauss;
        for (Size i=0; i<N; ++i) {
            currSobolNum = (sobolGen.nextSequence().value)[0];
            Real nrand = invGauss(currSobolNum);
        }

        // build binomial tree for rates
        for (int i = 0; i <= N; i++)
        {
            for (int j = 0; j <= i; j++)
            {
                spotRate_[i][j] = initRate*pow(u,j)*pow(d,i-j);
                discountRate_[i][j] = spotRate[i][j] + OAS;
            }
        }


        faceAmount = faceValue_;
        int k = 0;
        long int M = 10000;
        //int cnt = 0;
        double r = 0.0;
        int j = 0;
        int i = 0;

        for (k = 0; k < M; k++) // M: number of simulation paths
        {
            sum = 0.0;
            balance = faceValue_;
            refRate = financeRate;
            j = 0;
            disc.clear();
            disc.empty();
            disc.push_back(discountRate_[0][0]);

            for (i = 0; i < N; i++) // N: number of tree paths per simulation
            {
                    balance = balance - (schedulePrincipal + prepaidPrincipal);
                    deviate = util.gasdev(idum);

                    if (deviate > 0)
                    {
                        j++;
                        refRate = refRate*u;
                    }
                    else
                    {
                        j--;
                        if (j < 0)
                            j = 0;
                        refRate = refRate*d;
                    }
                    disc.push_back(discountRate[i+1][j]);
                    interest = coupon*balance;
                    pay = calcPayment(balance,WAM-i);
                    schedulePrincipal = pay - interest;

                    if (balance >= schedulePrincipal)
                    {
                        CPR = calcCPR(refRate);
                        SMM = calcSMM(CPR);
                        prepaidPrincipal = SMM*(balance - schedulePrincipal);

                        if (i != N-1)
                            CF[i] = interest + schedulePrincipal + prepaidPrincipal;
                        else
                            CF[i] = interest + balance;

                        r = computeZeroRates(i,disc);
                        sum = sum + CF[i]/(pow(1+r,i+1));

                    }
                    else
                        goto x;

            }
            x:
            totalsum = totalsum + sum;
        }
        double ave = (totalsum/M);
        std::cout << "MBS price = " << ave << endl;
    }


    double MBS::calcCPR(double rate)
    {
        double CPR = 0.0;
        double value = WAC_ - rate;

        /*
        if (value <= 0)
            CPR = 0.05;
        else if ((value <= 0.005) && (value > 0))
            CPR = 0.10;
        else if ((value <= 0.01) && (value > 0.005))
            CPR = 0.20;
        else if ((value <= 0.0125) && (value > 0.01))
            CPR = 0.30;
        else if ((value <= 0.02) && (value > 0.0125))
            CPR = 0.40;
        else if ((value <= 0.025) && (value > 0.02))
            CPR = 0.50;
        else if ((value <= 0.03) && (value > 0.025))
            CPR = 0.60;
        else
            CPR = 0.70;
        */

        CPR = 100.0 * (1.0 - pow((1.0 - (value/100.0)), 12.0));
        return CPR;
    }



    double MBS::calcPayment(double fv, double T) {
        return (fv * coupon_)/(1.0-pow(1.0/(1.0 + coupon_), T));
    }

    void MBS::calcPrice(double initRate, double financeRate, int N, long int M){

    }



    double MBS::calcSMM(double CPR) {
         return (1.0 - pow((1.0 - CPR), 1.0/12.0));
    }

    double MBS::computeZeroRates(int cnt, std::vector<double> rate)
    {

        double value = WAC_+1;
        for (int j = 1; j <= cnt; ++j)
            value = value*(1.0 + rate[j]);

        if (cnt == 0)
            value = WAC_;
        else
           value = pow(value, 1.0/(cnt+1.0)) - 1.0;

        return value;
    }

}

